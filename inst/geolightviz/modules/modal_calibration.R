# Modal for twilight calibration display
modal_calibration_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "modal-calibration-container",
      shiny::h4("Twilight Calibration"),
      shiny::p(
        "The histogram shows twilight errors for the selected stationary periods and the line shows the proposed calibration. When available, the active calibration is overlaid in blue. Changes are only applied when you use the proposed calibration."
      ),
      shiny::div(
        class = "modal-input-group",
        shiny::tags$label("Calibration staps:"),
        shiny::selectizeInput(
          ns("calib_stap_ids"),
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select stationary periods"),
          width = "100%"
        )
      ),
      plotly::plotlyOutput(
        ns("calibration_plot"),
        width = "100%",
        height = "450px"
      ),
      shiny::p(
        "The calibration consists of fitting a kernel density to the twilight errors. You can control the smoothness of the fit using the ",
        shiny::tags$code("twl_calib_adjust"),
        " argument of ",
        shiny::tags$a(
          shiny::tags$code("geolight_map()"),
          href = "https://geopressure.org/GeoPressureR/reference/geolight_map.html#arg-twl-calib-adjust",
          target = "_blank"
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::div(
            class = "modal-input-group",
            shiny::tags$label(
              "twl_calib_adjust:",
              title = "Adjustment parameter for density()"
            ),
            shiny::numericInput(
              ns("twl_calib_adjust"),
              label = NULL,
              value = 1.4,
              step = 0.1,
              width = "70px"
            )
          )
        ),
        shiny::column(
          6,
          shiny::actionButton(
            ns("use_calibration"),
            "Use the proposed calibration",
            class = "btn-primary",
            icon = shiny::icon("check"),
            style = "width: 100%;"
          )
        )
      ),
      shiny::hr(),
      shiny::h4("Likelihood Aggregation"),
      shiny::p(
        "The second parameter controlling the likelihood map is how twilight likelihood maps are aggregated over stationary periods. Since twilight errors are typically correlated over time, we use a log-linear pooling function. See more information in the ",
        shiny::tags$a(
          "Probability Aggregation chapter of the GeoPressureManual",
          href = "https://geopressure.org/GeoPressureManual/probability-aggregation.html#log-linear-pooling-w-lognn",
          target = "_blank"
        ),
        " and ",
        shiny::tags$a(
          shiny::tags$code("geolight_map()"),
          href = "https://geopressure.org/GeoPressureR/reference/geolight_map.html#arg-twl-llp",
          target = "_blank"
        ),
        "."
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::div(
            class = "modal-input-group",
            shiny::tags$label("Log-Linear Pooling factor: f(n) = 1 /"),
            shiny::numericInput(
              ns("llp_adjust"),
              label = NULL,
              value = 1.0,
              step = 0.1,
              width = "70px"
            ),
            shiny::tags$label("log(n) / n")
          )
        ),
        shiny::column(
          6,
          shiny::actionButton(
            ns("update_likelihood"),
            "Update Likelihood Map",
            class = "btn-primary",
            icon = shiny::icon("refresh"),
            style = "width: 100%;"
          )
        )
      )
    )
  )
}

modal_calibration_server <- function(
  id,
  twl,
  stapath,
  twl_calib,
  map_light_twl,
  tag,
  compute_known,
  llp_param
) {
  shiny::moduleServer(id, function(input, output, session) {
    make_stap_choices <- function(stapath_) {
      choices <- as.list(stapath_$stap_id)
      known <- !is.na(stapath_$known_lat) & !is.na(stapath_$known_lon)
      names(choices) <- glue::glue(
        "#{stapath_$stap_id} ({round(stapath_$duration, 1)} d.){ifelse(known, ' — known', '')}"
      )
      choices
    }

    get_active_stap_ids <- function(stapath_) {
      twl_calib_ <- twl_calib()
      if (!is.null(twl_calib_) && !is.null(twl_calib_$calib_stap$stap_id)) {
        active_ids <- intersect(twl_calib_$calib_stap$stap_id, stapath_$stap_id)
        if (length(active_ids) > 0) {
          return(active_ids)
        }
      }

      stapath_$stap_id[
        !is.na(stapath_$known_lat) & !is.na(stapath_$known_lon)
      ]
    }

    get_current_adjust <- function() {
      if (!is.null(twl_calib()) && !is.null(twl_calib()$adjust)) {
        twl_calib()$adjust
      } else {
        1.4
      }
    }

    get_current_llp <- function() {
      if (!is.null(llp_param()) && llp_param() != 0) {
        1 / llp_param()
      } else {
        1.0
      }
    }

    # Compute calibration based on selected stap and input adjustment
    current_calibration <- shiny::reactive({
      adjust <- if (is.null(input$twl_calib_adjust)) {
        get_current_adjust()
      } else {
        input$twl_calib_adjust
      }

      stapath_ <- stapath()
      selected_ids <- input$calib_stap_ids
      selected_ids <- unique(as.numeric(selected_ids))
      selected_rows <- match(selected_ids, stapath_$stap_id)
      selected_rows <- selected_rows[!is.na(selected_rows)]
      if (length(selected_rows) < 1) {
        return(NULL)
      }

      # Create stap_known for calibration
      stap_known <- stapath_[selected_rows, , drop = FALSE]
      is_known <- !is.na(stap_known$known_lat) & !is.na(stap_known$known_lon)
      if (!("zenith" %in% names(stap_known))) {
        stap_known$zenith <- NA_real_
      }
      stap_known$known_lat[is.na(stap_known$known_lat)] <- stap_known$lat[
        is.na(stap_known$known_lat)
      ]
      stap_known$known_lon[is.na(stap_known$known_lon)] <- stap_known$lon[
        is.na(stap_known$known_lon)
      ]

      active_stap <- twl_calib()$calib_stap
      if (!is.null(active_stap)) {
        active_rows <- match(stap_known$stap_id, active_stap$stap_id)
        missing_lat <- is.na(stap_known$known_lat)
        missing_lon <- is.na(stap_known$known_lon)
        stap_known$known_lat[missing_lat] <- active_stap$known_lat[
          active_rows[missing_lat]
        ]
        stap_known$known_lon[missing_lon] <- active_stap$known_lon[
          active_rows[missing_lon]
        ]
      }
      stap_known$calib_type <- ifelse(is_known, "known", "fitted")

      # Compute calibration
      tryCatch(
        {
          GeoPressureR::geolight_calibrate(
            twl = twl(),
            calib_stap = stap_known,
            twl_calib_adjust = adjust
          )
        },
        error = function(e) {
          return(NULL)
        }
      )
    })

    # Render the plot
    output$calibration_plot <- plotly::renderPlotly({
      twl_calib_stap <- current_calibration()
      shiny::validate(shiny::need(
        twl_calib_stap,
        "Could not compute calibration for this stationary period."
      ))
      p <- plot_twl_calib(
        twl_calib_stap,
        plot_plotly = TRUE
      )

      twl_calib_orig <- twl_calib()
      if (!is.null(twl_calib_orig)) {
        line_scale <- sum(unlist(
          twl_calib_stap$hist_counts,
          use.names = FALSE
        )) *
          twl_calib_stap$binwidth
        line_data_orig <- data.frame(
          x = twl_calib_orig$x,
          y = twl_calib_orig$y * line_scale
        )

        p <- p |>
          plotly::add_lines(
            data = line_data_orig,
            x = ~x,
            y = ~y,
            line = list(color = "#1f77b4", width = 4),
            name = "Active Calibration",
            hovertemplate = "Zenith angle: %{x:.2f}<br>Density: %{y:.2f}<extra></extra>",
            inherit = FALSE
          )
      }

      p |>
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "select2d",
            "lasso2d",
            "zoomIn2d",
            "zoomOut2d"
          )
        )
    })

    show_calibration_modal <- function() {
      stapath_ <- stapath()

      # Show modal
      shiny::showModal(shiny::modalDialog(
        title = "Likelihood Map Settings",
        modal_calibration_ui(id),
        size = "xl",
        easyClose = TRUE,
        footer = NULL
      ))

      # Update inputs with current active values (must be after showModal)
      choices <- make_stap_choices(stapath_)
      shiny::updateSelectizeInput(
        session,
        "calib_stap_ids",
        choices = choices,
        selected = get_active_stap_ids(stapath_),
        server = TRUE
      )
      current_adjust <- get_current_adjust()
      current_llp <- get_current_llp()

      shiny::updateNumericInput(session, "twl_calib_adjust", value = current_adjust)
      shiny::updateNumericInput(session, "llp_adjust", value = current_llp)
    }

    shiny::observeEvent(input$use_calibration, {
      proposed_calibration <- current_calibration()
      shiny::req(proposed_calibration)

      # Disable button during processing
      shinyjs::disable("use_calibration")
      on.exit(shinyjs::enable("use_calibration"))

      # Recompute twilight likelihood maps
      tryCatch(
        {
          tag_likelihood <- tag
          tag_likelihood$twilight <- twl()
          tag_likelihood$stap <- stapath()
          tag_likelihood$param$geolight_map[[
            "twl_calib"
          ]] <- proposed_calibration
          tag_likelihood <- GeoPressureR::geolight_map_likelihood(
            tag = tag_likelihood,
            compute_known = compute_known,
            quiet = TRUE
          )
          twl_calib(proposed_calibration)
          map_light_twl(tag_likelihood$map_light_twl)
          shiny::showNotification(
            "Calibration updated and likelihood maps recomputed.",
            type = "message"
          )
          shiny::removeModal() # Close modal on success
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error recomputing map:", e$message),
            type = "error"
          )
        }
      )
    })

    shiny::observeEvent(input$update_likelihood, {
      # Disable button during processing
      shinyjs::disable("update_likelihood")
      on.exit(shinyjs::enable("update_likelihood"))

      # Update llp_param (transform input to 1/x)
      if (!is.null(input$llp_adjust) && input$llp_adjust != 0) {
        llp_param(1 / input$llp_adjust)
        shiny::showNotification(
          "Likelihood map updated with new Log-Linear Pool factor.",
          type = "message"
        )
        shiny::removeModal() # Close modal on success
      }
    })

    return(show_calibration_modal)
  })
}
