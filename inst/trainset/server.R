# Source utility functions
source("utils.R")

tag <- shiny::getShinyOption("tag")

# Check if tag is provided
if (is.null(tag)) {
  cli::cli_abort(
    "No tag data found in shiny options. Please provide a valid tag object with {.fun trainset} or {.code shiny::shinyOptions(tag = tag)}."
  )
}

# Preprocess tag data before server starts
# Extract data once for efficiency
pressure_data <- tag$pressure
# Add empty label for NA labels if not present
if (!"label" %in% names(pressure_data)) {
  pressure_data$label <- ""
}

has_acceleration <- !is.null(tag$acceleration) && nrow(tag$acceleration) > 0

acceleration_data <- if (has_acceleration) {
  tag$acceleration
} else {
  data.frame(date = as.POSIXct(character(0)), value = numeric(0), label = character(0))
}
if (!"label" %in% names(acceleration_data)) {
  acceleration_data$label <- ""
}

stap_data <- if (!is.null(tag$stap) && nrow(tag$stap) > 0) {
  tag$stap
} else {
  data.frame(
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0)),
    stap_id = character(0)
  )
}

server <- function(input, output, session) {
  label_dir <- shiny::getShinyOption("label_dir")

  # Fallback: if label_dir option is not set, use ./data/tag-label when it exists
  print(label_dir)
  if (is.null(label_dir)) {
    default_label_dir <- file.path("data", "tag-label")
    if (dir.exists(default_label_dir)) {
      label_dir <- default_label_dir
    }
  }

  auto_save_enabled <- !is.null(label_dir) && dir.exists(label_dir)

  # Reactive container for mutable state derived from the tag
  state <- reactiveValues(
    tag = tag,
    stap_data = {
      d <- stap_data
      d$duration <- stap2duration(d)
      d
    },
    labels_dirty = FALSE
  )

  update_state_tag_labels <- function() {
    state$tag$pressure$label <- reactive_label_pres()
    if (has_acceleration) {
      state$tag$acceleration$label <- reactive_label_acc()
    }
  }

  write_labels_csv <- function(path) {
    update_state_tag_labels()
    tag_label_write(state$tag, file = path, quiet = TRUE)
    state$labels_dirty <- FALSE
  }

  # Handle compute_stap_btn click: update labels and recompute stap
  observeEvent(input$compute_stap_btn, {
    state$tag$pressure$label <- reactive_label_pres()
    if (has_acceleration) {
      state$tag$acceleration$label <- reactive_label_acc()
    }

    state$tag <- tag_label_stap(state$tag, quiet = TRUE)

    state$stap_data <- if (!is.null(state$tag$stap) && nrow(state$tag$stap) > 0) {
      d <- state$tag$stap
      d$duration <- stap2duration(d)
      d
    } else {
      data.frame(
        start = as.POSIXct(character(0)),
        end = as.POSIXct(character(0)),
        stap_id = character(0),
        duration = numeric(0)
      )
    }

    showNotification("STAP recomputed.", type = "message", duration = 2)
  })

  # Configure save/download UI based on availability of auto-save folder
  session$onFlushed(
    function() {
      if (isTRUE(auto_save_enabled)) {
        shinyjs::show("save_btn")
        shinyjs::hide("export_btn")
      } else {
        shinyjs::hide("save_btn")
        shinyjs::show("export_btn")
        folder_msg <- if (!is.null(label_dir)) label_dir else "data/tag-label"
        showNotification(
          glue::glue(
            "Automatic save is disabled: folder {folder_msg} does not exist. Use the download dialog to save labels."
          ),
          duration = 10,
          type = "error"
        )
      }
    },
    once = TRUE
  )

  # Disable add_label_btn when acceleration is active
  observe({
    if (!is.null(input$active_series) && input$active_series == "acceleration") {
      shinyjs::disable("add_label_btn")
    } else {
      shinyjs::enable("add_label_btn")
    }
  })
  # Update browser tab title with tag ID
  session$sendCustomMessage("updateTitle", glue::glue("Trainset - {tag$param$id}"))

  output$header_title <- renderUI({
    tagList(
      span("GeoPressure Trainset"),
      span(
        glue::glue("  {tag$param$id}"),
        style = "font-size: 0.8em; font-weight: 600; color: #e0e0e0; margin-left: 8px;"
      )
    )
  })

  # Handle session end - stop app when browser is closed
  session$onSessionEnded(function() {
    stopApp()
  })

  # Provide availability flags for conditional UI
  output$acceleration_data_available <- reactive({
    has_acceleration
  })
  outputOptions(output, "acceleration_data_available", suspendWhenHidden = FALSE)

  output$stap_data_available <- reactive({
    nrow(state$stap_data) > 0
  })
  outputOptions(output, "stap_data_available", suspendWhenHidden = FALSE)

  # Initialize reactive data with preprocessed data
  reactive_label_pres <- reactiveVal(pressure_data$label)
  reactive_label_acc <- reactiveVal(acceleration_data$label)

  #----- LABELS -----
  # Initialize stap_elev_count from highest elev_xxx in pressure_data$label
  elev_labels <- grep("^elev_\\d+$", pressure_data$label, value = TRUE)
  elev_nums <- as.integer(sub("elev_", "", elev_labels))
  stap_elev_count <- reactiveVal(
    if (length(elev_nums) > 0 && !all(is.na(elev_nums))) max(elev_nums, na.rm = TRUE) else 1
  )

  # Handle add label button click
  observeEvent(input$add_label_btn, {
    stap_elev_count(stap_elev_count() + 1)
  })

  # Dynamically update label choices based on active_series and stap_elev_count
  observe({
    series <- input$active_series
    if (series == "acceleration") {
      label_choices <- "flight"
    } else {
      label_choices <- c("discard")
      if (!has_acceleration) {
        label_choices <- c(label_choices, "flight")
      }
      label_choices <- c(label_choices, paste0("elev_", seq(1, stap_elev_count())))
    }
    session$onFlushed(function() {
      updateSelectInput(
        session,
        "label_select",
        choices = label_choices,
        selected = label_choices[[length(label_choices)]]
      )
    })
    # Show or hide compute_stap_btn based on presence of 'flight' label
    if ("flight" %in% label_choices) {
      shinyjs::show("compute_stap_btn")
    } else {
      shinyjs::hide("compute_stap_btn")
    }
  })

  #----- STAP -----
  # Update stap_id choices when stap data is available/updated
  observe({
    d <- state$stap_data
    if (nrow(d) == 0) {
      return()
    }

    stap_choices <- c(
      "None" = "",
      setNames(
        d$stap_id,
        glue::glue("#{d$stap_id} ({round(d$duration, 1)}d)")
      )
    )

    current <- isolate(input$stap_id)
    if (is.null(current) || !(current %in% d$stap_id)) {
      current <- ""
    }

    updateSelectInput(
      session,
      "stap_id",
      choices = stap_choices,
      selected = current
    )
  })

  observeEvent(input$stap_id_prev, {
    d <- state$stap_data
    if (nrow(d) == 0) {
      return()
    }

    current_stap <- input$stap_id

    if (current_stap == "" || is.null(current_stap)) {
      # If no stap selected, select the first one
      new_stap <- d$stap_id[1]
    } else {
      # Find current index and move to previous
      current_index <- which(d$stap_id == current_stap)
      if (length(current_index) > 0 && current_index > 1) {
        new_stap <- d$stap_id[current_index - 1]
      } else {
        # If at first stap, go to "None"
        new_stap <- ""
      }
    }
    updateSelectInput(session, "stap_id", selected = new_stap)
  })

  observeEvent(input$stap_id_next, {
    d <- state$stap_data
    if (nrow(d) == 0) {
      return()
    }

    current_stap <- input$stap_id

    if (current_stap == "" || is.null(current_stap)) {
      # If no stap selected, select the first one
      new_stap <- d$stap_id[1]
    } else {
      # Find current index and move to next
      current_index <- which(d$stap_id == current_stap)
      if (length(current_index) > 0 && current_index < nrow(d)) {
        new_stap <- d$stap_id[current_index + 1]
      } else {
        # If at last stap, stay there
        new_stap <- current_stap
      }
    }
    updateSelectInput(session, "stap_id", selected = new_stap)
  })

  # Handle stap_id selection to set x-axis limits only if stap data exists
  observeEvent(input$stap_id, {
    if (!is.null(input$stap_id) && input$stap_id != "") {
      d <- state$stap_data
      if (nrow(d) == 0) {
        return()
      }
      # Find the selected stap
      selected_stap <- d[d$stap_id == input$stap_id, ]

      if (nrow(selected_stap) > 0) {
        lag_x <- 60 * 60 * 24 / 2 # 1 day in seconds
        lag_y <- 5 # Pressure units

        pressure_val_stap_id <- pressure_data$value[
          pressure_data$date >= selected_stap$start & pressure_data$date <= selected_stap$end
        ]

        # Update the plot x-axis range
        layout_update <- list(
          xaxis = list(range = list(selected_stap$start - lag_x, selected_stap$end + lag_x)),
          yaxis = list(
            range = c(min(pressure_val_stap_id) - lag_y, max(pressure_val_stap_id) + lag_y)
          )
        )

        plotlyProxy("ts_plot", session) |>
          plotlyProxyInvoke("relayout", layout_update)
      }
    }
  })

  # Update active_series choices based on available data
  init_active_series <- "pressure"
  if (has_acceleration) {
    # Both pressure and acceleration available - selector will be shown by conditionalPanel
    updateSelectInput(
      session,
      "active_series",
      choices = c("Pressure" = "pressure", "Acceleration" = "acceleration"),
      selected = init_active_series
    )
  }

  # Get initial styling
  initial_styles <- get_plot_styles(
    init_active_series,
    pressure_data$label,
    acceleration_data$label
  )

  # Pre-compute static values that don't change
  time_range <- list(
    min(c(pressure_data$date, acceleration_data$date)),
    max(c(pressure_data$date, acceleration_data$date))
  )

  # Main plot rendering - static plot created only once
  output$ts_plot <- renderPlotly({
    # Create the plot with dual y-axes (optimized for performance)
    p <- plot_ly() |>
      # Add pressure LINE trace for range slider visibility
      add_trace(
        data = pressure_data,
        x = ~date,
        y = ~value,
        type = "scatter", # Regular scatter for range slider compatibility
        mode = "lines", # Lines only for range slider
        name = "Pressure_line",
        line = list(
          width = initial_styles$pressure_line_style$line.width,
          color = initial_styles$pressure_line_style$line.color
        ),
        yaxis = "y", # Use primary y-axis for range slider compatibility
        hovertemplate = "Pressure: %{y}<br>Time: %{x}<extra></extra>",
        showlegend = FALSE,
        visible = TRUE
      ) |>
      # Add pressure MARKERS trace for interaction and performance
      add_trace(
        data = pressure_data,
        x = ~date,
        y = ~value,
        type = "scattergl", # Use WebGL for better performance with many points
        mode = "markers", # Markers only for interaction
        name = "Pressure",
        marker = list(
          size = initial_styles$pressure_markers_style$marker.size,
          color = initial_styles$pressure_markers_style$marker.color[[1]], # Extract from list
          opacity = initial_styles$pressure_markers_style$marker.opacity
        ),
        yaxis = "y", # Same y-axis as line trace
        hoverinfo = "skip", # Make sure hover is enabled
        showlegend = FALSE
      )

    # Conditionally add acceleration trace if acceleration data exists
    if (has_acceleration) {
      p <- p |>
        add_trace(
          data = acceleration_data,
          x = ~date,
          y = ~value,
          type = "scattergl", # Use WebGL for better performance
          mode = "lines+markers", # Drop lines for large datasets
          name = "Acceleration",
          line = list(
            width = initial_styles$acceleration_style$line.width,
            color = initial_styles$acceleration_style$line.color
          ),
          marker = list(
            size = initial_styles$acceleration_style$marker.size,
            color = initial_styles$acceleration_style$marker.color[[1]], # Extract from list
            opacity = initial_styles$acceleration_style$marker.opacity
          ),
          yaxis = "y2",
          hovertemplate = "Acceleration: %{y}<br>Time: %{x}<extra></extra>", # Simplified hover
          showlegend = FALSE,
          visible = TRUE
        )
    }

    # Create layout configuration conditionally
    layout_config <- list(
      xaxis = list(
        title = "Time",
        range = time_range,
        # Range selector with dropdown-style configuration
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "1D",
              step = "day",
              stepmode = "backward"
            ),
            list(
              count = 7,
              label = "1W",
              step = "day",
              stepmode = "backward"
            ),
            list(
              count = 1,
              label = "1M",
              step = "month",
              stepmode = "backward"
            ),
            list(step = "all", label = "All")
          )
        ),
        # Range slider at the bottom
        rangeslider = list(
          visible = TRUE,
          range = time_range
        )
      ),
      yaxis = list(
        title = "Pressure",
        side = "left",
        fixedrange = FALSE
      ),
      dragmode = "select",
      selectdirection = "d", # Prevent horizontal/vertical only selection
      showlegend = FALSE,
      # Add margins to provide space for the right y-axis and range controls
      margin = list(
        l = 60, # Left margin for pressure y-axis
        r = 80, # Right margin for acceleration y-axis (increased)
        t = 80, # Top margin (increased for range selector)
        b = 60 # Bottom margin (increased for range slider)
      ),
      # Performance optimizations
      hovermode = "closest"
    )

    # Add yaxis2 if acceleration data exists
    if (has_acceleration) {
      layout_config$yaxis2 <- list(
        title = "Acceleration",
        side = "right",
        overlaying = "y",
        position = 1,
        fixedrange = FALSE,
        scaleanchor = NULL
      )
    }

    p_with_layout <- do.call(layout, c(list(p), layout_config))

    p_with_layout |>
      config(
        scrollZoom = FALSE, # Disable built-in scroll zoom
        displayModeBar = TRUE,
        doubleClick = "reset", # Reset zoom on double click
        modeBarButtonsToRemove = list(
          "zoomIn2d",
          "zoomOut2d",
          "autoScale2d",
          "resetScale2d",
          "toImage",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "select2d", # Remove box select tool
          "lasso2d" # Remove lasso select tool
        ),
        displaylogo = FALSE
      ) |>
      # Setup plotly event handlers for selection and click with Ctrl/Cmd key detection
      htmlwidgets::onRender(
        "
        function(el, x) {
          setupPlotlyEventHandlers(el);
        }
      "
      )
  })

  # Periodic backup of labels to CSV when they change and label_dir exists
  backup_timer <- reactiveTimer(60000, session) # every 60 seconds

  observe({
    backup_timer()

    if (!isTRUE(auto_save_enabled)) {
      return()
    }

    if (!isTRUE(state$labels_dirty)) {
      return()
    }

    base_name <- tools::file_path_sans_ext(state$tag$param$id)
    backup_file <- file.path(
      label_dir,
      glue::glue("{base_name}-labeled-backup-{format(Sys.time(), '%Y%m%d-%H%M%S')}.csv")
    )

    try(
      {
        write_labels_csv(backup_file)
      },
      silent = TRUE
    )
  })

  # Handle active series changes and label updates without resetting view
  # Use debounced reactive to prevent excessive updates
  observeEvent(
    {
      list(
        input$active_series,
        reactive_label_pres(),
        reactive_label_acc()
      )
    },
    {
      # Apply styling using the external function
      apply_plot_styling(
        plotlyProxy("ts_plot", session),
        input$active_series,
        reactive_label_pres(),
        reactive_label_acc()
      )
    }
  )

  # Shared labeling function for both selection and click events
  apply_labels_to_points <- function(point_data, ctrl_pressed = FALSE) {
    active_series <- input$active_series

    # Use empty string if Ctrl/Cmd is pressed, otherwise use selected label
    selected_label <- if (isTRUE(ctrl_pressed)) "" else input$label_select

    # Handle empty data - do nothing silently
    if (is.null(point_data) || nrow(point_data) == 0 || length(point_data) == 0) {
      return()
    }

    # Treat pressure line clicks as pressure markers for labeling
    point_data$curveNumber[point_data$curveNumber == 0] <- 1

    # Filter points to keep only those from the active series
    # Determine which curveNumber corresponds to the active series
    has_acceleration <- !is.null(acceleration_data)
    target_curve <- if (active_series == "pressure") {
      1 # Pressure markers are curveNumber 1
    } else if (active_series == "acceleration" && has_acceleration) {
      2 # Acceleration is curveNumber 2
    } else {
      NULL
    }

    # Filter points to keep only those from the active series
    if (!is.null(target_curve)) {
      point_data <- point_data[point_data$curveNumber == target_curve, , drop = FALSE]
    }

    # Check if any points remain after filtering
    if (nrow(point_data) == 0) {
      showNotification(
        glue::glue(
          "No {active_series} points in selection. Switch active series or select different points."
        ),
        duration = 3,
        type = "warning"
      )
      return()
    }

    # Apply labels to the filtered points
    point_indices <- point_data$pointNumber + 1 # Convert to 1-based indexing

    if (active_series == "pressure") {
      current_labels <- reactive_label_pres()
      current_labels[point_indices] <- selected_label
      reactive_label_pres(current_labels)
    } else if (active_series == "acceleration") {
      current_labels <- reactive_label_acc()
      current_labels[point_indices] <- selected_label
      reactive_label_acc(current_labels)
    }

    state$labels_dirty <- TRUE

    # Show success notification with appropriate message
    label_action <- if (isTRUE(ctrl_pressed)) {
      "Cleared labels from"
    } else {
      glue::glue("Applied label '{selected_label}' to")
    }
    showNotification(
      glue::glue("{label_action} {length(point_indices)} {active_series} points"),
      duration = 2,
      type = "message"
    )
  }

  # Helper function to process plotly events (selection/click)
  process_plotly_event <- function(event_info) {
    if (!is.null(event_info$points) && length(event_info$points) > 0) {
      # Convert to data frame format expected by apply_labels_to_points
      point_data <- data.frame(
        pointNumber = sapply(event_info$points, function(p) {
          if (is.null(p$pointNumber)) 0 else p$pointNumber
        }),
        curveNumber = sapply(event_info$points, function(p) {
          if (is.null(p$curveNumber)) 0 else p$curveNumber
        }),
        x = sapply(event_info$points, function(p) if (is.null(p$x)) NA else p$x),
        y = sapply(event_info$points, function(p) if (is.null(p$y)) NA else p$y),
        stringsAsFactors = FALSE
      )
      apply_labels_to_points(point_data, event_info$ctrlPressed)
    }
    apply_plot_styling(
      plotlyProxy("ts_plot", session),
      input$active_series,
      reactive_label_pres(),
      reactive_label_acc()
    )
  }

  # Handle point selection events with direct key state
  observeEvent(input$plotly_selected_with_keys, {
    process_plotly_event(input$plotly_selected_with_keys)
  })

  # Handle point click events with direct key state
  observeEvent(input$plotly_click_with_keys, {
    process_plotly_event(input$plotly_click_with_keys)
  })

  # Handle save button - auto-save to project data/tag-label if available
  observeEvent(input$save_btn, {
    if (!isTRUE(auto_save_enabled)) {
      showNotification(
        "Automatic save is disabled for this session. Use the download dialog instead.",
        duration = 10,
        type = "error"
      )
      return()
    }

    base_name <- tools::file_path_sans_ext(state$tag$param$id)
    target_dir <- label_dir

    target_file <- file.path(target_dir, glue::glue("{base_name}-labeled.csv"))

    tryCatch(
      {
        write_labels_csv(target_file)
        showNotification(
          glue::glue("Labels saved to {target_file}"),
          duration = 5,
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          glue::glue("Save failed: {e$message}. Using manual download instead."),
          duration = 10,
          type = "warning"
        )
        shinyjs::click("export_btn")
      }
    )
  })

  # Download handler for manual export (fallback when label_dir is missing)
  output$export_btn <- downloadHandler(
    filename = function() {
      base_name <- tools::file_path_sans_ext(state$tag$param$id)
      glue::glue("{base_name}-labeled.csv")
    },
    content = function(file) {
      tryCatch(
        {
          write_labels_csv(file)
        },
        error = function(e) {
          showNotification(
            glue::glue("Download failed: {e$message}"),
            duration = 10,
            type = "error"
          )
        }
      )
    },
    contentType = "text/csv"
  )

  #----- SHORTCUTS HELP MODAL -----
  observeEvent(input$shortcuts_help, {
    showModal(
      modalDialog(
        title = "Keyboard and mouse shortcuts",
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close"),
        tagList(
          h4("Labeling"),
          tags$ul(
            tags$li("Click or drag on the plot to apply the selected label."),
            tags$li(
              tagList(
                tags$kbd("Ctrl"),
                " (Windows / Linux) or ",
                tags$kbd("Cmd"),
                " (macOS) + mouse click/selection: clear labels instead of applying a new one."
              )
            )
          ),
          h4("Time navigation (x-axis)"),
          tags$ul(
            tags$li(
              tagList(
                tags$kbd("Mouse wheel"),
                " over plot area: zoom in/out in time."
              )
            ),
            tags$li(
              tagList(
                tags$kbd("\u2191"),
                " / ",
                tags$kbd("\u2193"),
                ": zoom in / out around the current view."
              )
            ),
            tags$li(
              tagList(
                tags$kbd("\u2190"),
                " / ",
                tags$kbd("\u2192"),
                ": pan left / right in time."
              )
            ),
            tags$li(
              tagList(
                tags$kbd("Shift"),
                " + ",
                tags$kbd("\u2190"),
                " / ",
                tags$kbd("\u2192"),
                ": pan by a full window width."
              )
            )
          ),
          h4("Y-axis zoom"),
          tags$ul(
            tags$li(
              tagList(
                tags$kbd("Mouse wheel"),
                " over left y-axis: zoom pressure axis."
              )
            ),
            tags$li(
              tagList(
                tags$kbd("Mouse wheel"),
                " over right y-axis: zoom acceleration axis (when available)."
              )
            )
          ),
          h4("STAP navigation"),
          tags$ul(
            tags$li(
              tagList(
                "Use the ",
                tags$kbd("<"),
                " and ",
                tags$kbd(">"),
                " buttons next to \"Stap ID\" to move between stationary periods."
              )
            )
          ),
          h4("Auto-save and files"),
          tags$ul(
            tags$li(
              "If an automatic label folder is configured (e.g. data/tag-label), clicking \"Save\" writes the main label CSV for this tag (e.g. {tag-id}-labeled.csv)."
            ),
            tags$li(
              "While you are editing labels and auto-save is enabled, a time-stamped backup CSV is also written automatically about once per minute in the same folder (files named like {tag-id}-labeled-backup-YYYYMMDD-HHMMSS.csv)."
            ),
            tags$li(
              "If no auto-save folder is available, use the \"Download\" button to manually save a CSV of the current labels."
            )
          ),
          h4("More information"),
          tags$p(
            "For detailed labelling instructions, see the ",
            tags$a(
              href = "https://raphaelnussbaumer.com/GeoPressureManual/labelling-tracks.html",
              target = "_blank",
              "GeoPressure Manual: labelling tracks"
            ),
            "."
          )
        )
      )
    )
  })
}
