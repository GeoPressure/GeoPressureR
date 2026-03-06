# STAP navigation + recompute. Sourced inside `server()`.

shiny::observeEvent(input$compute_stap_btn, {
  if (has_pressure) {
    state$tag$pressure$label <- reactive_label_pres()
  }
  if (has_acceleration) {
    state$tag$acceleration$label <- reactive_label_acc()
  }

  state$tag <- GeoPressureR::tag_label_stap(state$tag, quiet = TRUE)

  state$stap_data <- if (!is.null(state$tag$stap) && nrow(state$tag$stap) > 0) {
    d <- state$tag$stap
    d$duration <- GeoPressureR::stap2duration(d)
    d
  } else {
    data.frame(
      start = as.POSIXct(character(0)),
      end = as.POSIXct(character(0)),
      stap_id = character(0),
      duration = numeric(0)
    )
  }

  shiny::showNotification("STAP recomputed.", type = "message", duration = 2)
})

#----- STAP -----
shiny::observe({
  d <- state$stap_data
  if (nrow(d) == 0) {
    return()
  }

  stap_choices <- c(
    "None" = "",
    stats::setNames(
      d$stap_id,
      glue::glue("#{d$stap_id} ({round(d$duration, 1)}d)")
    )
  )

  current <- isolate(input$stap_id)
  if (is.null(current) || !(current %in% d$stap_id)) {
    current <- ""
  }

  shiny::updateSelectInput(
    session,
    "stap_id",
    choices = stap_choices,
    selected = current
  )
})

shiny::observeEvent(input$stap_id_prev, {
  d <- state$stap_data
  if (nrow(d) == 0) {
    return()
  }

  current_stap <- input$stap_id

  if (current_stap == "" || is.null(current_stap)) {
    new_stap <- d$stap_id[1]
  } else {
    current_index <- which(d$stap_id == current_stap)
    if (length(current_index) > 0 && current_index > 1) {
      new_stap <- d$stap_id[current_index - 1]
    } else {
      new_stap <- ""
    }
  }
  shiny::updateSelectInput(session, "stap_id", selected = new_stap)
})

shiny::observeEvent(input$stap_id_next, {
  d <- state$stap_data
  if (nrow(d) == 0) {
    return()
  }

  current_stap <- input$stap_id

  if (current_stap == "" || is.null(current_stap)) {
    new_stap <- d$stap_id[1]
  } else {
    current_index <- which(d$stap_id == current_stap)
    if (length(current_index) > 0 && current_index < nrow(d)) {
      new_stap <- d$stap_id[current_index + 1]
    } else {
      new_stap <- current_stap
    }
  }
  shiny::updateSelectInput(session, "stap_id", selected = new_stap)
})

shiny::observeEvent(input$stap_id, {
  if (!is.null(input$stap_id) && input$stap_id != "") {
    d <- state$stap_data
    if (nrow(d) == 0) {
      return()
    }
    selected_stap <- d[d$stap_id == input$stap_id, ]

    if (nrow(selected_stap) > 0) {
      lag_x <- 60 * 60 * 24 / 2
      lag_y <- 5

      layout_update <- list(
        xaxis = list(range = list(selected_stap$start - lag_x, selected_stap$end + lag_x))
      )
      if (has_pressure) {
        pressure_val_stap_id <- pressure_data$value[
          pressure_data$date >= selected_stap$start & pressure_data$date <= selected_stap$end
        ]
        if (length(pressure_val_stap_id) > 0 && !all(is.na(pressure_val_stap_id))) {
          layout_update$yaxis <- list(
            range = c(min(pressure_val_stap_id, na.rm = TRUE) - lag_y, max(pressure_val_stap_id, na.rm = TRUE) + lag_y)
          )
        }
      } else if (has_acceleration) {
        acceleration_val_stap_id <- acceleration_data$value[
          acceleration_data$date >= selected_stap$start & acceleration_data$date <= selected_stap$end
        ]
        if (length(acceleration_val_stap_id) > 0 && !all(is.na(acceleration_val_stap_id))) {
          layout_update$yaxis <- list(
            range = c(min(acceleration_val_stap_id, na.rm = TRUE) - lag_y, max(acceleration_val_stap_id, na.rm = TRUE) + lag_y)
          )
        }
      }

      plotly::plotlyProxy("ts_plot", session) |>
        plotly::plotlyProxyInvoke("relayout", layout_update)

      # Ensure the interactive traces refresh even if Plotly does not emit a usable relayout payload.
      xmin <- selected_stap$start - lag_x
      xmax <- selected_stap$end + lag_x
      state$last_relayout_ms <- c(round(as.numeric(xmin) * 1000), round(as.numeric(xmax) * 1000))
      refresh_detail_traces(xmin, xmax)
    }
  }
})
