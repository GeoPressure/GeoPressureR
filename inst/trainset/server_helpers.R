# Helpers sourced inside `server()`. Assumes the following exist in the calling environment:
# `input`, `output`, `session`, `state`, `has_pressure`, `has_acceleration`, curve indices, and
# `reactive_label_*`.

dbg <- function(...) {
  if (!isTRUE(trainset_debug)) {
    return(invisible())
  }
  ts <- format(Sys.time(), "%H:%M:%S")
  msg <- paste0("[TRAINSET_DEBUG ", ts, "] ", paste0(..., collapse = ""))
  message(msg)
}

active_series_or_default <- function() {
  series <- isolate(input$active_series)
  if (!is.null(series)) {
    return(series)
  }
  if (has_pressure) "pressure" else "acceleration"
}

view_labels <- function() {
  list(
    pressure = if (has_pressure) reactive_label_pres()[state$pressure_detail_idx] else NULL,
    acceleration = if (has_acceleration) {
      reactive_label_acc()[state$acceleration_detail_idx]
    } else {
      NULL
    }
  )
}

apply_current_styling <- function(
  plot_proxy,
  pressure_labels,
  acceleration_labels = NULL,
  active_series = NULL
) {
  if (is.null(active_series)) {
    active_series <- active_series_or_default()
  }
  if (!has_pressure) {
    pressure_labels <- NULL
  }
  if (!has_acceleration) {
    acceleration_labels <- NULL
  }

  apply_plot_styling(
    plot_proxy,
    active_series,
    pressure_labels,
    acceleration_labels,
    has_pressure = has_pressure,
    has_acceleration = has_acceleration,
    acc_has_lines = if (has_acceleration) state$acc_has_lines else TRUE,
    pressure_overview_trace = curve_overview_pressure,
    pressure_markers_trace = curve_pressure_markers,
    pressure_detail_trace = curve_pressure_detail_line,
    acceleration_trace = curve_acceleration
  )
}

restyle_xy <- function(plot_proxy, curve, x, y, row_index, text = NULL, extra = NULL) {
  base_payload <- list(
    x = list(x),
    y = list(y),
    customdata = list(row_index)
  )
  if (!is.null(text)) {
    base_payload$text <- list(text)
  }

  payload <- c(base_payload, extra)
  plot_proxy |>
    plotly::plotlyProxyInvoke("restyle", payload, list(curve))
}

points_to_df <- function(points) {
  data.frame(
    pointNumber = vapply(
      points,
      function(p) {
        if (is.null(p$pointNumber)) 0 else p$pointNumber
      },
      numeric(1)
    ),
    curveNumber = vapply(
      points,
      function(p) {
        if (is.null(p$curveNumber)) 0 else p$curveNumber
      },
      numeric(1)
    ),
    rowIndex = vapply(
      points,
      function(p) {
        if (is.null(p$customdata)) {
          return(NA_real_)
        }
        cd <- p$customdata
        if (is.list(cd)) {
          cd <- cd[[1]]
        }
        as.numeric(cd)
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

refresh_stap_state <- function() {
  if (has_pressure) {
    state$tag$pressure$label <- reactive_label_pres()
  }
  if (has_acceleration) {
    state$tag$acceleration$label <- reactive_label_acc()
  }

  state$tag <- tag_label_stap(
    state$tag,
    quiet = TRUE
  )

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
}

zoom_to_window <- function(start, end, lag_x_hours = 12, lag_y = 5) {
  lag_x <- as.difftime(lag_x_hours, units = "hours")
  xmin <- start - lag_x
  xmax <- end + lag_x

  layout_update <- list(
    xaxis = list(range = list(xmin, xmax))
  )

  active_series <- active_series_or_default()
  if (active_series == "acceleration" && has_acceleration) {
    y_vals <- acceleration_data$value[acceleration_data$date >= start & acceleration_data$date <= end]
    y_axis <- if (has_pressure) "yaxis2" else "yaxis"
  } else if (has_pressure) {
    y_vals <- pressure_data$value[pressure_data$date >= start & pressure_data$date <= end]
    y_axis <- "yaxis"
  } else if (has_acceleration) {
    y_vals <- acceleration_data$value[acceleration_data$date >= start & acceleration_data$date <= end]
    y_axis <- "yaxis"
  } else {
    y_vals <- numeric(0)
    y_axis <- "yaxis"
  }

  if (length(y_vals) > 0 && !all(is.na(y_vals))) {
    layout_update[[y_axis]] <- list(
      range = c(
        min(y_vals, na.rm = TRUE) - lag_y,
        max(y_vals, na.rm = TRUE) + lag_y
      )
    )
  }

  plotly::plotlyProxy("ts_plot", session) |>
    plotly::plotlyProxyInvoke("relayout", layout_update)

  state$last_relayout_ms <- c(round(as.numeric(xmin) * 1000), round(as.numeric(xmax) * 1000))
  refresh_detail_traces(xmin, xmax)
}
