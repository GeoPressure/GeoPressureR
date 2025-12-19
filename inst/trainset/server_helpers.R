# Helpers sourced inside `server()`. Assumes the following exist in the calling environment:
# `input`, `output`, `session`, `state`, `has_acceleration`, curve indices, and `reactive_label_*`.

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
  if (is.null(series)) "pressure" else series
}

view_labels <- function() {
  list(
    pressure = reactive_label_pres()[state$pressure_detail_idx],
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
  if (!has_acceleration) {
    acceleration_labels <- NULL
  }

  apply_plot_styling(
    plot_proxy,
    active_series,
    pressure_labels,
    acceleration_labels,
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
