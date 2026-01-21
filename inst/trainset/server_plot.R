# Plot + windowed rendering. Sourced inside `server()`.

subset_for_view <- function(
  dates_sorted,
  values_sorted,
  time_sorted,
  row_map,
  labels,
  xmin,
  xmax,
  max_points,
  min_window_seconds
) {
  xmin_num <- as.numeric(xmin)
  xmax_num <- as.numeric(xmax)

  pos_view <- range_indices_sorted(time_sorted, xmin_num, xmax_num)

  if (
    is.finite(min_window_seconds) &&
      !is.na(min_window_seconds) &&
      min_window_seconds > 0 &&
      abs(xmax_num - xmin_num) < min_window_seconds
  ) {
    center <- (xmin_num + xmax_num) / 2
    xmin_num <- center - min_window_seconds / 2
    xmax_num <- center + min_window_seconds / 2
  }

  pos_data <- range_indices_sorted(time_sorted, xmin_num, xmax_num)
  if (length(pos_data) == 0) {
    return(list(
      date = as.POSIXct(character(0), tz = time_tz),
      value = numeric(0),
      label = character(0),
      row_index = integer(0)
    ))
  }

  pos_keep <- pos_data

  if (!is.infinite(max_points) && length(pos_keep) > max_points) {
    pos_view <- unique(pos_view)
    pos_view <- pos_view[pos_view >= pos_keep[[1]] & pos_view <= pos_keep[[length(pos_keep)]]]

    if (length(pos_view) >= max_points) {
      pos_keep <- pos_view[downsample_indices(length(pos_view), max_points)]
    } else {
      remaining_needed <- as.integer(max_points - length(pos_view))
      remaining <- if (length(pos_view) > 0) setdiff(pos_data, pos_view) else pos_data
      if (length(remaining) > remaining_needed && remaining_needed > 0) {
        remaining <- remaining[downsample_indices(length(remaining), remaining_needed)]
      }
      pos_keep <- sort.int(unique(c(pos_view, remaining)))
    }
  }

  row_index <- row_map[pos_keep]
  list(
    date = dates_sorted[pos_keep],
    value = values_sorted[pos_keep],
    label = labels[row_index],
    row_index = row_index
  )
}

refresh_detail_traces <- function(xmin, xmax) {
  state$view_xmin <- xmin
  state$view_xmax <- xmax

  view_seconds <- as.numeric(difftime(xmax, xmin, units = "secs"))
  dbg("refresh_detail_traces view=", round(view_seconds / 3600, 3), "h")

  pres <- subset_for_view(
    pressure_date_sorted,
    pressure_value_sorted,
    pressure_time_sorted,
    pressure_sort_idx,
    reactive_label_pres(),
    xmin,
    xmax,
    max_points_detail,
    min_window_seconds
  )
  state$pressure_detail_idx <- pres$row_index
  dbg("pressure points=", length(pres$row_index))

  plot_proxy <- plotly::plotlyProxy("ts_plot", session)
  active_series <- active_series_or_default()

  acc_labels_view <- NULL
  if (has_acceleration) {
    acc <- subset_for_view(
      acceleration_date_sorted,
      acceleration_value_sorted,
      acceleration_time_sorted,
      acceleration_sort_idx,
      reactive_label_acc(),
      xmin,
      xmax,
      max_points_detail,
      min_window_seconds
    )
    state$acceleration_detail_idx <- acc$row_index
    dbg("acceleration points=", length(acc$row_index))

    acc_mode <- if (length(acc$row_index) > 20000) "markers" else "lines+markers"
    state$acc_has_lines <- identical(acc_mode, "lines+markers")

    acc_labels_view <- acc$label
  }

  styles <- get_plot_styles(
    active_series,
    pres$label,
    acc_labels_view,
    acc_has_lines = if (has_acceleration) state$acc_has_lines else TRUE
  )

  # Clear selection and update data + styling together to avoid a brief color mismatch flicker.
  plot_proxy |>
    plotly::plotlyProxyInvoke("restyle", list(selectedpoints = NULL))

  restyle_xy(
    plot_proxy,
    curve_pressure_detail_line,
    pres$date,
    pres$value,
    pres$row_index,
    extra = styles$pressure_detail_line_style
  )
  restyle_xy(
    plot_proxy,
    curve_pressure_markers,
    pres$date,
    pres$value,
    pres$row_index,
    text = pres$label,
    extra = styles$pressure_markers_style
  )

  if (has_acceleration && !is.null(acc_labels_view) && !is.null(styles$acceleration_style)) {
    restyle_xy(
      plot_proxy,
      curve_acceleration,
      acc$date,
      acc$value,
      acc$row_index,
      extra = c(styles$acceleration_style, list(mode = list(acc_mode)))
    )
  }
}

# Active series selector (only when acceleration exists)
init_active_series <- "pressure"
if (has_acceleration) {
  shiny::updateSelectInput(
    session,
    "active_series",
    choices = c("Pressure" = "pressure", "Acceleration" = "acceleration"),
    selected = init_active_series
  )
}

# Overview (range slider) is a light downsample of pressure.
n_pressure_sorted <- length(pressure_sort_idx)
pressure_overview_idx <- downsample_indices(n_pressure_sorted, max_points_overview)
pressure_overview <- data.frame(
  date = pressure_date_sorted[pressure_overview_idx],
  value = pressure_value_sorted[pressure_overview_idx],
  row_index = pressure_sort_idx[pressure_overview_idx]
)

# Initial detail traces use the full time range but are capped.
initial_pressure_detail <- subset_for_view(
  pressure_date_sorted,
  pressure_value_sorted,
  pressure_time_sorted,
  pressure_sort_idx,
  isolate(reactive_label_pres()),
  time_min,
  time_max,
  max_points_detail,
  min_window_seconds
)
state$pressure_detail_idx <- initial_pressure_detail$row_index

initial_acc_detail <- NULL
acc_has_lines_initial <- TRUE
if (has_acceleration) {
  initial_acc_detail <- subset_for_view(
    acceleration_date_sorted,
    acceleration_value_sorted,
    acceleration_time_sorted,
    acceleration_sort_idx,
    isolate(reactive_label_acc()),
    time_min,
    time_max,
    max_points_detail,
    min_window_seconds
  )
  state$acceleration_detail_idx <- initial_acc_detail$row_index
  acc_has_lines_initial <- length(initial_acc_detail$row_index) <= 20000
  state$acc_has_lines <- acc_has_lines_initial
}

initial_styles <- get_plot_styles(
  init_active_series,
  initial_pressure_detail$label,
  if (has_acceleration) initial_acc_detail$label else NULL,
  acc_has_lines = acc_has_lines_initial
)

time_range <- list(time_min, time_max)

output$ts_plot <- plotly::renderPlotly({
  p <- plotly::plot_ly() |>
    plotly::add_trace(
      data = pressure_overview,
      x = ~date,
      y = ~value,
      type = "scatter",
      mode = "lines",
      name = "Pressure_overview",
      line = list(
        width = initial_styles$pressure_overview_line_style$line.width,
        color = initial_styles$pressure_overview_line_style$line.color
      ),
      yaxis = "y",
      hoverinfo = "skip",
      showlegend = FALSE,
      visible = TRUE,
      customdata = ~row_index
    ) |>
    plotly::add_trace(
      data = data.frame(
        date = initial_pressure_detail$date,
        value = initial_pressure_detail$value,
        row_index = initial_pressure_detail$row_index
      ),
      x = ~date,
      y = ~value,
      type = "scattergl",
      mode = "lines",
      name = "Pressure_detail_line",
      line = list(
        width = initial_styles$pressure_detail_line_style$line.width,
        color = initial_styles$pressure_detail_line_style$line.color
      ),
      yaxis = "y",
      hoverinfo = "skip",
      showlegend = FALSE,
      customdata = ~row_index
    ) |>
    plotly::add_trace(
      data = data.frame(
        date = initial_pressure_detail$date,
        value = initial_pressure_detail$value,
        label = initial_pressure_detail$label,
        row_index = initial_pressure_detail$row_index
      ),
      x = ~date,
      y = ~value,
      type = "scattergl",
      mode = "markers",
      name = "Pressure",
      marker = list(
        size = initial_styles$pressure_markers_style$marker.size,
        color = initial_styles$pressure_markers_style$marker.color[[1]],
        opacity = initial_styles$pressure_markers_style$marker.opacity
      ),
      yaxis = "y",
      text = ~label,
      hovertemplate = "Pressure: %{y}<br>Time: %{x}<br>Label: %{text}<extra></extra>",
      showlegend = FALSE,
      customdata = ~row_index
    )

  if (has_acceleration) {
    acc_mode <- if (length(initial_acc_detail$row_index) > 20000) "markers" else "lines+markers"
    p <- p |>
      plotly::add_trace(
        data = data.frame(
          date = initial_acc_detail$date,
          value = initial_acc_detail$value,
          row_index = initial_acc_detail$row_index
        ),
        x = ~date,
        y = ~value,
        type = "scattergl",
        mode = acc_mode,
        name = "Acceleration",
        line = if (identical(acc_mode, "lines+markers")) {
          list(
            width = initial_styles$acceleration_style$line.width,
            color = initial_styles$acceleration_style$line.color
          )
        } else {
          NULL
        },
        marker = list(
          size = initial_styles$acceleration_style$marker.size,
          color = initial_styles$acceleration_style$marker.color[[1]],
          opacity = initial_styles$acceleration_style$marker.opacity
        ),
        yaxis = "y2",
        hovertemplate = "Acceleration: %{y}<br>Time: %{x}<extra></extra>",
        showlegend = FALSE,
        visible = TRUE,
        customdata = ~row_index
      )
  }

  layout_config <- list(
    xaxis = list(
      title = "Time",
      range = time_range,
      rangeselector = list(
        buttons = list(
          list(count = 1, label = "1D", step = "day", stepmode = "backward"),
          list(count = 7, label = "1W", step = "day", stepmode = "backward"),
          list(count = 1, label = "1M", step = "month", stepmode = "backward"),
          list(step = "all", label = "All")
        )
      ),
      rangeslider = list(visible = TRUE, range = time_range)
    ),
    yaxis = list(title = "Pressure", side = "left", fixedrange = FALSE),
    dragmode = "select",
    selectdirection = "d",
    showlegend = FALSE,
    margin = list(l = 60, r = 80, t = 80, b = 60),
    hovermode = "closest"
  )

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

  p_with_layout <- do.call(plotly::layout, c(list(p), layout_config))

  p_with_layout |>
    plotly::config(
      scrollZoom = FALSE,
      displayModeBar = TRUE,
      doubleClick = "reset",
      modeBarButtonsToRemove = list(
        "zoomIn2d",
        "zoomOut2d",
        "autoScale2d",
        "resetScale2d",
        "toImage",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "lasso2d"
      ),
      displaylogo = FALSE
    ) |>
    htmlwidgets::onRender(
      "
      function(el, x) {
        setupPlotlyEventHandlers(el);
      }
    "
    )
})

shiny::observeEvent(input$plotly_relayout_xrange, {
  ev <- input$plotly_relayout_xrange
  if (is.null(ev)) {
    return()
  }

  # Basic de-dup to avoid needless redraws from repeated relayout events
  xmin_ms <- ev$xmin_ms
  xmax_ms <- ev$xmax_ms
  if (!is.null(xmin_ms) && !is.null(xmax_ms) && !is.na(xmin_ms) && !is.na(xmax_ms)) {
    # Epoch ms does not fit in 32-bit integers; keep as numeric.
    current <- c(round(as.numeric(xmin_ms)), round(as.numeric(xmax_ms)))
    last <- isolate(state$last_relayout_ms)
    if (
      !is.null(last) &&
        length(last) == 2 &&
        !anyNA(last) &&
        !anyNA(current) &&
        all(as.numeric(last) == current)
    ) {
      nav <- if (is.null(ev$nav)) "unknown" else ev$nav
      dbg("relayout (dup) skip nav=", nav)
      return()
    }
    state$last_relayout_ms <- current
    nav <- if (is.null(ev$nav)) "unknown" else ev$nav
    dbg("relayout nav=", nav, " xmin_ms=", current[[1]], " xmax_ms=", current[[2]])
  }

  if (isTRUE(ev$autorange)) {
    dbg("relayout autorange")
    refresh_detail_traces(time_min, time_max)
    return()
  }

  if (is.null(ev$xmin_ms) || is.null(ev$xmax_ms) || is.na(ev$xmin_ms) || is.na(ev$xmax_ms)) {
    return()
  }

  xmin <- as.POSIXct(ev$xmin_ms / 1000, origin = "1970-01-01", tz = time_tz)
  xmax <- as.POSIXct(ev$xmax_ms / 1000, origin = "1970-01-01", tz = time_tz)
  refresh_detail_traces(xmin, xmax)
})
