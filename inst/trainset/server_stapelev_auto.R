# Manual-offset stapelev proposal workspace. Sourced inside `server()`.

stapelev_classification_settings <- shiny::reactiveVal(NULL)

stapelev_query_label <- function(bundle, i) {
  lat_suffix <- if (bundle$returned_lat >= 0) "N" else "S"
  lon_suffix <- if (bundle$returned_lon >= 0) "E" else "W"
  requested <- format(bundle$requested_at, "%Y-%m-%d %H:%M UTC", tz = "UTC")
  glue::glue(
    "{i} · {sprintf('%.3f', abs(bundle$returned_lat))}° {lat_suffix}, ",
    "{sprintf('%.3f', abs(bundle$returned_lon))}° {lon_suffix} · {requested}"
  )
}

stapelev_current_offsets <- function() {
  vapply(seq_along(state$stapelev_offsets), function(i) {
    value <- input[[paste0("stapelev_offset_", i)]]
    round(if (is.null(value)) state$stapelev_offsets[i] else value, 1)
  }, numeric(1))
}

stapelev_current_windows <- function() {
  windows <- lapply(seq_along(state$stapelev_offsets), function(i) {
    value <- input[[paste0("stapelev_window_", i)]]
    if (is.null(value)) state$stapelev_windows[i, ] else as.numeric(value)
  })
  matrix(unlist(windows), ncol = 2, byrow = TRUE)
}

stapelev_level_label <- function(i) {
  if (i == 1L) "" else paste0("elev_", i - 1L)
}

stapelev_level_name <- function(i) {
  if (i == 1L) "elev_0" else stapelev_level_label(i)
}

stapelev_level_color <- function(i) {
  get_marker_colors(stapelev_level_label(i))[1]
}

stapelev_input_settings <- shiny::reactive({
  list(
    offsets = stapelev_current_offsets(),
    windows = stapelev_current_windows(),
    range = if (is.null(input$stapelev_range)) 2 else input$stapelev_range,
    continuity = if (is.null(input$stapelev_continuity)) 6 else input$stapelev_continuity
  )
})
stapelev_display_settings <- shiny::debounce(stapelev_input_settings, millis = 350)

stapelev_best_offset <- function(x, range) {
  x <- sort(x[is.finite(x)])
  if (!length(x)) {
    return(0)
  }
  end <- findInterval(x + 2 * range, x)
  start <- which.max(end - seq_along(x) + 1L)
  round(mean(x[c(start, end[start])]), 1)
}

compute_stapelev_proposal <- function(query_id) {
  bundle <- state$stapelev_queries[[as.integer(query_id)]]
  idx <- state$stapelev_pressure_idx
  pressure <- state$tag$pressure[idx, , drop = FALSE]
  labels <- reactive_label_pres()[idx]
  era5_idx <- match(as.numeric(pressure$date), as.numeric(bundle$date))
  surface_pressure <- bundle$surface_pressure[era5_idx]

  state$stapelev_proposal <- data.frame(
    date = pressure$date,
    pressure_tag = pressure$value,
    surface_pressure = surface_pressure,
    residual = pressure$value - surface_pressure,
    offset = NA_real_,
    error = NA_real_,
    label = ifelse(labels == "flight", "flight", "discard"),
    stringsAsFactors = FALSE
  )
  state$stapelev_query_id <- as.integer(query_id)
}

stapelev_assign_offsets <- function(proposal, offsets, windows, range, continuity) {
  proposal$offset <- NA_real_
  proposal$error <- NA_real_
  proposal$label[proposal$label != "flight"] <- "discard"
  if (!length(offsets)) {
    return(proposal)
  }

  date_num <- as.numeric(proposal$date)
  in_window <- outer(date_num, windows[, 1], ">=") & outer(date_num, windows[, 2], "<=")
  in_window[, !is.finite(windows[, 1]) | !is.finite(windows[, 2]) | windows[, 1] >= windows[, 2]] <- FALSE
  deviation <- outer(proposal$residual, offsets, "-")
  distance <- abs(deviation)
  distance[!in_window] <- Inf
  nearest <- max.col(-distance, ties.method = "first")
  error <- deviation[cbind(seq_len(nrow(proposal)), nearest)]
  group <- integer(nrow(proposal))
  valid <- proposal$label != "flight" &
    is.finite(distance[cbind(seq_len(nrow(proposal)), nearest)]) & abs(error) <= range
  group[valid] <- nearest[valid]

  if (continuity > 0 && any(group > 0L)) {
    date_diff <- diff(sort(unique(date_num)))
    dt <- if (length(date_diff)) stats::median(date_diff) else 0
    keep <- logical(nrow(proposal))
    for (g in seq_along(offsets)) {
      i <- which(group == g)
      if (!length(i)) next
      start <- c(1L, which(diff(date_num[i]) > 2 * dt) + 1L)
      end <- c(start[-1L] - 1L, length(i))
      for (j in seq_along(start)) {
        run <- i[start[j]:end[j]]
        between <- run[1]:run[length(run)]
        duration <- (date_num[run[length(run)]] - date_num[run[1]] + dt) / 3600
        outliers <- sum(group[between] != g)
        if (duration >= continuity && outliers <= max(1L, floor(0.1 * length(between)))) {
          keep[run] <- TRUE
        }
      }
    }
    group[!keep] <- 0L
  }

  level_labels <- c("", paste0("elev_", seq_along(offsets)[-1L] - 1L))
  proposal$label[group > 0L] <- level_labels[group[group > 0L]]
  proposal$offset[group > 0L] <- offsets[group[group > 0L]]
  proposal$error[group > 0L] <- deviation[cbind(which(group > 0L), group[group > 0L])]
  proposal
}

stapelev_classified_proposal <- shiny::reactive({
  proposal <- state$stapelev_proposal
  settings <- stapelev_classification_settings()
  shiny::req(proposal, settings)
  stapelev_assign_offsets(
    proposal,
    settings$offsets,
    settings$windows,
    settings$range,
    settings$continuity
  )
})

stapelev_new_offset <- function(
  settings,
  window = range(as.numeric(state$stapelev_proposal$date))
) {
  proposal <- stapelev_assign_offsets(
    state$stapelev_proposal,
    settings$offsets,
    settings$windows,
    settings$range,
    settings$continuity
  )
  in_window <- as.numeric(proposal$date) >= window[1] & as.numeric(proposal$date) <= window[2]
  stapelev_best_offset(
    proposal$residual[proposal$label == "discard" & in_window],
    settings$range
  )
}

stapelev_fitted_data <- function(proposal) {
  hour <- floor(as.numeric(proposal$date) / 3600) * 3600
  i <- !duplicated(hour)
  data.frame(
    date = as.POSIXct(hour[i], origin = "1970-01-01", tz = "UTC"),
    surface_pressure = proposal$surface_pressure[i]
  )
}

stapelev_ribbon_y <- function(fitted, range) {
  if (!length(fitted)) return(fitted)
  lower <- fitted - range
  c(lower, lower[length(lower)], rev(fitted + range))
}

stapelev_ribbon_x <- function(date) {
  if (!length(date)) return(date)
  c(date, date[length(date)], rev(date))
}

stapelev_level_data <- function(fitted_data, window) {
  if (!all(is.finite(window))) {
    return(fitted_data[FALSE, ])
  }
  date_num <- as.numeric(fitted_data$date)
  x <- sort(unique(c(
    window[1],
    date_num[date_num > window[1] & date_num < window[2]],
    window[2]
  )))
  data.frame(
    date = as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
    surface_pressure = stats::approx(
      date_num,
      fitted_data$surface_pressure,
      xout = x,
      rule = 2
    )$y
  )
}

stapelev_restyle_levels <- function(settings, difference) {
  proxy <- plotly::plotlyProxy("stapelev_proposal_plot", session)
  fitted_data <- stapelev_fitted_data(state$stapelev_proposal)
  for (i in seq_along(settings$offsets)) {
    level_data <- stapelev_level_data(fitted_data, settings$windows[i, ])
    fitted <- if (difference) {
      rep(settings$offsets[i], nrow(level_data))
    } else {
      level_data$surface_pressure + settings$offsets[i]
    }
    plotly::plotlyProxyInvoke(
      proxy,
      "restyle",
      list(
        x = list(stapelev_ribbon_x(level_data$date)),
        y = list(stapelev_ribbon_y(fitted, settings$range))
      ),
      list((i - 1L) * 2L)
    )
    plotly::plotlyProxyInvoke(
      proxy,
      "restyle",
      list(
        x = list(level_data$date),
        y = list(fitted),
        hovertemplate = list(if (difference) {
          "%{x}<br>Offset: %{y:.2f} hPa<extra></extra>"
        } else {
          "%{x}<br>ERA5 fitted: %{y:.2f} hPa<extra></extra>"
        })
      ),
      list((i - 1L) * 2L + 1L)
    )
  }
}

stapelev_restyle_view <- function(settings, difference) {
  stapelev_restyle_levels(settings, difference)
  proposal <- state$stapelev_proposal
  i <- downsample_indices(nrow(proposal), 10000L)
  logger <- if (difference) {
    proposal$pressure_tag[i] - proposal$surface_pressure[i]
  } else {
    proposal$pressure_tag[i]
  }
  proxy <- plotly::plotlyProxy("stapelev_proposal_plot", session)
  logger_trace <- length(settings$offsets) * 2L
  plotly::plotlyProxyInvoke(proxy, "restyle", list(y = list(logger)), list(logger_trace))
  plotly::plotlyProxyInvoke(
    proxy,
    "restyle",
    list(
      y = list(logger),
      hovertemplate = list(if (difference) {
        "%{x}<br>Logger − ERA5: %{y:.2f} hPa<br>%{text}<extra></extra>"
      } else {
        "%{x}<br>Logger: %{y:.2f} hPa<br>%{text}<extra></extra>"
      })
    ),
    list(logger_trace + 1L)
  )
  plotly::plotlyProxyInvoke(
    proxy,
    "relayout",
    list(
      "yaxis.title.text" = if (difference) "Pressure difference (hPa)" else "Pressure (hPa)",
      "yaxis.autorange" = TRUE,
      meta = list(
        stapelev_key = glue::glue("{state$tag$param$id}-{state$stapelev_stap_id}"),
        stapelev_view = if (difference) "difference" else "pressure"
      )
    )
  )
}

stapelev_draft_save <- function() {
  stap_id <- as.character(state$stapelev_stap_id)
  if (is.null(stap_id) || identical(stap_id, "")) {
    return()
  }
  query_id <- if (is.null(input$stapelev_query_id)) state$stapelev_query_id else as.integer(input$stapelev_query_id)
  settings <- stapelev_input_settings()
  state$stapelev_drafts[[stap_id]] <- list(
    query_requested_at = as.numeric(state$stapelev_queries[[query_id]]$requested_at),
    offsets = settings$offsets,
    windows = settings$windows,
    range = settings$range,
    continuity = settings$continuity,
    live = isTRUE(input$stapelev_live),
    diff_view = isTRUE(input$stapelev_diff_view)
  )
}

show_stapelev_modal <- function() {
  current_stap <- isolate(input$stap_id)
  if (is.null(current_stap) || identical(current_stap, "")) {
    shiny::showNotification(
      "Select a stationary period before opening Auto stapelev.",
      type = "warning",
      duration = 5
    )
    return()
  }

  refresh_stap_state()
  if (!(as.numeric(current_stap) %in% state$stap_data$stap_id)) {
    shiny::showNotification(
      "The selected stationary period changed after recomputing flight boundaries.",
      type = "warning",
      duration = 6
    )
    return()
  }
  idx <- which(state$tag$pressure$stap_id == as.numeric(current_stap))
  bundles <- GeoPressureR:::pressure_query_cache_read(
    state$tag$param$id,
    state$tag$pressure$date[idx]
  )
  if (!length(bundles)) {
    shiny::showNotification(
      "No compatible ERA5 query found. Query this stationary period in GeoPressureViz first.",
      type = "warning",
      duration = 8
    )
    return()
  }

  state$stapelev_stap_id <- as.character(current_stap)
  state$stapelev_queries <- bundles
  state$stapelev_pressure_idx <- idx
  query_choices <- stats::setNames(
    seq_along(bundles),
    vapply(seq_along(bundles), function(i) stapelev_query_label(bundles[[i]], i), character(1))
  )
  draft <- state$stapelev_drafts[[state$stapelev_stap_id]]
  query_id <- 1L
  if (!is.null(draft)) {
    query_id <- match(
      draft$query_requested_at,
      vapply(bundles, function(x) as.numeric(x$requested_at), numeric(1))
    )
    if (is.na(query_id)) query_id <- 1L
  }
  compute_stapelev_proposal(query_id)
  full_window <- range(as.numeric(state$stapelev_proposal$date))
  settings <- if (is.null(draft)) {
    list(
      offsets = stapelev_best_offset(
        state$stapelev_proposal$residual[state$stapelev_proposal$label != "flight"],
        2
      ),
      windows = matrix(full_window, nrow = 1),
      range = 2,
      continuity = 6
    )
  } else {
    draft[c("offsets", "windows", "range", "continuity")]
  }
  if (is.null(settings$windows) || nrow(settings$windows) != length(settings$offsets)) {
    settings$windows <- matrix(rep(full_window, length(settings$offsets)), ncol = 2, byrow = TRUE)
  }
  state$stapelev_offsets <- settings$offsets
  state$stapelev_windows <- settings$windows
  stapelev_classification_settings(settings)
  live <- if (is.null(draft)) length(idx) <= 10000L else draft$live
  difference <- if (is.null(draft) || is.null(draft$diff_view)) FALSE else draft$diff_view

  modal <- shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-center justify-content-between w-100",
      shiny::span(glue::glue("Automatic stapelev proposal — Stap {current_stap}")),
      shiny::tags$button(
        type = "button",
        class = "btn-close",
        title = "Close",
        onclick = "Shiny.setInputValue('stapelev_cancel', Date.now(), {priority: 'event'});"
      )
    ),
    size = "xl",
    easyClose = FALSE,
    fade = FALSE,
    footer = NULL,
    shiny::div(
      class = "row g-0 stapelev-workspace",
      shiny::div(
        class = "col-lg-9 stapelev-plot-column p-3",
        plotly::plotlyOutput("stapelev_proposal_plot", width = "100%", height = "100%")
      ),
      shiny::div(
        class = "col-lg-3 stapelev-controls d-flex flex-column p-4",
        shiny::div(
          class = "stapelev-control-content",
          shiny::selectInput(
            "stapelev_query_id",
            "ERA5 query",
            query_choices,
            selected = as.character(query_id),
            width = "100%"
          ),
          shiny::div(
            class = "stapelev-switch stapelev-view-switch mb-3",
            shiny::checkboxInput("stapelev_diff_view", "Pressure difference view", value = difference)
          ),
          shiny::uiOutput("stapelev_offset_controls"),
          shiny::div(
            class = "stapelev-inline-input mb-3",
            shiny::tags$label("Range (hPa)"),
            shiny::numericInput("stapelev_range", NULL, value = settings$range, min = 0.1, step = 0.1)
          ),
          shiny::div(
            class = "stapelev-inline-input mb-3",
            shiny::tags$label("Minimum duration (h)"),
            shiny::numericInput(
              "stapelev_continuity",
              NULL,
              value = settings$continuity,
              min = 0,
              step = 1
            )
          ),
          shiny::div(
            class = "stapelev-switch stapelev-update-controls d-flex align-items-center gap-2 mb-3",
            shiny::checkboxInput("stapelev_live", "Live update", value = live),
            shiny::conditionalPanel(
              condition = "input.stapelev_live === false",
              shiny::actionButton(
                "stapelev_update",
                "Update",
                icon = shiny::icon("rotate"),
                class = "btn btn-outline-primary btn-sm"
              )
            )
          )
        ),
        shiny::div(
          class = "stapelev-actions mt-auto pt-3",
          shiny::actionButton(
            "stapelev_cancel",
            "Cancel",
            class = "btn btn-secondary btn-sm stapelev-cancel"
          ),
          shiny::actionButton(
            "stapelev_apply",
            "Apply proposal",
            icon = shiny::icon("check"),
            class = "btn btn-success"
          )
        )
      )
    )
  )
  modal$attribs$class <- paste(modal$attribs$class, "stapelev-modal")
  shiny::showModal(modal)
}

output$stapelev_offset_controls <- shiny::renderUI({
  offsets <- state$stapelev_offsets
  shiny::tagList(
    shiny::tags$label(class = "form-label", "Elevation offsets (hPa)"),
    lapply(seq_along(offsets), function(i) {
      label <- stapelev_level_name(i)
      color <- stapelev_level_color(i)
      shiny::div(
        class = "stapelev-level-control",
        shiny::div(
          class = "stapelev-offset-row",
          shiny::span(class = "label-dot", style = glue::glue("background-color: {color}")),
          shiny::span(class = "stapelev-offset-label", label),
          shiny::numericInput(
            paste0("stapelev_offset_", i),
            label = NULL,
            value = offsets[i],
            step = 0.1,
            width = "100%"
          ),
          shiny::tags$button(
            id = paste0("stapelev_range_btn_", i),
            type = "button",
            class = "btn btn-outline-secondary btn-sm stapelev-range-btn",
            title = glue::glue("Set the time range for {label}"),
            onclick = glue::glue(
              "Shiny.setInputValue('stapelev_range_select', {i}, {{priority: 'event'}});"
            ),
            shiny::icon("arrows-left-right")
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-outline-secondary btn-sm",
            title = glue::glue("Automatically fit {label}"),
            onclick = glue::glue(
              "Shiny.setInputValue('stapelev_refit', {i}, {{priority: 'event'}});"
            ),
            shiny::icon("rotate")
          ),
          if (i > 1L) shiny::tags$button(
            type = "button",
            class = "btn btn-outline-danger btn-sm",
            title = glue::glue("Remove {label}"),
            onclick = glue::glue(
              "Shiny.setInputValue('stapelev_delete', {i}, {{priority: 'event'}});"
            ),
            shiny::icon("trash")
          ) else shiny::span(class = "stapelev-delete-placeholder")
        )
      )
    }),
    shiny::actionButton(
      "stapelev_add",
      "Add elevation level",
      icon = shiny::icon("plus"),
      class = "btn btn-outline-primary btn-sm w-100 mb-3"
    )
  )
})

output$stapelev_proposal_plot <- plotly::renderPlotly({
  proposal <- stapelev_classified_proposal()
  settings <- stapelev_classification_settings()
  fitted_data <- stapelev_fitted_data(proposal)
  i_plot <- downsample_indices(nrow(proposal), 10000L)
  proposal_plot <- proposal[i_plot, , drop = FALSE]
  difference <- shiny::isolate(isTRUE(input$stapelev_diff_view))

  p <- plotly::plot_ly()
  for (i in seq_along(settings$offsets)) {
    display_label <- stapelev_level_name(i)
    color <- stapelev_level_color(i)
    rgb <- grDevices::col2rgb(color)
    level_data <- stapelev_level_data(fitted_data, settings$windows[i, ])
    fitted <- if (difference) {
      rep(settings$offsets[i], nrow(level_data))
    } else {
      level_data$surface_pressure + settings$offsets[i]
    }
    p <- p |>
      plotly::add_ribbons(
        x = level_data$date,
        ymin = fitted - settings$range,
        ymax = fitted + settings$range,
        name = paste(display_label, "range"),
        line = list(color = "transparent"),
        fillcolor = glue::glue("rgba({rgb[1]}, {rgb[2]}, {rgb[3]}, 0.15)"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) |>
      plotly::add_lines(
        x = level_data$date,
        y = fitted,
        name = display_label,
        line = list(color = color, width = 1),
        hovertemplate = if (difference) {
          "%{x}<br>Offset: %{y:.2f} hPa<extra></extra>"
        } else {
          "%{x}<br>ERA5 fitted: %{y:.2f} hPa<extra></extra>"
        }
      )
  }

  logger <- if (difference) {
    proposal_plot$pressure_tag - proposal_plot$surface_pressure
  } else {
    proposal_plot$pressure_tag
  }
  hover_label <- ifelse(proposal_plot$label == "", "elev_0", proposal_plot$label)
  view <- if (difference) "difference" else "pressure"
  p |>
    plotly::add_lines(
      x = proposal_plot$date,
      y = logger,
      name = "Logger pressure",
      line = list(color = "#999999", width = 1),
      hoverinfo = "skip"
    ) |>
    plotly::add_markers(
      x = proposal_plot$date,
      y = logger,
      text = hover_label,
      name = "Proposed labels",
      marker = list(color = get_marker_colors(proposal_plot$label), size = 5),
      hovertemplate = if (difference) {
        "%{x}<br>Logger − ERA5: %{y:.2f} hPa<br>%{text}<extra></extra>"
      } else {
        "%{x}<br>Logger: %{y:.2f} hPa<br>%{text}<extra></extra>"
      }
    ) |>
    plotly::layout(
      xaxis = list(title = "Time"),
      yaxis = list(title = if (difference) "Pressure difference (hPa)" else "Pressure (hPa)"),
      hovermode = "closest",
      meta = list(
        stapelev_key = glue::glue("{state$tag$param$id}-{state$stapelev_stap_id}"),
        stapelev_view = view
      ),
      uirevision = glue::glue("stapelev-{state$tag$param$id}-{state$stapelev_stap_id}"),
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.08, yanchor = "bottom"),
      margin = list(l = 65, r = 20, t = 75, b = 55)
    ) |>
    plotly::config(
      scrollZoom = FALSE,
      displayModeBar = TRUE,
      doubleClick = "reset",
      modeBarButtonsToRemove = list(
        "zoomIn2d",
        "zoomOut2d",
        "resetScale2d",
        "toImage",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "lasso2d",
        "select2d"
      ),
      displaylogo = FALSE
    ) |>
    htmlwidgets::onRender("function(el, x) { setupStapelevRangeBars(el); }")
})

shiny::observeEvent(input$stapelev_auto_btn, {
  show_stapelev_modal()
})

shiny::observeEvent(stapelev_display_settings(), {
  if (isTRUE(input$stapelev_live) && !is.null(state$stapelev_proposal)) {
    stapelev_classification_settings(stapelev_display_settings())
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$stapelev_live, {
  if (isTRUE(input$stapelev_live) && !is.null(state$stapelev_proposal)) {
    stapelev_classification_settings(stapelev_input_settings())
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$stapelev_update, {
  stapelev_classification_settings(stapelev_input_settings())
})

shiny::observeEvent(stapelev_display_settings(), {
  if (!isTRUE(input$stapelev_live) && !is.null(state$stapelev_proposal)) {
    settings <- stapelev_display_settings()
    committed <- stapelev_classification_settings()
    if (length(settings$offsets) == length(committed$offsets)) {
      stapelev_restyle_levels(settings, isTRUE(input$stapelev_diff_view))
    }
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$stapelev_diff_view, {
  if (!is.null(state$stapelev_proposal)) {
    stapelev_restyle_view(stapelev_input_settings(), isTRUE(input$stapelev_diff_view))
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$stapelev_range_select, {
  session$sendCustomMessage("stapelevRangeSelect", as.integer(input$stapelev_range_select))
})

shiny::observeEvent(input$stapelev_range_selection, {
  i <- as.integer(input$stapelev_range_selection$level)
  window <- sort(c(
    input$stapelev_range_selection$start_ms,
    input$stapelev_range_selection$end_ms
  )) / 1000
  proposal_time <- as.numeric(state$stapelev_proposal$date)
  time_range <- range(proposal_time)
  time_step <- stats::median(diff(sort(unique(proposal_time))))
  window <- pmin(pmax(window, time_range[1]), time_range[2])
  window <- round((window - time_range[1]) / time_step) * time_step + time_range[1]
  windows <- state$stapelev_windows
  windows[i, ] <- window
  state$stapelev_windows <- windows
  settings <- stapelev_input_settings()
  settings$windows <- windows
  if (isTRUE(input$stapelev_live)) {
    stapelev_classification_settings(settings)
  } else {
    stapelev_restyle_levels(settings, isTRUE(input$stapelev_diff_view))
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$stapelev_add, {
  settings <- stapelev_input_settings()
  settings$offsets <- c(settings$offsets, stapelev_new_offset(settings))
  settings$windows <- rbind(settings$windows, range(as.numeric(state$stapelev_proposal$date)))
  state$stapelev_offsets <- settings$offsets
  state$stapelev_windows <- settings$windows
  stapelev_classification_settings(settings)
})

shiny::observeEvent(input$stapelev_refit, {
  settings <- stapelev_input_settings()
  i <- as.integer(input$stapelev_refit)
  remaining <- settings
  remaining$offsets <- settings$offsets[-i]
  remaining$windows <- settings$windows[-i, , drop = FALSE]
  settings$offsets[i] <- stapelev_new_offset(remaining, settings$windows[i, ])
  state$stapelev_offsets <- settings$offsets
  stapelev_classification_settings(settings)
})

shiny::observeEvent(input$stapelev_delete, {
  settings <- stapelev_input_settings()
  i <- as.integer(input$stapelev_delete)
  settings$offsets <- settings$offsets[-i]
  settings$windows <- settings$windows[-i, , drop = FALSE]
  state$stapelev_offsets <- settings$offsets
  state$stapelev_windows <- settings$windows
  stapelev_classification_settings(settings)
})

shiny::observeEvent(input$stapelev_query_id, {
  if (!is.null(state$stapelev_proposal)) {
    compute_stapelev_proposal(input$stapelev_query_id)
    stapelev_classification_settings(stapelev_input_settings())
  }
}, ignoreInit = TRUE)

shiny::observe({
  current_stap <- input$stap_id
  if (is.null(current_stap) || identical(current_stap, "") || !has_pressure) {
    shinyjs::hide("stapelev_auto_btn")
    return()
  }
  idx <- which(state$tag$pressure$stap_id == as.numeric(current_stap))
  bundles <- GeoPressureR:::pressure_query_cache_read(state$tag$param$id, state$tag$pressure$date[idx])
  if (length(bundles)) shinyjs::show("stapelev_auto_btn") else shinyjs::hide("stapelev_auto_btn")
})

shiny::observeEvent(input$stapelev_cancel, {
  stapelev_draft_save()
  state$stapelev_proposal <- NULL
  shiny::removeModal()
})

shiny::observeEvent(input$stapelev_apply, {
  settings <- stapelev_input_settings()
  stapelev_classification_settings(settings)
  proposal <- stapelev_assign_offsets(
    state$stapelev_proposal,
    settings$offsets,
    settings$windows,
    settings$range,
    settings$continuity
  )
  idx <- state$stapelev_pressure_idx
  shiny::req(proposal, length(idx) == nrow(proposal))

  labels <- reactive_label_pres()
  editable <- labels[idx] != "flight"
  state$stapelev_undo <- list(idx = idx[editable], label = labels[idx][editable])
  labels[idx[editable]] <- proposal$label[editable]
  reactive_label_pres(labels)
  state$labels_dirty <- TRUE
  stapelev_draft_save()

  elev <- grep("^elev_\\d+$", proposal$label, value = TRUE)
  if (length(elev)) {
    stap_elev_count(max(stap_elev_count(), as.integer(sub("elev_", "", elev))))
  }
  shinyjs::show("undo_stapelev_btn")
  refresh_detail_traces(state$view_xmin, state$view_xmax)
  state$stapelev_proposal <- NULL
  shiny::removeModal()
  shiny::showNotification("Stapelev proposal applied.", type = "message", duration = 4)
})

shiny::observeEvent(input$undo_stapelev_btn, {
  undo <- state$stapelev_undo
  if (is.null(undo)) {
    return()
  }
  labels <- reactive_label_pres()
  labels[undo$idx] <- undo$label
  reactive_label_pres(labels)
  state$labels_dirty <- TRUE
  state$stapelev_undo <- NULL
  shinyjs::hide("undo_stapelev_btn")
  refresh_detail_traces(state$view_xmin, state$view_xmax)
  shiny::showNotification("Automatic stapelev labels undone.", type = "message", duration = 3)
})
