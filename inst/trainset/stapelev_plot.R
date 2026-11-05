# Stapelev proposal plot, proxy restyling and solar strip. Sourced inside `server()`.

# A one-row heatmap spans an independent, fixed full-height overlay axis.
# Update the background without rebuilding pressure traces.
stapelev_strip_data <- shiny::reactive({
  shiny::req(state$stapelev_proposal, length(state$stapelev_solar_elevation))
  settings <- stapelev_input_settings()
  i <- 1L
  strip <- stapelev_solar_strip(
    state$stapelev_proposal$date,
    state$stapelev_solar_elevation,
    settings$solar_angle[i]
  )
  strip$visible <- TRUE
  strip
})
stapelev_strip_display <- shiny::debounce(stapelev_strip_data, millis = 350)

shiny::observeEvent(
  stapelev_strip_display(),
  {
    shiny::req(state$stapelev_proposal)
    strip <- stapelev_strip_display()
    settings <- stapelev_classification_settings()
    plotly::plotlyProxyInvoke(
      plotly::plotlyProxy("stapelev_proposal_plot", session),
      "restyle",
      list(
        x = list(trainset_format_time(strip$x)),
        z = list(strip$z),
        text = list(strip$text),
        colorscale = list(list(list(0, "rgba(37,58,90,0.08)"), list(1, "rgba(255,255,255,0)"))),
        visible = list(strip$visible)
      ),
      list(length(settings$offsets) * 2L + 2L)
    )
  }
)

stapelev_restyle_levels <- function(settings, difference) {
  proxy <- plotly::plotlyProxy("stapelev_proposal_plot", session)
  fitted_data <- stapelev_fitted_data(state$stapelev_proposal)
  for (i in seq_along(settings$offsets)) {
    level_data <- stapelev_level_data(fitted_data, settings$windows[i, ])
    fitted <- if (difference) {
      rep(settings$offsets[[i]], nrow(level_data))
    } else {
      level_data$surface_pressure + settings$offsets[[i]]
    }
    plotly::plotlyProxyInvoke(
      proxy,
      "restyle",
      list(
        x = list(trainset_format_time(stapelev_ribbon_x(level_data$date))),
        y = list(stapelev_ribbon_y(fitted, settings$range[i]))
      ),
      list((i - 1L) * 2L)
    )
    plotly::plotlyProxyInvoke(
      proxy,
      "restyle",
      list(
        x = list(trainset_format_time(level_data$date)),
        y = list(fitted),
        hovertemplate = list(
          if (difference) {
            "%{x}<br>Offset: %{y:.2f} hPa<extra></extra>"
          } else {
            "%{x}<br>ERA5 fitted: %{y:.2f} hPa<extra></extra>"
          }
        )
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
      hovertemplate = list(
        if (difference) {
          "%{x}<br>Logger − ERA5: %{y:.2f} hPa<br>%{text}<extra></extra>"
        } else {
          "%{x}<br>Logger: %{y:.2f} hPa<br>%{text}<extra></extra>"
        }
      )
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
      rep(settings$offsets[[i]], nrow(level_data))
    } else {
      level_data$surface_pressure + settings$offsets[[i]]
    }
    p <- p |>
      plotly::add_ribbons(
        x = trainset_format_time(level_data$date),
        ymin = fitted - settings$range[i],
        ymax = fitted + settings$range[i],
        name = paste(display_label, "range"),
        line = list(color = "transparent"),
        fillcolor = glue::glue("rgba({rgb[1]}, {rgb[2]}, {rgb[3]}, 0.15)"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) |>
      plotly::add_lines(
        x = trainset_format_time(level_data$date),
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
  hover_label <- glue::glue(
    "{hover_label}<br>Sun elevation: {round(state$stapelev_solar_elevation[i_plot], 1)}°"
  )
  view <- if (difference) "difference" else "pressure"
  p <- p |>
    plotly::add_lines(
      x = trainset_format_time(proposal_plot$date),
      y = logger,
      name = "Logger pressure",
      line = list(color = "#999999", width = 1),
      hoverinfo = "skip"
    ) |>
    plotly::add_markers(
      x = trainset_format_time(proposal_plot$date),
      y = logger,
      text = hover_label,
      name = "Proposed labels",
      marker = list(color = get_marker_colors(proposal_plot$label), size = 5),
      hovertemplate = if (difference) {
        "%{x}<br>Logger − ERA5: %{y:.2f} hPa<br>%{text}<extra></extra>"
      } else {
        "%{x}<br>Logger: %{y:.2f} hPa<br>%{text}<extra></extra>"
      }
    )
  if (length(state$stapelev_solar_elevation) == nrow(proposal)) {
    strip <- shiny::isolate(stapelev_strip_data())
    p <- p |>
      plotly::add_heatmap(
        x = trainset_format_time(strip$x),
        y = 0,
        z = strip$z,
        text = strip$text,
        yaxis = "y2",
        name = "Solar background",
        zmin = 0,
        zmax = 1,
        colorscale = list(list(0, "rgba(37,58,90,0.08)"), list(1, "rgba(255,255,255,0)")),
        visible = strip$visible,
        zsmooth = FALSE,
        showscale = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }
  p |>
    plotly::layout(
      xaxis = list(title = "Time", type = "date"),
      yaxis = list(
        title = if (difference) "Pressure difference (hPa)" else "Pressure (hPa)"
      ),
      yaxis2 = list(
        overlaying = "y",
        range = c(-0.5, 0.5),
        fixedrange = TRUE,
        visible = FALSE
      ),
      hovermode = "closest",
      meta = list(
        stapelev_key = glue::glue("{state$tag$param$id}-{state$stapelev_stap_id}"),
        stapelev_view = view
      ),
      uirevision = glue::glue("stapelev-{state$tag$param$id}-{state$stapelev_stap_id}"),
      showlegend = FALSE,
      margin = list(l = 50, r = 5, t = 25, b = 38)
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
    htmlwidgets::onRender("function(el, x) { setupStapelevRangeBars(el); }") |>
    trainset_compact_plot()
})
