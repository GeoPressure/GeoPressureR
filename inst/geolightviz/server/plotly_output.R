# Main plotly visualization output

render_plotly_output <- function(
  input,
  output,
  twl,
  stapath,
  drawing,
  is_modifying,
  zoom_state,
  .light_trace
) {
  output$plotly_div <- plotly::renderPlotly({
    twl_ <- twl()
    stapath_ <- stapath()
    idx <- as.numeric(input$stap_id)
    current_range <- c(stapath_$start[idx], stapath_$end[idx])

    # Create base heatmap
    p <- plotly::plot_ly() |>
      plotly::add_trace(
        x = ~ .light_trace$day,
        y = ~ .light_trace$plottime,
        z = ~ .light_trace$value,
        type = "heatmap",
        colorscale = "Greys",
        showscale = FALSE
      )
    y_range <- range(.light_trace$plottime)

    # Add highlighted range
    p <- p |>
      plotly::add_trace(
        x = c(
          current_range[1],
          current_range[2],
          current_range[2],
          current_range[1]
        ),
        y = c(y_range[1], y_range[1], y_range[2], y_range[2]),
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = "rgba(255, 200, 0, 0.3)",
        line = list(color = "transparent"),
        name = "Highlighted Range",
        hoverinfo = "none"
      )

    # Add twilight points
    if (nrow(twl_) > 0) {
      p <- p |>
        plotly::add_trace(
          data = twl_,
          x = ~ as.Date(twilight),
          y = ~plottime,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = ~ ifelse(label != "discard", "yellow", "red"),
            symbol = ~ ifelse(label != "discard", "circle", "x"),
            size = 10
          )
        )
    }

    # Add predicted twilight lines if position exists
    if (!is.na(idx) && idx >= 1 && idx <= nrow(stapath_) &&
      !is.na(stapath_$lat[idx]) && !is.na(stapath_$lon[idx])) {
      twll <- GeoPressureR::path2twilight(stapath_[idx, ])
      twll$plottime <- GeoPressureR:::time2plottime(
        twll$twilight,
        ref = .light_trace$time[1]
      )

      p <- p |>
        plotly::add_trace(
          data = twll,
          x = ~date,
          y = ~plottime,
          split = ~rise,
          type = "scatter",
          mode = "lines"
        )
    }

    # Determine drag mode
    dragmode <- if (!is.null(drawing())) {
      "drawrect"
    } else if (is_modifying()) {
      "select"
    } else {
      "zoom"
    }

    # Apply layout
    p <- p |>
      plotly::layout(
        margin = list(l = 20, r = 0, t = 0, b = 20),
        dragmode = dragmode,
        newshape = list(line = list(color = "blue", width = 2)),
        showlegend = FALSE,
        coloraxis = NULL,
        plot_bgcolor = "black",
        paper_bgcolor = "black",
        font = list(color = "white"),
        xaxis = list(
          title = "Date",
          zeroline = FALSE,
          automargin = TRUE
        ),
        yaxis = list(
          tickformat = "%H:%M",
          title = "Time of Day",
          zeroline = FALSE,
          automargin = TRUE
        )
      ) |>
      plotly::config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d",
          "zoomIn2d",
          "zoomOut2d",
          "resetScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian",
          "toggleSpikelines",
          "toImage"
        )
      )

    # Restore zoom state if it exists
    if (!is.null(zoom_state())) {
      p <- p |>
        plotly::layout(
          xaxis = list(range = zoom_state()$xaxis.range),
          yaxis = list(range = zoom_state()$yaxis.range)
        )
    }

    p
  })
}
