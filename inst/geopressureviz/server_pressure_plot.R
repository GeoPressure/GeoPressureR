# Pressure time series plot. Sourced inside `server()`.

output$pressure_plot <- plotly::renderPlotly({
  # Plotting too many discarded points makes Plotly very slow, so cap them.
  max_discard_points <- 5000L
  disc <- subset(pressure, label == "discard")
  if (nrow(disc) > max_discard_points) {
    disc <- disc[round(seq(1, nrow(disc), length.out = max_discard_points)), , drop = FALSE]
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = pressure,
      ggplot2::aes(x = date, y = value),
      colour = "grey"
    ) +
    ggplot2::geom_point(
      data = disc,
      ggplot2::aes(x = date, y = value),
      colour = "black"
    ) +
    ggplot2::theme_bw()

  if (nrow(reactVal$pressurepath) > 0) {
    pp <- reactVal$pressurepath
    group_var <- if ("stap_id" %in% names(pp)) {
      "stap_id"
    } else if ("stap_ref" %in% names(pp)) {
      "stap_ref"
    } else {
      NULL
    }

    y_var <- if ("surface_pressure_norm" %in% names(pp)) {
      "surface_pressure_norm"
    } else if ("surface_pressure" %in% names(pp)) {
      "surface_pressure"
    } else {
      NULL
    }

    has_linetype <- "linetype" %in% names(pp)

    p <- p +
      ggplot2::geom_line(
        data = pp,
        mapping = if (!is.null(group_var) && !is.null(y_var) && isTRUE(has_linetype)) {
          ggplot2::aes(
            x = .data$date,
            y = .data[[y_var]],
            color = .data$col,
            group = .data[[group_var]],
            linetype = .data$linetype
          )
        } else if (!is.null(group_var) && !is.null(y_var)) {
          ggplot2::aes(
            x = .data$date,
            y = .data[[y_var]],
            color = .data$col,
            group = .data[[group_var]]
          )
        } else if (!is.null(y_var)) {
          ggplot2::aes(
            x = .data$date,
            y = .data[[y_var]],
            color = .data$col
          )
        } else {
          ggplot2::aes(
            x = .data$date,
            y = .data$value,
            color = .data$col
          )
        }
      ) +
      ggplot2::scale_color_identity()
  }

  tooltip_vars <- if (
    isTRUE(nrow(reactVal$pressurepath) > 0) && "linetype" %in% names(reactVal$pressurepath)
  ) {
    c("x", "y", "linetype")
  } else {
    c("x", "y")
  }

  plotly::ggplotly(
    p,
    dynamicTicks = TRUE,
    height = 300,
    tooltip = tooltip_vars,
    source = "pressure_plot"
  ) |>
    plotly::layout(
      showlegend = FALSE,
      uirevision = "pressure-plot",
      yaxis = list(title = "Pressure [hPa]")
    ) |>
    plotly::config(
      scrollZoom = FALSE, # Disable built-in scroll zoom
      displayModeBar = TRUE,
      # doubleClick = "reset", # Reset zoom on double click
      modeBarButtonsToRemove = list(
        "zoomIn2d",
        "zoomOut2d",
        # "autoScale2d",
        # "resetScale2d",
        "toImage",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "select2d",
        "lasso2d"
      ),
      displaylogo = FALSE
    )
})

# Pressure Graph
shiny::observe({
  if (!input$full_track) {
    stap_id <- stap$stap_id[as.numeric(input$stap_id)]
    pressure_val_stap_id <- pressure$value[pressure$stap_id == stap_id]
    plotly::plotlyProxy("pressure_plot", session) |>
      plotly::plotlyProxyInvoke(
        "relayout",
        list(
          yaxis = list(
            range = c(
              min(pressure_val_stap_id, na.rm = TRUE) - 5,
              max(pressure_val_stap_id, na.rm = TRUE) + 5
            )
          ),
          xaxis = list(
            range = c(
              stap$start[as.numeric(input$stap_id)] - 60 * 60 * 24,
              stap$end[as.numeric(input$stap_id)] + 60 * 60 * 24
            )
          )
        )
      )
  } else {
    plotly::plotlyProxy("pressure_plot", session) |>
      plotly::plotlyProxyInvoke(
        "relayout",
        list(
          yaxis = list(autorange = TRUE),
          xaxis = list(autorange = TRUE)
        )
      )
  }
})
