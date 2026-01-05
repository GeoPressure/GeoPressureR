#' Plot movement model of a `graph`
#'
#' This function display a plot of pressure time series recorded by a tag
#
#' @param graph a GeoPressureR `graph` object or a movement list (from `graph_set_movement`).
#' @param speed Vector of speed value (km/h) used on the x-axis.
#' @param plot_plotly logical to use `plotly`
#'
#' @return a plot or ggplotly object.
#' @examples
#' movement_gamma <- list(
#'   type = "gs",
#'   method = "gamma",
#'   shape = 7,
#'   scale = 7,
#'   low_speed_fix = 15,
#'   zero_speed_ratio = 1
#' )
#' plot_graph_movement(movement_gamma)
#'
#' movement_logis <- list(
#'   type = "gs",
#'   method = "logis",
#'   scale = 7,
#'   location = 40,
#'   low_speed_fix = 15,
#'   zero_speed_ratio = 1
#' )
#' plot_graph_movement(movement_logis)
#'
#' bird <- bird_create("Example bird", mass = 0.1, wing_span = 0.4, wing_aspect = 7)
#' movement_power <- list(
#'   type = "as",
#'   method = "power",
#'   bird = bird,
#'   power2prob = \(power) (1 / power)^3,
#'   low_speed_fix = 15,
#'   zero_speed_ratio = 1
#' )
#' plot_graph_movement(movement_power)
#' @family movement
#' @export
plot_graph_movement <- function(
  graph,
  speed = seq(0, 120),
  plot_plotly = FALSE
) {
  if (inherits(graph, "graph")) {
    graph_assert(graph, "movement")
    movement <- graph$param$graph_set_movement
  } else if (is.list(graph)) {
    movement <- graph
  } else {
    cli::cli_abort("`graph` must be a `graph` object or a movement list.")
  }

  d <- data.frame(
    speed = speed,
    prob = speed2prob(speed, movement),
    line = movement$low_speed_fix == 0 | speed > 0
  )
  max_prob <- max(d$prob, na.rm = TRUE)

  type <- movement$type
  if (is.null(type) && !is.null(movement$method) && movement$method == "power") {
    type <- "as"
  }
  if (!is.null(type) && type == "as") {
    xlab <- "Airspeed [km/h]"
  } else {
    xlab <- "Groundspeed [km/h]"
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = d[d$line, ],
      ggplot2::aes(x = .data$speed, y = .data$prob),
      color = "#2F2F2F",
      linewidth = 1.5
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Probability") +
    ggplot2::scale_x_continuous(name = xlab) +
    ggplot2::theme(legend.position = "none")

  if (movement$low_speed_fix > 0) {
    p <- p +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = movement$low_speed_fix),
        color = "#1B9E77",
        linewidth = 0.7
      ) +
      ggplot2::annotate(
        "text",
        x = movement$low_speed_fix,
        y = max_prob * 0.5,
        label = paste0("low_speed_fix: ", movement$low_speed_fix),
        color = "#1B9E77",
        size = 3,
        hjust = -0.1
      )
  }

  if (movement$zero_speed_ratio > 0) {
    p <- p +
      ggplot2::geom_point(
        data = d[!d$line, ],
        ggplot2::aes(x = .data$speed, y = .data$prob),
        color = "#D95F02",
        size = 3.2
      ) +
      ggplot2::annotate(
        "text",
        x = 0,
        y = max(d$prob[!d$line]),
        label = paste0("zero_speed_ratio: ", movement$zero_speed_ratio),
        color = "#D95F02",
        size = 3,
        hjust = -0.1
      )
  }

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}
