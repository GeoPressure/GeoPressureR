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
    prob = speed2prob(speed, movement)
  )
  lsf <- data.frame(
    low_speed_fix = movement$low_speed_fix
  )

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
      data = d,
      ggplot2::aes(x = .data$speed, y = .data$prob),
      color = "grey"
    ) +
    ggplot2::geom_vline(
      data = lsf,
      ggplot2::aes(xintercept = .data$low_speed_fix),
      color = "red"
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Probability") +
    ggplot2::scale_x_continuous(name = xlab) +
    ggplot2::theme(legend.position = "none")

  if (plot_plotly) {
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  } else {
    return(p)
  }
}
