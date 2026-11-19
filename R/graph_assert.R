#' Assert the status of a `graph`
#'
#' This function checks the condition of a `graph` object.
#'
#' @param graph a GeoPressureR `graph` object
#' @param condition condition to assert `graph` for. One of `"graph"` (default), `"movement"`,
#' or `"full"`.
#'
#' @return `TRUE` if the `graph` object fulfils the condition, otherwise an error is raised.
#' @examples
#' # Load a tag with pressure and light likelihood maps computed with `geopressuretemplate()`
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- load_interim("18LX", var = "tag")
#' })
#' graph <- graph_create(tag, quiet = TRUE)
#' graph_assert(graph)
#'
#' graph <- graph_set_movement(graph)
#' graph_assert(graph, "movement")
#' @family graph
#' @export
graph_assert <- function(graph, condition = "graph") {
  status <- graph_status(graph)

  if (condition == "graph") {
    return(TRUE)
  } else if (condition == "movement") {
    msg <- c(
      "x" = "The `graph` object has no movement model.",
      ">" = "Use {.fun graph_set_movement} to define the movement model."
    )
  } else if (condition == "full") {
    msg <- c(
      "x" = "The `graph` object is empty. No nodes nor edges are left at the construction.",
      ">" = "Check the input and run {.fun graph_create} again."
    )
  } else {
    cli::cli_abort("{.var condition} {.val {condition}} is unknown.")
  }

  if (condition %in% status) {
    return(TRUE)
  }

  cli::cli_abort(msg)
}

#' @noRd
graph_status <- function(graph) {
  assertthat::assert_that(inherits(graph, "graph"))

  assertthat::assert_that(assertthat::has_name(
    graph,
    c(
      "s",
      "t",
      "gs",
      "obs",
      "sz",
      "stap",
      "equipment",
      "retrieval"
    )
  ))

  status <- c()

  if (assertthat::has_name(graph$param, "graph_set_movement")) {
    status <- append(status, "movement")
  }
  if (length(graph$s) > 0) {
    status <- append(status, "full")
  }
  status
}
