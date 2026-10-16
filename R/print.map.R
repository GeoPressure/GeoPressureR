#' Print a `map` object
#'
#' This function displays the information of a `map` object.
#
#' @param x a GeoPressureR `map` object
#' @param ... arguments passed to other methods
#'
#' @return `map` is returned invisibly and unchanged
#'
#' @examples
#' # Load a tag with pressure and light likelihood maps computed with `geopressuretemplate()`
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- load_interim("18LX", var = "tag")
#' })
#'
#' print(tag$map_pressure)
#'
#' @family map
#' @method print map
#' @export
print.map <- function(x, ...) {
  map <- x

  cli::cli_h1("GeoPressureR `map` object of {.field {map$type}} for {map$id}")

  cli::cli_h3("Map")
  cli::cli_bullets(c(
    "*" = "Extent (W, E, S, N): {.val {map$extent[1]}}\u00b0, {.val {map$extent[2]}}\u00b0, {.val {map$extent[3]}}\u00b0, {.val {map$extent[4]}}\u00b0",
    "*" = "Dimensions (lat x lon): {.val {dim(map)[1]}} x {.val {dim(map)[2]}} (res. {.val {1/map$scale}}\u00b0)"
  ))

  cli::cli_h3("Stationary periods {.field stap} (n={.val {nrow(map$stap)}})")
  cli::cli_text("Run {.code map$stap} to display full table")

  invisible(x)
}
