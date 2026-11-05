#' Print a `bird` object
#'
#' This function displays the information of a `bird` object.
#
#' @param x a GeoPressureR `bird` object.
#' @param ... arguments passed to other methods
#'
#' @return `bird` is returned invisibly and unchanged
#'
#' @examples
#' # Using AVONET dataset
#' bird_create("Acrocephalus arundinaceus")
#'
#' @family bird
#' @method print bird
#' @export
print.bird <- function(x, ...) {
  bird <- x
  cli::cli_h1("GeoPressureR `bird` object")
  cli::cli_bullets(c(
    "*" = "Scientific name: {bird$scientific_name}",
    "*" = "Mass: {signif(bird$mass, 3)} (kg).",
    "*" = "Body frontal area: {signif(bird$body_frontal_area, 3)} (m^2).",
    "*" = "Wing span: {signif(bird$wing_span, 3)} (m).",
    "*" = "Wing aspect: {signif(bird$wing_aspect, 3)} (-)."
  ))
  invisible(bird)
}
