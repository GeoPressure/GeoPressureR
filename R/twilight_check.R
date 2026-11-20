#' Check twilights against a path
#'
#' @description
#' Compute, for each twilight of `tag`, the solar zenith angle at the position of its stationary
#' period on `path`, and flag the twilights outside the calibrated zenith range. These are the
#' twilights that make a stationary period orange in `plot_twl_calib(tag, path)`, and the ones whose
#' light likelihood is low at that position.
#'
#' @param tag a GeoPressureR `tag` object with twilights and a twilight calibration (see
#' [`geolight_map_calibrate()`]).
#' @param path a GeoPressureR `path` or `pressurepath` data.frame with `stap_id`, `lat` and `lon`. The
#' first row of each `stap_id` is used.
#' @inheritParams plot_twl_calib
#'
#' @return `tag$twilight` with the columns
#' - `include`: `FALSE` for discarded or incomplete twilights.
#' - `zenith`: solar zenith angle at the position of the stationary period (degrees), `NA` during
#'   flights or when the stationary period has no position.
#' - `outside_ci`: `zenith` is outside the `warning_ci` interval of the calibrated density.
#'
#' The interval (degrees) is returned in the attribute `zenith_ci`.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 4,
#'       known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
#'     ) |>
#'     twilight_create() |>
#'     twilight_label_read() |>
#'     geolight_map_calibrate(quiet = TRUE)
#' })
#'
#' path <- data.frame(stap_id = tag$stap$stap_id, lat = 48.9, lon = 17.05)
#' twl <- twilight_check(tag, path)
#' twl[which(twl$outside_ci & twl$include), ]
#'
#' @family geolight
#' @keywords internal
#' @export
twilight_check <- function(tag, path, warning_ci = 0.95) {
  tag_assert(tag, "twl_calib")
  assertthat::assert_that(assertthat::has_name(tag, "twilight"))
  assertthat::assert_that(assertthat::has_name(tag$twilight, "stap_id"))
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("stap_id", "lat", "lon")))
  assertthat::assert_that(assertthat::is.number(warning_ci), warning_ci > 0, warning_ci < 1)

  # Symmetric interval of the calibrated zenith density
  twl_calib <- tag$param$geolight_map$twl_calib
  alpha <- (1 - warning_ci) / 2
  cdf <- cumsum(twl_calib$y) / sum(twl_calib$y)
  zenith_ci <- stats::approx(cdf, twl_calib$x, xout = c(alpha, 1 - alpha))$y

  # Zenith angle of each twilight at the position of its stationary period (one position per
  # call, as geolight_solar() returns a lat x lon x date grid)
  twl <- twilight_include(tag$twilight)
  k <- match(twl$stap_id, path$stap_id)
  twl$zenith <- NA_real_
  for (j in unique(k[!is.na(path$lat[k]) & !is.na(path$lon[k])])) {
    i <- which(k == j)
    twl$zenith[i] <- as.vector(geolight_solar(twl$twilight[i], path$lat[j], path$lon[j]))
  }
  twl$outside_ci <- twl$zenith < zenith_ci[1] | twl$zenith > zenith_ci[2]
  attr(twl, "zenith_ci") <- zenith_ci
  twl
}
