#' Compute likelihood map from twilight
#'
#' @description
#' This function estimates a likelihood map for each stationary period based on twilight data.
#' The function performs the following steps:
#'
#' 1. Perform a calibration on the known stationary period. See below for details
#' 2. Compute a likelihood map for each twilight using the calibration.
#' 3. Combine the likelihood maps of all twilights belonging to the same stationary periods with a
#' log-linear pooling. See [GeoPressureManual | Probability aggregation](
#' https://geopressure.org/GeoPressureManual/probability-aggregation.html)
#' for more information on probability aggregation using log-linear pooling.
#'
#' # Calibration
#'
#' Calibration can be based on:
#' - a known location for at least one stationary period (set via `tag_set_map(known = ...)`), and/or
#' - an automatically fitted calibration location (enabled via `fitted_location_duration`).
#'
#' When `fitted_location_duration` is finite, the longest eligible stationary period is selected and
#' its location is estimated from twilight times, then used like a known location for calibration.
#'
#' Instead of calibrating the twilight errors in terms of duration, we directly model the zenith
#' angle error. We use a kernel distribution to fit the zenith angle during the known stationary
#' period(s). The `twl_calib_adjust` parameter allows to manually adjust how smooth you want the
#' fit of the zenith angle to be. Because the zenith angle error model is fitted with data from the
#' calibration site only, and we are using it for all locations of the bird’s journey, it is safer
#' to assume a broader/smoother distribution.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param twl_calib_adjust smoothing parameter for the kernel density (see [`stats::density()`]).
#' @param fitted_location_duration Minimum duration (in days) of stationary period(s) eligible to be
#' used as an automatic (fitted) calibration site estimated from twilight times.
#' If enabled (finite duration), the longest eligible stationary period is selected and its
#' location is estimated from twilight times, then used like a known location for calibration.
#' Default is `Inf` (disabled).
#' @param twl_llp log-linear pooling aggregation weight.
#' @param compute_known logical defining if the map(s) for known stationary period should be
#' estimated based on twilight or hard defined by the known location `stap$known_l**`
#' @param keep_twl logical defining if the likelihood map of each twilight is retained.
#' @param quiet logical to hide messages about the progress
#'
#' @return a `tag` with the likelihood of light as `tag$map_light`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   # Read geolocator data and set map parameters
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 10,
#'       known = data.frame(
#'         stap_id = 1,
#'         known_lon = 17.05,
#'         known_lat = 48.9
#'       )
#'     ) |>
#'     twilight_create() |>
#'     twilight_label_read()
#' })
#'
#' # Visualize twilights
#' plot(tag, type = "twilight")
#'
#' # Calibrate twilight to zenith angle and visualize
#' tag <- geolight_map_calibrate(tag)
#' plot_twl_calib(tag)
#'
#' # Compute likelihood for each twilight and visualize
#' tag <- geolight_map_likelihood(tag)
#' plot(tag, type = "map_light_twl")
#'
#' # Aggregate twilights to stationary periods and visualize
#' tag <- geolight_map_aggregate(tag, quiet = TRUE)
#' plot(tag, type = "map_light")
#'
#' @family geolight
#' @export
geolight_map <- function(
  tag,
  twl_calib_adjust = 1.4,
  fitted_location_duration = Inf,
  twl_llp = \(n) log(n) / n,
  compute_known = FALSE,
  keep_twl = FALSE,
  quiet = FALSE
) {
  tag <- geolight_map_calibrate(
    tag,
    twl_calib_adjust = twl_calib_adjust,
    fitted_location_duration = fitted_location_duration,
    quiet = quiet
  )

  tag <- geolight_map_likelihood(
    tag,
    compute_known = compute_known,
    quiet = quiet
  )

  tag <- geolight_map_aggregate(
    tag,
    twl_llp = twl_llp,
    compute_known = compute_known,
    keep_twl = keep_twl,
    quiet = quiet
  )

  tag
}

#' Get twilight to include in map computation
#' @param x a `tag` object or a twilight data.frame
#' @param only_known logical or NULL. If `NULL`, all twilight are included. If `TRUE`,
#' only twilight associated to known stationary periods are included. If `FALSE`, only
#' twilight associated to unknown stationary periods are included.
#' @return a twilight data.frame with an additional `include` logical column
#' @noRd
twilight_include <- function(twl) {
  # Base inclusion: complete cases
  twl$include <- stats::complete.cases(twl)

  # Remove discarded twilight
  if ("label" %in% names(twl)) {
    twl$include <- twl$include & twl$label != "discard"
  }

  if (!any(twl$include)) {
    cli::cli_abort(c("x" = "There are no twilights left after labeling."))
  }
  twl
}
