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
#' https://raphaelnussbaumer.com/GeoPressureManual/probability-aggregation.html)
#' for more information on probability aggregation using log-linear pooling.
#'
#' # Calibration
#'
#' Calibration requires to have a known position for a least one stationary periods. Use
#' `tag_set_map()` to define the known position.
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
#' @param twl_llp log-linear pooling aggregation weight.
#' @param compute_known logical defining if the map(s) for known stationary period should be
#' estimated based on twilight or hard defined by the known location `stap$known_l**`
#' @param quiet logical to hide messages about the progress
#'
#' @return a `tag` with the likelihood of light as `tag$map_light`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   # Read geolocator data and build twilight
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
#'     )
#'
#'   # Compute the twilight
#'   tag <- twilight_create(tag) |> twilight_label_read()
#'
#'   # Compute likelihood map
#'   tag <- geolight_map(tag, quiet = TRUE)
#' })
#'
#' plot(tag, type = "map_light")
#'
#'
#' # Calibration kernel fit can be retrieved from
#'
#' twl_calib <- tag$param$geolight_map$twl_calib
#'
#' library(ggplot2)
#'
#' x_lim <- range(twl_calib$x[twl_calib$y > .001 * max(twl_calib$y)])
#'
#' line_data <- data.frame(
#'   x = twl_calib$x,
#'   y = twl_calib$y / max(twl_calib$y) * max(twl_calib$hist_count)
#' )
#'
#' line_data <- line_data[line_data$x >= x_lim[1] & line_data$x <= x_lim[2], ]
#'
#' ggplot() +
#'   geom_bar(aes(x = twl_calib$hist_mids, y = twl_calib$hist_count),
#'     stat = "identity", fill = "lightblue", color = "blue",
#'     width = diff(twl_calib$hist_mids)[1]
#'   ) +
#'   geom_line(data = line_data, aes(x = x, y = y), color = "red", linewidth = 1) +
#'   labs(x = "Solor zenith angle", y = "Count of twilights") +
#'   theme_minimal() +
#'   xlim(x_lim) +
#'   theme(legend.position = "none")
#'
#' @family geolight
#' @export
geolight_map <- function(
  tag,
  twl_calib_adjust = 1.4,
  twl_llp = \(n) log(n) / n,
  compute_known = FALSE,
  quiet = FALSE
) {
  # Check tag
  tag_assert(tag, "setmap")
  tag_assert(tag, "twilight")

  tag <- geolight_map_calibrate(tag, twl_calib_adjust = twl_calib_adjust)

  tag <- geolight_map_twilight(tag, compute_known = compute_known)

  tag <- geolight_map_likelihood(
    tag,
    twl_llp = twl_llp,
    compute_known = compute_known,
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
