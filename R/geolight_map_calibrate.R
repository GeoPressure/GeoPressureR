#' @family geolight
#' @rdname geolight_map
#' @export
geolight_map_calibrate <- function(
  tag,
  twl_calib_adjust = formals(geolight_map)$twl_calib_adjust
) {
  tag_assert(tag, "twilight")
  tag_assert(tag, "stap")
  assertthat::assert_that(is.numeric(twl_calib_adjust))

  # Compute a kernel density object containing the fit of the distribution of the twilights at the
  # known location
  twl_calib <- geolight_calibration(
    twl = tag$twilight,
    known_stap = tag$stap, # you can send all stap and the function will filter for those needed
    twl_calib_adjust = twl_calib_adjust
  )

  # Add parameters
  if (!"geolight_map" %in% names(tag$param)) {
    tag$param$geolight_map <- list()
  }
  tag$param$geolight_map$twl_calib_adjust <- twl_calib_adjust
  tag$param$geolight_map$wl_calib <- twl_calib

  tag
}

#' Calibrate twilight to zenith angle using known locations
#' @param twl a twilight data.frame
#' @param known_stap a stationary period data.frame with known locations. Must contain
#' the columns `known_lat`, `known_lon`, `start`, and `end`.
#' @inheritParams geolight_map
#' @return a list with components:
#' * `x`: the zenith angle sequence
#' * `y`: the estimated density values
#' * `hist_mids`: the mid points of the histogram bins
#' * `hist_count`: the counts of the histogram bins
#'
#' @noRd
geolight_calibration <- function(
  twl,
  known_stap,
  twl_calib_adjust = formals(geolight_map)$twl_calib_adjust
) {
  assertthat::assert_that(all(
    c("known_lat", "known_lon", "start", "end") %in% names(known_stap)
  ))
  assertthat::assert_that(all(c("twilight", "stap_id") %in% names(twl)))

  # remove any staps without known
  known_stap <- known_stap[
    !is.na(known_stap$known_lat) & !is.na(known_stap$known_lon),
  ]

  if (nrow(known_stap) == 0) {
    cli::cli_abort(c(
      "x" = "There are no known location on which to calibrate in {.var known_stap}.",
      ">" = "Add a the calibration stationary period {.var known} with {.fun tag_set_map}."
    ))
  }

  # Get twilight to include
  twl <- twilight_include(twl)
  twl_include <- twl[twl$include, ]

  # Calibrate the twilight in term of zenith angle with a kernel density.
  z_calib <- c()
  for (i in seq_len(nrow(known_stap))) {
    id <- twl_include$twilight >= known_stap$start[i] &
      twl_include$twilight <= known_stap$end[i]
    sun_calib <- geolight_solar(
      date = twl_include$twilight[id],
      lat = known_stap$known_lat[i],
      lon = known_stap$known_lon[i]
    )
    z_calib <- c(z_calib, as.numeric(sun_calib))
  }

  # Compute the kernel density
  twl_calib <- stats::density(
    z_calib,
    adjust = twl_calib_adjust,
    from = 60,
    to = 120
  )

  # Compute the histogram
  hist_vals <- graphics::hist(z_calib, plot = FALSE)
  twl_calib$hist_count <- hist_vals$density * length(z_calib)
  twl_calib$hist_mids <- hist_vals$mids

  # Add the adjust parameter
  twl_calib$adjust <- twl_calib_adjust

  # return the twlight calibration object
  twl_calib
}
