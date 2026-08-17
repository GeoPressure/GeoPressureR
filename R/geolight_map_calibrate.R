#' @family geolight
#' @rdname geolight_map
#' @export
geolight_map_calibrate <- function(
  tag,
  twl_calib_adjust = 1.4,
  fitted_location_duration = Inf,
  zenith_prior_mean = 93,
  zenith_prior_sd = 1.3,
  zenith_prior_penalty_weight = 1e-5,
  refine_fitted_location_scale_km = 20,
  refine_fitted_location_max_iter = 0,
  quiet = FALSE
) {
  tag_assert(tag, "twilight")
  tag_assert(tag, "stap")
  tag_assert(tag, "setmap")
  assertthat::assert_that(is.logical(quiet), length(quiet) == 1)

  # Fit locations for stationary periods without known locations
  if (!quiet) {
    cli::cli_progress_step("Calibrate zenith angle")
  }
  stap <- geolight_fit_location(
    tag = tag,
    fitted_location_duration = fitted_location_duration,
    extent = tag$param$tag_set_map$extent,
    zenith_prior_mean = zenith_prior_mean,
    zenith_prior_sd = zenith_prior_sd,
    zenith_prior_penalty_weight = zenith_prior_penalty_weight,
    compute_known = FALSE,
    quiet = TRUE
  )

  fitted_idx <- is.finite(stap$lon) &
    is.finite(stap$lat) &
    is.na(stap$known_lon) &
    is.na(stap$known_lat)

  if (refine_fitted_location_max_iter > 0 && any(fitted_idx)) {
    stap_refined <- geolight_refine_location(
      tag = tag,
      path = stap[fitted_idx, ],
      twl_calib_adjust = twl_calib_adjust,
      scale_km = refine_fitted_location_scale_km,
      max_iter = refine_fitted_location_max_iter
    )

    idx <- match(stap_refined$stap_id, stap$stap_id)
    stap$lon[idx] <- stap_refined$lon
    stap$lat[idx] <- stap_refined$lat
    stap$zenith[idx] <- NA_real_
  }

  stap$calib_type <- ifelse(fitted_idx, "fitted", "known")
  stap$known_lat[!is.na(stap$lat)] <- stap$lat[!is.na(stap$lat)]
  stap$known_lon[!is.na(stap$lon)] <- stap$lon[!is.na(stap$lon)]

  twl_calib <- geolight_calibrate(
    twl = tag$twilight,
    calib_stap = stap,
    twl_calib_adjust = twl_calib_adjust
  )

  # Add parameters
  if (!"geolight_map" %in% names(tag$param)) {
    tag$param$geolight_map <- list()
  }
  tag$param$geolight_map$twl_calib_adjust <- twl_calib_adjust
  tag$param$geolight_map$fitted_location_duration <- fitted_location_duration
  tag$param$geolight_map$zenith_prior_mean <- zenith_prior_mean
  tag$param$geolight_map$zenith_prior_sd <- zenith_prior_sd
  tag$param$geolight_map$zenith_prior_penalty_weight <- zenith_prior_penalty_weight
  tag$param$geolight_map$refine_fitted_location_scale_km <- refine_fitted_location_scale_km
  tag$param$geolight_map$refine_fitted_location_max_iter <- refine_fitted_location_max_iter
  tag$param$geolight_map$twl_calib <- twl_calib

  tag
}

#' Calibrate twilight to zenith angle using calibration locations
#' @param twl a twilight data.frame
#' @param calib_stap a stationary period data.frame with calibration locations. Must contain
#' the columns `known_lat`, `known_lon`, `start`, and `end`. This can include both true known
#' locations and estimated (fitted) locations. When produced by `geolight_map_calibrate()`,
#' `calib_type` records whether each calibration anchor is `"known"` or `"fitted"`.
#' @inheritParams geolight_map
#' @return a `twl_calib` object with components:
#' * `x`: the zenith angle sequence
#' * `y`: the estimated density values
#' * `hist_breaks`: common histogram breaks used for all calibration staps
#' * `hist_counts`: list of histogram counts per calibration stationary period
#' * `calib_stap`: calibration stationary periods used for calibration
#'
#' @keywords internal
#' @export
geolight_calibrate <- function(
  twl,
  calib_stap,
  twl_calib_adjust = 1.4
) {
  hard_bounds <- c(60, 120)
  assertthat::assert_that(all(
    c("known_lat", "known_lon", "start", "end") %in% names(calib_stap)
  ))
  assertthat::assert_that("twilight" %in% names(twl))

  # keep only staps with calibration coordinates
  calib_stap <- calib_stap[
    !is.na(calib_stap$known_lat) & !is.na(calib_stap$known_lon),
  ]

  if (nrow(calib_stap) == 0) {
    cli::cli_abort(c(
      "x" = "There are no calibration locations on which to calibrate.",
      ">" = "Provide a known calibration site via {.fun tag_set_map} ({.var known}), or enable automatic calibration via {.var fitted_location_duration}."
    ))
  }

  # Get twilight to include
  twl <- twilight_include(twl)
  twl_include <- twl[twl$include, ]

  # Calibrate twilight errors in terms of solar zenith angle.
  z_calib <- lapply(seq_len(nrow(calib_stap)), function(i) {
    id <- twl_include$twilight >= calib_stap$start[i] &
      twl_include$twilight <= calib_stap$end[i]
    z <- as.numeric(geolight_solar(
      date = twl_include$twilight[id],
      lat = calib_stap$known_lat[i],
      lon = calib_stap$known_lon[i]
    ))

    valid <- is.finite(z) & z >= hard_bounds[1] & z <= hard_bounds[2]
    if (!all(valid)) {
      cli::cli_warn(c(
        "x" = "Some twilight calibration angles in stap {.val {i}} fall outside the hard bounds ({hard_bounds[1]}, {hard_bounds[2]}).",
        ">" = "These values will be ignored for calibration."
      ))
    }
    z[valid]
  })
  names(z_calib) <- as.character(calib_stap$stap_id)
  z_calib_all <- unlist(z_calib, use.names = FALSE)
  if (length(z_calib_all) == 0) {
    cli::cli_abort(c(
      "x" = "There are no usable twilight calibration angles.",
      ">" = "Check that the calibration stationary periods overlap with labeled twilight."
    ))
  }

  z_range <- range(z_calib_all, finite = TRUE)
  pad <- min(max(2, diff(z_range) * 0.1), 5)
  zenith_bounds <- c(z_range[1] - pad, z_range[2] + pad)
  zenith_bounds <- pmax(pmin(zenith_bounds, hard_bounds[2]), hard_bounds[1])

  # Compute the kernel density
  dens <- stats::density(
    z_calib_all,
    adjust = twl_calib_adjust,
    from = zenith_bounds[1],
    to = zenith_bounds[2]
  )

  hist_breaks <- seq(
    floor(zenith_bounds[1] * 2) / 2,
    ceiling(zenith_bounds[2] * 2) / 2,
    by = 0.5
  )
  hist_mids <- (hist_breaks[-1] + hist_breaks[-length(hist_breaks)]) / 2
  hist_counts <- lapply(
    z_calib,
    function(z) {
      z <- z[is.finite(z)]
      tabulate(
        cut(z, breaks = hist_breaks, include.lowest = TRUE, right = TRUE),
        nbins = length(hist_breaks) - 1L
      )
    }
  )
  names(hist_counts) <- names(z_calib)

  # Add parameters
  twl_calib <- list(
    x = dens$x,
    y = dens$y,
    adjust = twl_calib_adjust,
    hist_breaks = hist_breaks,
    hist_mids = hist_mids,
    binwidth = diff(hist_breaks)[1],
    hist_counts = hist_counts,
    calib_stap = calib_stap
  )
  class(twl_calib) <- "twl_calib"

  # return the twlight calibration object
  twl_calib
}
