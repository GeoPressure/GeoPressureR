.DEG2RAD <- pi / 180
#' Estimate one location per stationary period from twilight times
#'
#' @description
#' For each stationary period (`stap_id`) with usable twilight
#' events, estimate a single longitude, latitude, and effective zenith angle by
#' minimizing the squared spherical distance to the corresponding iso-zenith
#' small circles.
#'
#' Each twilight defines a small circle on the Earth:
#' the set of locations at a fixed angular distance (zenith angle) from the
#' subsolar point at that time. The fitted location is the point that is, in a
#' least-squares sense, closest to all those circles simultaneously.
#'
#' @param tag A GeoPressureR tag object containing `tag$twilight` and `tag$stap`.
#' @param fitted_location_duration Minimum duration (in days) of stationary period(s) eligible to be
#' used as a fitted location from twilight times. Default is `Inf` (disabled).
#' @param extent Numeric vector `c(W, E, S, N)` in degrees
#'   (longitude west/east, latitude south/north).
#' @param zenith_init Initial zenith angle (degrees).
#' @param zenith_bounds Numeric vector of length 2 giving zenith bounds (degrees).
#' @param compute_known Logical; if FALSE, known stationary periods are copied from `tag$stap`
#'   (rather than being estimated).
#' @param quiet Logical; if TRUE, suppress informative messages.
#'
#' @return A `path` data.frame derived from `tag$stap` with added columns:
#'   `lon`, `lat`, `zenith`. Rows where fitting was not possible remain `NA`.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read()
#' })
#' tag <- tag_stap_daily(tag, quiet = TRUE)
#' path <- geolight_fit_location(tag, fitted_location_duration = 5, quiet = TRUE)
#'
#' @family geolight
#' @export
geolight_fit_location <- function(
  tag,
  fitted_location_duration = Inf,
  extent = NULL,
  zenith_init = 96,
  zenith_bounds = c(60, 120),
  compute_known = FALSE,
  quiet = FALSE
) {
  tag_assert(tag, "twilight")
  tag_assert(tag, "stap")

  if (is.null(extent)) {
    if (
      assertthat::has_name(tag$param, "tag_set_map") &&
        assertthat::has_name(tag$param$tag_set_map, "extent")
    ) {
      extent <- tag$param$tag_set_map$extent
    } else {
      extent <- c(-180, 180, -90, 90)
    }
  }

  if (is.null(fitted_location_duration)) {
    fitted_location_duration <- Inf
  }
  if (!is.numeric(fitted_location_duration)) {
    fitted_location_duration <- suppressWarnings(as.numeric(unlist(fitted_location_duration)))
  }
  assertthat::assert_that(
    is.numeric(fitted_location_duration),
    length(fitted_location_duration) == 1,
    !is.na(fitted_location_duration)
  )
  assertthat::assert_that(is.numeric(extent), length(extent) == 4L)
  assertthat::assert_that(is.numeric(zenith_bounds), length(zenith_bounds) == 2L)

  # Twilight table and inclusion mask
  twl <- twilight_include(tag$twilight)
  assertthat::assert_that(nrow(twl) > 0)

  # Add stationary period for each twilight in case not yet computed
  twl$stap_id <- find_stap(tag$stap, twl$twilight)

  # Split twilight row indices by stationary period
  twl_id_by_stap <- split(which(twl$include), twl$stap_id[twl$include])

  # Eligible stationary periods
  eligible <- stap2duration(tag$stap) >= fitted_location_duration
  if ("include" %in% names(tag$stap)) {
    eligible <- eligible & tag$stap$include
  }
  if (!compute_known) {
    eligible <- eligible &
      is.na(tag$stap$known_lat) &
      is.na(tag$stap$known_lon)
  }

  # Only keep eligible staps for fitting
  twl_id_by_stap <- twl_id_by_stap[names(twl_id_by_stap) %in% tag$stap$stap_id[eligible]]

  # Keep only periods with at least one twilight
  twl_id_by_stap <- twl_id_by_stap[lengths(twl_id_by_stap) > 0]

  # Precompute solar quantities for all twilight times once
  sun_all <- geolight_solar_constants(twl$twilight)

  # Initial parameters (lon, lat, zenith)
  par_init <- c(
    lon = mean(c(extent[1], extent[2])),
    lat = mean(c(extent[3], extent[4])),
    zenith = zenith_init
  )

  lower <- c(extent[1], extent[3], zenith_bounds[1])
  upper <- c(extent[2], extent[4], zenith_bounds[2])
  names(lower) <- names(upper) <- c("lon", "lat", "zenith")

  # Prepare output table
  path <- tag$stap[, c("stap_id", "start", "end", "known_lon", "known_lat")]
  path$lon <- NA_real_
  path$lat <- NA_real_
  path$zenith <- NA_real_

  for (k in seq_along(twl_id_by_stap)) {
    stap_id <- names(twl_id_by_stap)[k]
    idx <- twl_id_by_stap[[k]]

    # Match stap_id to row in path table
    path_row <- match(as.numeric(stap_id), path$stap_id)

    # Subset solar constants to twilights of this stationary period
    sun_i <- sun_all[idx, ]

    fit <- stats::optim(
      par = par_init,
      fn = geolight_fit_location_objective,
      sun = sun_i,
      method = "L-BFGS-B",
      lower = lower,
      upper = upper
    )

    if (!quiet && !is.null(fit$convergence) && fit$convergence != 0L) {
      cli::cli_warn(
        "Location optimisation failed for stap_id {stap_id} (convergence = {fit$convergence})."
      )
    }

    # parameter at bounds
    tol <- 1e-8
    at_lower <- abs(fit$par - lower) < tol
    at_upper <- abs(fit$par - upper) < tol

    if (!quiet && any(at_lower | at_upper)) {
      hit <- names(fit$par)[at_lower | at_upper]
      cli::cli_warn(
        "Location fit for stap_id {stap_id} hit parameter bounds: {paste(hit, collapse = ', ')}."
      )
    }

    path$lon[path_row] <- fit$par[1]
    path$lat[path_row] <- fit$par[2]
    path$zenith[path_row] <- fit$par[3]
  }

  if (!compute_known) {
    known_idx <- which(
      !is.na(tag$stap$known_lat) & !is.na(tag$stap$known_lon)
    )
    for (i in known_idx) {
      path$lon[path$stap_id == tag$stap$stap_id[i]] <- tag$stap$known_lon[i]
      path$lat[path$stap_id == tag$stap$stap_id[i]] <- tag$stap$known_lat[i]
      path$zenith[path$stap_id == tag$stap$stap_id[i]] <- NA_real_
    }
  }

  path
}

#' Objective function: distance to iso-zenith small circles
#'
#' @description
#' For each twilight time, the Sun defines a subsolar point on Earth.
#' For a given zenith angle `zenith`, the set of locations where the Sun
#' has that zenith is a *small circle* centered on the subsolar point with
#' angular radius equal to `zenith`.
#'
#' This objective computes, for a candidate location (lon, lat), the
#' great-circle angular distance to each subsolar point, subtracts the
#' small-circle radius, and sums the squared residuals:
#'
#'   sum_i ( distance(candidate, subsolar_i) - zenith )^2
#'
#' Minimizing this quantity finds the point that is, in a least-squares
#' sense, closest to all twilight curves simultaneously.
#'
#' @param par Numeric vector `c(lon, lat, zenith)` in **degrees**.
#' @param sun data.frame returned by `geolight_solar_constants()`,
#'   already subset to the twilight rows being fitted.
#'
#' @return Numeric scalar: sum of squared distances (radians^2).
#' @noRd
geolight_fit_location_objective <- function(par, sun) {
  # ---- unpack parameters (degrees) ----
  lon_deg <- par[1]
  lat_deg <- par[2]
  zen_deg <- par[3]

  # ---- convert candidate location to radians ----
  lon_r <- lon_deg * .DEG2RAD
  lat_r <- lat_deg * .DEG2RAD

  # Small-circle radius (zenith angle) in radians
  rho <- zen_deg * .DEG2RAD

  # ---- subsolar points for each twilight (radians) ----
  # solar declination is already encoded as sin(delta)
  lat_s <- asin(sun$sin_solar_dec)

  # subsolar longitude (degrees east), converted to radians
  lon_s <- (180 - sun$solar_time) * .DEG2RAD

  # ---- longitude difference with wrap-around ----
  # Using atan2(sin, cos) avoids dateline discontinuities
  dlon <- atan2(
    sin(lon_r - lon_s),
    cos(lon_r - lon_s)
  )

  # ---- great-circle angular distance candidate -> subsolar ----
  cos_gamma <-
    sin(lat_r) * sin(lat_s) + cos(lat_r) * cos(lat_s) * cos(dlon)

  # numerical safety for acos
  cos_gamma <- pmin(1, pmax(-1, cos_gamma))
  gamma <- acos(cos_gamma) # radians

  # ---- signed distance to small circle ----
  # positive if candidate is outside the circle,
  # negative if inside
  d <- gamma - rho

  # ---- objective: sum of squared distances ----
  sum(d * d)
}
