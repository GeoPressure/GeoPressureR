#' Refine fitted light-calibration locations
#'
#' Starting from fitted calibration anchors, refine each anchor with twilights from the same
#' stationary period only. Each iteration calibrates a stap-specific zenith-error density at the
#' current anchor, scores a local anisotropic grid, and moves the anchor to the local
#' light-likelihood mode. This is used only before global twilight calibration.
#'
#' @param tag A GeoPressureR tag object with twilight, stap, and map settings.
#' @param path A path data.frame containing `stap_id`, `lon`, and `lat`; all columns are preserved.
#' @param twl_calib_adjust Smoothing parameter passed to `stats::density()`.
#' @param scale_km Target grid resolution and movement threshold for convergence, in kilometers.
#' @param max_iter Maximum number of refinement iterations per anchor.
#' @return `path` with refined `lon` and `lat`.
#' @noRd
geolight_refine_location <- function(
  tag,
  path,
  twl_calib_adjust = 1.4,
  scale_km = 20,
  max_iter = 2
) {
  tag_assert(tag, "twilight")
  tag_assert(tag, "stap")
  tag_assert(tag, "setmap")
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("stap_id", "lon", "lat")))
  assertthat::assert_that(is.numeric(twl_calib_adjust), length(twl_calib_adjust) == 1L)
  assertthat::assert_that(is.numeric(scale_km), length(scale_km) == 1L, scale_km > 0)
  assertthat::assert_that(is.numeric(max_iter), length(max_iter) == 1L, max_iter >= 1)

  extent <- tag$param$tag_set_map$extent

  twl <- twilight_include(tag$twilight)
  twl$stap_id <- find_stap(tag$stap, twl$twilight)
  sun_all <- geolight_solar_constants(twl$twilight)

  out <- path

  refine_idx <- which(
    is.finite(out$lon) &
      is.finite(out$lat) &
      out$stap_id %in% twl$stap_id[twl$include]
  )

  for (ipath in refine_idx) {
    idx <- which(twl$include & twl$stap_id == out$stap_id[ipath])
    refined <- geolight_refine_location_one(
      lon = out$lon[ipath],
      lat = out$lat[ipath],
      twl = twl[idx, ],
      sun = sun_all[idx, , drop = FALSE],
      extent = extent,
      map_scale = tag$param$tag_set_map$scale,
      twl_calib_adjust = twl_calib_adjust,
      scale_km = scale_km,
      max_iter = as.integer(max_iter)
    )

    out$lon[ipath] <- refined["lon"]
    out$lat[ipath] <- refined["lat"]
  }

  out
}

#' Refine one fitted light-calibration anchor
#'
#' Uses a local search window around the current anchor. The window is wider in latitude than
#' longitude because fitted longitude is usually better constrained by twilight timing. If the best
#' grid cell is on the boundary, the search window expands up to a fixed cap.
#'
#' @noRd
geolight_refine_location_one <- function(
  lon,
  lat,
  twl,
  sun,
  extent,
  map_scale,
  twl_calib_adjust = 1.4,
  scale_km = 20,
  max_iter = 2
) {
  scale_local <- geolight_refine_scale(extent, scale_km, map_scale)
  radius_lat_km <- 200
  radius_lon_km <- 100
  max_radius_lat_km <- 500
  max_radius_lon_km <- 250

  for (i in seq_len(max_iter)) {
    fz <- geolight_refine_density(
      twilight = twl$twilight,
      lon = lon,
      lat = lat,
      twl_calib_adjust = twl_calib_adjust
    )
    repeat {
      extent_local <- geolight_refine_extent(
        lon = lon,
        lat = lat,
        radius_lat_km = radius_lat_km,
        radius_lon_km = radius_lon_km,
        extent = extent,
        scale = scale_local
      )
      refined <- geolight_refine_location_score(
        sun = sun,
        extent = extent_local,
        scale = scale_local,
        fz = fz
      )

      if (
        !refined["boundary"] ||
          (radius_lat_km >= max_radius_lat_km && radius_lon_km >= max_radius_lon_km)
      ) {
        break
      }
      radius_lat_km <- min(max_radius_lat_km, radius_lat_km * 2)
      radius_lon_km <- min(max_radius_lon_km, radius_lon_km * 2)
    }

    lon_new <- unname(refined["lon"])
    lat_new <- unname(refined["lat"])

    move_km <- haversine_distance(
      matrix(c(lon, lat), ncol = 2),
      matrix(c(lon_new, lat_new), ncol = 2)
    )
    if (move_km <= scale_km) {
      return(c(lon = lon_new, lat = lat_new))
    }

    lon <- lon_new
    lat <- lat_new

    radius_lat_km <- min(max_radius_lat_km, max(3 * scale_km, 1.5 * move_km))
    radius_lon_km <- min(max_radius_lon_km, max(1.5 * scale_km, 0.75 * move_km))
  }

  c(lon = lon, lat = lat)
}

#' Select a map scale close to the requested kilometer resolution
#' @noRd
geolight_refine_scale <- function(extent, scale_km, map_scale = NULL) {
  target_scale <- min(8, max(1, 2^ceiling(log2(111.32 / scale_km))))
  scales <- unique(c(8, 4, 2, 1, map_scale))
  valid <- vapply(scales, \(scale) {
    all(round(c(extent[4] - extent[3], extent[2] - extent[1]) * scale) ==
      c(extent[4] - extent[3], extent[2] - extent[1]) * scale)
  }, logical(1))
  scales <- scales[valid]
  scales[which.min(abs(log(scales / target_scale)))]
}

#' Build the local search extent around one anchor
#' @noRd
geolight_refine_extent <- function(lon, lat, radius_lat_km, radius_lon_km, extent, scale) {
  n_lat <- round((extent[4] - extent[3]) * scale)
  n_lon <- round((extent[2] - extent[1]) * scale)
  radius_lat <- radius_lat_km / 111.32
  radius_lon <- radius_lon_km / (111.32 * max(cos(lat * .DEG2RAD), 0.1))

  south_cell <- max(0, floor((lat - radius_lat - extent[3]) * scale))
  north_cell <- min(n_lat, ceiling((lat + radius_lat - extent[3]) * scale))
  west_cell <- max(0, floor((lon - radius_lon - extent[1]) * scale))
  east_cell <- min(n_lon, ceiling((lon + radius_lon - extent[1]) * scale))

  if (north_cell <= south_cell) {
    north_cell <- min(n_lat, south_cell + 1)
  }
  if (east_cell <= west_cell) {
    east_cell <- min(n_lon, west_cell + 1)
  }

  c(
    extent[1] + west_cell / scale,
    extent[1] + east_cell / scale,
    extent[3] + south_cell / scale,
    extent[3] + north_cell / scale
  )
}

#' Score one local grid under the current stap-specific light calibration
#' @noRd
geolight_refine_location_score <- function(sun, extent, scale, fz) {
  g <- map_expand(extent, scale)
  zk <- geolight_solar_refracted(geolight_solar_zenith(
    sun = sun,
    lat = g$lat,
    lon = g$lon
  ))
  lk <- fz(as.vector(zk))
  lk[!is.finite(lk)] <- 0
  log_score <- matrix(
    rowSums(matrix(log(lk + .Machine$double.eps), nrow = prod(g$dim))),
    nrow = g$dim[1]
  )
  best <- arrayInd(which.max(log_score), .dim = g$dim)

  c(
    lon = g$lon[best[2]],
    lat = g$lat[best[1]],
    boundary = best[1] %in% c(1, g$dim[1]) || best[2] %in% c(1, g$dim[2])
  )
}

#' Build the stap-specific zenith likelihood used during refinement
#' @noRd
geolight_refine_density <- function(twilight, lon, lat, twl_calib_adjust) {
  hard_bounds <- c(60, 120)
  z <- as.numeric(geolight_solar(date = twilight, lat = lat, lon = lon))
  z <- z[is.finite(z) & z >= hard_bounds[1] & z <= hard_bounds[2]]
  z_range <- range(z, finite = TRUE)
  pad <- min(max(2, diff(z_range) * 0.1), 5)
  dens <- stats::density(
    z,
    adjust = twl_calib_adjust,
    from = max(hard_bounds[1], z_range[1] - pad),
    to = min(hard_bounds[2], z_range[2] + pad)
  )
  stats::approxfun(dens$x, dens$y, yleft = 0, yright = 0)
}
