#' Ground elevation along a path
#'
#' @description
#' This function returns ground elevation along a path from precomputed statistics of the
#' [Copernicus DEM GLO-90](https://registry.opendata.aws/copernicus-dem/) (90 m): the minimum,
#' the 10th to 90th percentiles and the maximum of the DEM pixels within each grid cell. These
#' statistics describe the range of terrain elevation the bird can fly over within a cell.
#'
#' The statistics are computed by
#' [`data-raw/ground_elevation.R`](https://github.com/GeoPressure/GeoPressureR/blob/main/data-raw/ground_elevation.R)
#' on the map grids of [tag_set_map()] (`scale` 1, 2, 4, 5 and 10) and published as the
#' [`ground-elevation-v1` release](https://github.com/GeoPressure/GeoPressureR/releases/tag/ground-elevation-v1).
#' The file of the requested `scale` (0.4 MB for `scale = 1`, 6 MB for `4`, 9 MB for `5`, 33 MB
#' for `10`) is downloaded once on first use and cached in `tools::R_user_dir("GeoPressureR",
#' "cache")`; later calls are local.
#'
#' Consecutive path positions are connected by great-circle segments. Each segment is sampled
#' from its start at a fixed distance interval controlled by `sampling_scale`, and the last
#' position is added. Each sample reads the requested statistics of the grid cell containing it.
#'
#' Missing positions between known positions are interpolated separately in longitude and latitude using cumulative flight duration from `start` and `end`. Missing positions before the first or after the last known position are excluded with a warning.
#'
#' The result is a spatial profile without timestamps. Its fractional `stap_id` values describe distance along each segment, whereas [find_stap()] uses elapsed flight time. Interpolating elevation onto a pressure path using `stap_id` therefore assumes constant speed along each segment.
#'
#' Elevation is rounded to whole meters above the EGM2008 geoid. Sea is `0`, and coastal cells
#' include sea pixels as ground the bird can fly over.
#'
#' @param path A GeoPressureR `path` data.frame with `lon`, `lat`, `stap_id`, `start`, and `end` columns. Coordinates are in degrees, times are POSIXct, and `stap_id` is numbered `1, ..., nrow(path)` in chronological order.
#' @param scale Elevation grid resolution in pixels per degree (e.g., `4` corresponds to `0.25°`),
#'   matching `scale` of [tag_set_map()] so that cells coincide with the map grid. The finest
#'   available grid not coarser than `scale` is used among `1, 2, 4, 5, 10` (e.g., `3` uses `4`),
#'   and `10` above it.
#' @param sampling_scale Sampling density along each path segment, with a distance interval of `111139 / sampling_scale` meters (about 27.8 km for `4`). Defaults to `scale`; increasing it adds samples without changing the elevation grid resolution.
#' @param percentile Elevation percentiles to retrieve within each grid cell, among
#'   `0, 10, 20, ..., 90, 100`; `0` is the minimum, `50` the median and `100` the maximum.
#' @param debug logical to display additional information, such as the elevation file used.
#'
#' @return A data.frame containing
#' - `stap_id` stationary-period ID plus the fraction of segment distance traveled toward the next position (e.g., `2.5` is halfway from position 2 to 3)
#' - `lon`, `lat` centre of the grid cell read, in degrees, rounded to three decimal places; these differ from the exact positions on the polyline
#' - `distance` cumulative geodesic (WGS84) distance in km from the first retained path position
#' - `X<percentile>` ground elevation in meters for each requested percentile (e.g., `X10`, `X50`, and `X90` by default)
#'
#' @examplesIf interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Create a path
#' path <- data.frame(
#'   lon = c(8.47, 9.41, 9.01, -0.91, 14.24, 27.30, 34.39, 30.00),
#'   lat = c(48.89, 44.78, 40.07, 37.68, 17.33, 7.32, 8.09, -23.13),
#'   start = as.POSIXct(
#'     c(
#'       "2017-05-01 00:42", "2017-05-03 01:22", "2017-05-03 22:47", "2017-05-06 03:32",
#'       "2017-05-07 01:12", "2017-05-07 22:32", "2017-05-09 21:52", "2017-05-10 21:12"
#'     ),
#'     tz = "UTC"
#'   ),
#'   end = as.POSIXct(
#'     c(
#'       "2017-05-02 22:12", "2017-05-03 20:12", "2017-05-06 02:47", "2017-05-06 23:22",
#'       "2017-05-07 17:42", "2017-05-09 20:27", "2017-05-10 19:57", "2017-05-11 21:17"
#'     ),
#'     tz = "UTC"
#'   ),
#'   stap_id = seq(1, 8)
#' )
#'
#'
#' plot_path(path)
#'
#' elevation <- path2elevation(path)
#'
#' plot(elevation$distance, elevation$X50,
#'   type = "l",
#'   ylab = "Elevation (m)", xlab = "Distance from start (km)"
#' )
#' lines(elevation$distance, elevation$X10, lty = 5)
#' lines(elevation$distance, elevation$X90, lty = 5)
#' id <- elevation$stap_id %% 1 == 0
#' points(elevation$distance[id], elevation$X90[id], col = "red")
#'
#' # Starting from the most likely path and its pressurepath computed with `geopressuretemplate()`
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   path <- load_interim("18LX", var = "path_most_likely")
#'   pressurepath <- load_interim("18LX", var = "pressurepath_most_likely")
#' })
#' elevation <- path2elevation(path)
#'
#' # Map median ground elevation to pressure samples, assuming constant flight speed
#' pressurepath$ground_elevation <- stats::approx(
#'   x = elevation$stap_id,
#'   y = elevation$X50,
#'   xout = pressurepath$stap_id
#' )$y
#'
#' # Altitude above median ground level (meters)
#' pressurepath$altitude_agl <- pressurepath$altitude - pressurepath$ground_elevation
#'
#' @family path
#' @export
path2elevation <- function(
  path,
  scale = 4,
  sampling_scale = scale,
  percentile = c(10, 50, 90),
  debug = FALSE
) {
  # Check input
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(
    path,
    c("lon", "lat", "stap_id")
  ))
  assertthat::assert_that(is.numeric(scale))
  assertthat::assert_that(scale > 0)
  assertthat::assert_that(is.numeric(sampling_scale))
  assertthat::assert_that(sampling_scale > 0)
  assertthat::assert_that(all(is.numeric(percentile)))
  assertthat::assert_that(all(percentile %in% seq(0, 100, 10)))
  assertthat::assert_that(is.logical(debug))

  # 1. Interpolation of !include
  # In order to have the correct stap_id during the flight, we need to interpolate the position of
  # stap not included. As opposed to tag2path(), here we don't need to insure that the position
  # falls exactly on a grid cell (in order to be able to have an index).
  path_interp <- is.na(path$lat) | is.na(path$lon)

  # We only request elevation between the first and last defined position. Basically, if the path
  # is unknown at the end (or start), we just ignore this part
  stap_id_considered <- seq(min(which(!path_interp)), max(which(!path_interp)))
  stap_id_not_considered <- path$stap_id[
    !(path$stap_id %in% stap_id_considered)
  ]
  if (length(stap_id_not_considered) > 0) {
    cli::cli_warn(c(
      "!" = "The position are not defined for stationary periods ({.val {stap_id_not_considered}}). Because they are at the begining or end of the path, we are not able to interpolate them and they will consequently not be considered in the computation of elevation."
    ))
  }

  # Retrieve only the stap_id to consider
  path_c <- path[stap_id_considered, ]
  path_interp_c <- path_interp[stap_id_considered]

  # Compute flight duration including all stap_id
  flight <- stap2flight(path_c, include_stap_id = path_c$stap_id)

  # Cummulate the flight duration to get a proxy of the over distance covered
  total_flight <- cumsum(as.numeric(c(0, flight$duration)))

  # Interpolate the lat and lon separately using `total_flight` as a spacing between position
  path_c$lon[path_interp_c] <- stats::approx(
    total_flight[!path_interp_c],
    path_c$lon[!path_interp_c],
    total_flight[path_interp_c]
  )$y
  path_c$lat[path_interp_c] <- stats::approx(
    total_flight[!path_interp_c],
    path_c$lat[!path_interp_c],
    total_flight[path_interp_c]
  )$y

  assertthat::assert_that(all(diff(path_c$stap_id) == 1))

  # Finest precomputed grid not coarser than `scale`, downloaded once to the cache
  scale <- c(1, 2, 4, 5, 10)[min(which(c(1, 2, 4, 5, 10) >= min(scale, 10)))]
  file <- file.path(
    tools::R_user_dir("GeoPressureR", "cache"),
    glue::glue("glo90-ground-scale{scale}.tif")
  )
  if (!file.exists(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    httr2::request(glue::glue(
      "https://github.com/GeoPressure/GeoPressureR/releases/download/ground-elevation-v1/{basename(file)}"
    )) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = glue::glue("{file}.tmp"))
    file.rename(glue::glue("{file}.tmp"), file)
  }
  if (debug) {
    cli::cli_text("Ground elevation file: {.file {file}}")
  }

  # Sample each segment every `step` m of geodesic length from its start, placing samples on the
  # great circle; zero-length segments keep their start
  lon <- path_c$lon * pi / 180
  lat <- path_c$lat * pi / 180
  i <- seq_len(length(lon) - 1)
  delta <- 2 *
    asin(sqrt(
      sin((lat[i + 1] - lat[i]) / 2)^2 +
        cos(lat[i]) * cos(lat[i + 1]) * sin((lon[i + 1] - lon[i]) / 2)^2
    ))
  len <- terra::distance(
    cbind(path_c$lon[i], path_c$lat[i]),
    cbind(path_c$lon[i + 1], path_c$lat[i + 1]),
    lonlat = TRUE,
    pairwise = TRUE
  )
  step <- 111139 / sampling_scale
  n <- pmax(1, ceiling(len / step))
  seg <- rep(i, n)
  f <- ifelse(len[seg] > 0, (sequence(n) - 1) * step / len[seg], 0)
  a <- ifelse(delta[seg] > 0, sin((1 - f) * delta[seg]) / sin(delta[seg]), 1)
  b <- ifelse(delta[seg] > 0, sin(f * delta[seg]) / sin(delta[seg]), 0)
  x <- a * cos(lat[seg]) * cos(lon[seg]) + b * cos(lat[seg + 1]) * cos(lon[seg + 1])
  y <- a * cos(lat[seg]) * sin(lon[seg]) + b * cos(lat[seg + 1]) * sin(lon[seg + 1])
  z <- a * sin(lat[seg]) + b * sin(lat[seg + 1])

  # Append the last position
  sample_lon <- c(atan2(y, x) * 180 / pi, path_c$lon[length(lon)])
  sample_lat <- c(atan2(z, sqrt(x^2 + y^2)) * 180 / pi, path_c$lat[length(lon)])
  distance <- c(c(0, cumsum(len))[seg] + f * len[seg], sum(len)) / 1000

  # Read the statistics of the grid cell containing each sample
  elevation <- terra::rast(file)
  cell <- terra::cellFromXY(elevation, cbind((sample_lon + 180) %% 360 - 180, sample_lat))
  band <- ifelse(
    percentile == 0,
    "min",
    ifelse(percentile == 100, "max", glue::glue("p{percentile}"))
  )
  out <- stats::setNames(elevation[[band]][cell], glue::glue("X{percentile}"))
  xy <- round(terra::xyFromCell(elevation, cell), 3)
  out$distance <- distance
  out$lat <- xy[, 2]
  out$lon <- xy[, 1]
  out$stap_id <- c(path_c$stap_id[seg] + f, path_c$stap_id[length(lon)])

  out
}
