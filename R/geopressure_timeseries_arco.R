#' @rdname geopressure_timeseries
#' @export
geopressure_timeseries_arco <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = c("single-levels", "land"),
  altitude_formula = "virtual"
) {
  geopressure_timeseries(
    lat = lat,
    lon = lon,
    pressure = pressure,
    start_time = start_time,
    end_time = end_time,
    source = "arco",
    quiet = quiet,
    debug = debug,
    era5_dataset = era5_dataset,
    altitude_formula = altitude_formula
  )
}

geopressure_timeseries_arco_impl <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = "single-levels",
  altitude_formula = "virtual"
) {
  dataset_name <- if (era5_dataset == "land") "ERA5-Land" else "ERA5 single levels"
  query <- era5_grid_snap(lon, lat, era5_dataset)

  if (!is.null(pressure)) {
    requested_date <- as.POSIXct(pressure$date, tz = "UTC")
    requested_hour <- era5_hour_round(requested_date)
    date <- seq(
      min(requested_hour),
      max(requested_hour),
      by = "hour"
    )
  } else {
    date <- seq(
      as.POSIXct(ceiling(as.numeric(start_time) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
      as.POSIXct(ceiling(as.numeric(end_time) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
      by = "hour"
    )
  }
  read_variable <- function(v) {
    era5_arco_read_points(
      variable = v,
      era5_dataset = era5_dataset,
      lon = rep(query$lon, length(date)),
      lat = rep(query$lat, length(date)),
      date = date,
      debug = debug
    )
  }

  if (!quiet) {
    cli::cli_progress_step("Read {dataset_name} pressure from the ECMWF ARCO archive")
  }
  surface_pressure <- read_variable("surface_pressure")

  # The ARCO archive is masked over oceans. Match GeoPressureAPI by moving an ocean point inland.
  if (era5_dataset == "land" && all(is.na(surface_pressure))) {
    land <- era5_land_sea_mask()
    radius <- 10
    radius_lon <- min(180, radius / abs(cos(lat * pi / 180)))
    candidate <- terra::crop(
      land,
      terra::ext(lon - radius_lon, lon + radius_lon, lat - radius, lat + radius)
    )
    candidate_cell <- which(terra::values(candidate) > 0.5, arr.ind = FALSE)
    xy <- terra::xyFromCell(candidate, candidate_cell)
    dlon <- (xy[, 1] - lon) * pi / 180
    dlat <- (xy[, 2] - lat) * pi / 180
    a <- sin(dlat / 2)^2 + cos(lat * pi / 180) * cos(xy[, 2] * pi / 180) * sin(dlon / 2)^2
    distance <- 6371000 * 2 * atan2(sqrt(a), sqrt(1 - a))
    nearest <- which.min(distance)
    lon <- unname(xy[nearest, 1])
    lat <- unname(xy[nearest, 2])
    query <- era5_grid_snap(lon, lat, era5_dataset)
    if (!quiet) {
      cli::cli_alert_warning(
        "Requested position is over water; using the closest ERA5-Land cell ({round(distance[nearest] / 1000)} km away)."
      )
    }
    surface_pressure <- read_variable("surface_pressure")
  }

  out <- data.frame(
    date = date,
    surface_pressure = surface_pressure / 100,
    lat = lat,
    lon = lon
  )

  if (!is.null(pressure)) {
    nearest_time <- match(requested_hour, date)
    out <- out[nearest_time, ]
    out$date <- requested_date

    if (!quiet) {
      cli::cli_progress_step("Read {dataset_name} temperature and compute altitude")
    }
    temperature <- read_variable("temperature_2m")
    dewpoint <- if (altitude_formula == "virtual") {
      read_variable("dewpoint_temperature_2m")[nearest_time]
    }
    elevation <- era5_surface_elevation(query$lon, query$lat, era5_dataset, quiet)
    out$altitude <- pressure_to_altitude(
      pressure$value * 100,
      surface_pressure[nearest_time],
      temperature[nearest_time],
      elevation,
      dewpoint = dewpoint,
      lon = lon,
      lat = lat,
      date = requested_date,
      altitude_formula = altitude_formula
    )
    out <- out[c("date", "surface_pressure", "altitude", "lat", "lon")]
  }

  return(out)
}

era5_arco_client <- function(cache = FALSE) {
  cds_token <- ecmwfr::wf_get_key()
  arco_host <- "https://arco.datastores.ecmwf.int"
  object_cache <- new.env(parent = emptyenv())
  list(
    get_object = function(Bucket, Key, ...) {
      cache_key <- glue::glue("{Bucket}/{Key}")
      if (cache && exists(cache_key, envir = object_cache, inherits = FALSE)) {
        body <- get(cache_key, envir = object_cache, inherits = FALSE)
      } else {
        body <- httr2::request(glue::glue("{arco_host}/{Bucket}/{Key}")) |>
          httr2::req_auth_bearer_token(cds_token) |>
          httr2::req_perform() |>
          httr2::resp_body_raw()
        if (cache) {
          assign(cache_key, body, envir = object_cache)
        }
      }
      list(Body = body)
    },
    list_objects_v2 = function(Bucket, Prefix, ...) {
      response <- httr2::request(glue::glue("{arco_host}/{Bucket}/{Prefix}")) |>
        httr2::req_method("HEAD") |>
        httr2::req_auth_bearer_token(cds_token) |>
        httr2::req_error(is_error = function(response) FALSE) |>
        httr2::req_perform()
      list(KeyCount = as.integer(httr2::resp_status(response) == 200L))
    }
  )
}
