#' Request a pressure time series directly from ERA5-Land ARCO
#'
#' Experimental alternative to [geopressure_timeseries()] using the ECMWF ERA5-Land
#' Analysis-Ready Cloud-Optimised (ARCO) Zarr archive.
#'
#' @inheritParams geopressure_timeseries
#' @param cds_token Climate Data Store API token. By default, read from
#'   `CDSAPI_KEY`, `ecmwfr_PAT`, or `~/.cdsapirc`.
#' @param cache_dir Directory used to cache static ERA5-Land rasters.
#'
#' @return The same data.frame structure as [geopressure_timeseries()].
#' @family pressurepath
#' @export
geopressure_timeseries_arco <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  cds_token = NULL,
  cache_dir = tools::R_user_dir("GeoPressureR", "cache")
) {
  assertthat::assert_that(is.numeric(lon))
  assertthat::assert_that(is.numeric(lat))
  assertthat::assert_that(lon >= -180 & lon <= 180)
  assertthat::assert_that(lat >= -90 & lat <= 90)
  assertthat::assert_that(is.logical(quiet))
  if (is.null(cds_token)) {
    cds_token <- Sys.getenv("CDSAPI_KEY")
    if (!nzchar(cds_token)) {
      cds_token <- Sys.getenv("ecmwfr_PAT")
    }
    cdsapirc <- path.expand("~/.cdsapirc")
    if (!nzchar(cds_token) && file.exists(cdsapirc)) {
      key <- grep("^\\s*key\\s*:", readLines(cdsapirc, warn = FALSE), value = TRUE)
      if (length(key)) {
        cds_token <- trimws(sub("^\\s*key\\s*:\\s*", "", key[[1]]))
      }
    }
  }
  assertthat::assert_that(
    nzchar(cds_token),
    msg = "Set `CDSAPI_KEY`, `ecmwfr_PAT`, or configure `~/.cdsapirc`."
  )

  if (!requireNamespace("Rarr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg Rarr} is required for {.fun geopressure_timeseries_arco}.",
      "i" = "Install it with {.run BiocManager::install('Rarr')}."
    ))
  }
  arco_host <- "https://arco.datastores.ecmwf.int"
  arco_client <- list(
    get_object = function(Bucket, Key, ...) {
      body <- httr2::request(glue::glue("{arco_host}/{Bucket}/{Key}")) |>
        httr2::req_auth_bearer_token(cds_token) |>
        httr2::req_perform() |>
        httr2::resp_body_raw()
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

  if (!is.null(pressure)) {
    assertthat::assert_that(is.data.frame(pressure))
    assertthat::assert_that("date" %in% names(pressure))
    assertthat::assert_that(assertthat::is.time(pressure$date))
    assertthat::assert_that("value" %in% names(pressure))
    assertthat::assert_that(is.numeric(pressure$value))
    assertthat::assert_that(nrow(pressure) > 0)
    requested_date <- as.POSIXct(pressure$date, tz = "UTC")
    requested_hour <- as.POSIXct(
      ceiling(as.numeric(requested_date) / 3600) * 3600,
      origin = "1970-01-01",
      tz = "UTC"
    )
    date <- seq(
      min(requested_hour),
      max(requested_hour),
      by = "hour"
    )
  } else {
    start_time <- as.POSIXct(start_time, tz = "UTC")
    end_time <- as.POSIXct(end_time, tz = "UTC")
    assertthat::assert_that(start_time <= end_time)
    date <- seq(
      as.POSIXct(ceiling(as.numeric(start_time) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
      as.POSIXct(ceiling(as.numeric(end_time) / 3600) * 3600, origin = "1970-01-01", tz = "UTC"),
      by = "hour"
    )
  }

  if (!quiet) {
    cli::cli_progress_step("Read ERA5-Land pressure from the ECMWF ARCO archive")
  }
  surface_pressure <- era5_land_arco_read(
    variable = "sp",
    lon = round(lon * 10) / 10,
    lat = round(lat * 10) / 10,
    date = date,
    arco_client = arco_client,
    debug = debug
  )

  # The ARCO archive is masked over oceans. Match GeoPressureAPI by moving an ocean point inland.
  if (all(is.na(surface_pressure))) {
    lsm_file <- file.path(cache_dir, "lsm_1279l4_0.1x0.1.grb")
    if (!file.exists(lsm_file)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      httr2::request(
        "https://confluence.ecmwf.int/download/attachments/140385202/lsm_1279l4_0.1x0.1.grb?version=1&modificationDate=1567528624201&api=v2"
      ) |>
        httr2::req_perform(path = lsm_file)
    }
    land <- terra::rast(lsm_file)
    terra::ext(land) <- c(-0.05, 359.95, -90.05, 90.05)
    land <- terra::rotate(land)
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
    if (!quiet) {
      cli::cli_alert_warning(
        "Requested position is over water; using the closest ERA5-Land cell ({round(distance[nearest] / 1000)} km away)."
      )
    }
    surface_pressure <- era5_land_arco_read(
      variable = "sp",
      lon = round(lon * 10) / 10,
      lat = round(lat * 10) / 10,
      date = date,
      arco_client = arco_client,
      debug = debug
    )
  }

  out <- data.frame(
    date = date,
    surface_pressure = surface_pressure / 100,
    lat = lat,
    lon = lon
  )

  if (!is.null(pressure)) {
    if (!quiet) {
      cli::cli_progress_step("Read ERA5-Land temperature and compute altitude")
    }
    temperature <- era5_land_arco_read(
      variable = "t2m",
      lon = round(lon * 10) / 10,
      lat = round(lat * 10) / 10,
      date = date,
      arco_client = arco_client,
      debug = debug
    )
    nearest_time <- match(requested_hour, date)
    out <- out[nearest_time, ]
    out$date <- requested_date

    geopotential_file <- file.path(cache_dir, "geo_1279l4_0.1x0.1.grib2")
    if (!file.exists(geopotential_file)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      httr2::request(
        "https://confluence.ecmwf.int/download/attachments/140385202/geo_1279l4_0.1x0.1.grib2?version=1&modificationDate=1582901403445&api=v2"
      ) |>
        httr2::req_perform(path = geopotential_file)
    }
    geopotential <- terra::rast(geopotential_file)
    terra::ext(geopotential) <- c(-0.05, 359.95, -90.05, 90.05)
    geopotential <- terra::rotate(geopotential)
    elevation <- terra::extract(geopotential, matrix(c(lon, lat), ncol = 2))[[1]] / 9.80665
    out$altitude <- elevation +
      temperature[nearest_time] /
        -0.0065 *
        ((pressure$value * 100 / surface_pressure[nearest_time])^(-8.31432 *
          -0.0065 /
          9.80665 /
          0.0289644) -
          1)
    out <- out[c("date", "surface_pressure", "altitude", "lat", "lon")]

    pressure_merge <- pressure
    if (!("stap_id" %in% names(pressure_merge))) {
      pressure_merge$stap_id <- 1
    }
    if (!("label" %in% names(pressure_merge))) {
      pressure_merge$label <- ""
    }
    out <- merge(pressure_merge, out, all.x = TRUE)
    names(out)[names(out) == "value"] <- "pressure_tag"
    stap_id <- out$stap_id
    label <- out$label
    id_norm <- stap_id != 0 & label != "discard"
    if (sum(id_norm) > 0) {
      elev <- ifelse(startsWith(label, "elev_"), gsub("^.*?elev_", "", label), "0")
      for (elev_i in unique(elev)) {
        id_elev <- elev == elev_i
        out$surface_pressure_norm[id_elev] <- out$surface_pressure[id_elev] -
          mean(out$surface_pressure[id_elev & id_norm]) +
          mean(out$pressure_tag[id_elev & id_norm])
      }
    }
  }

  return(out)
}

era5_land_arco_read <- function(
  variable,
  lon,
  lat,
  date,
  arco_client,
  debug
) {
  store <- switch(
    variable,
    sp = "cadl-arco-geo-009/arco/reanalysis_era5_land/sfc-pressure-precipitation",
    t2m = "cadl-arco-geo-007/arco/reanalysis_era5_land/sfc-2m-temperature"
  )
  array <- glue::glue(
    "https://arco.datastores.ecmwf.int/{store}/geoChunked.zarr/{variable}"
  )
  # Convert ERA5-Land coordinates to one-based Zarr indexes.
  time_index <- as.integer(as.numeric(date) / 3600 - (-175296) + 1)
  lat_index <- as.integer(round((lat + 90) * 10) + 1)
  lon_index <- if (lon == -180) 3600L else as.integer(round((lon + 179.9) * 10) + 1)
  if (debug) {
    cli::cli_text(
      "Read {.field {variable}} indexes {range(time_index)}, {lat_index}, {lon_index} from {.url {array}}"
    )
  }
  Rarr::read_zarr_array(
    array,
    index = list(time_index, lat_index, lon_index),
    s3_client = arco_client
  ) |>
    drop() |>
    unname()
}
