#' Retrieve an ERA5 pressure time series directly from ARCO
#'
#' @description
#' `geopressure_timeseries_arco()` retrieves an hourly surface-pressure time series at one
#' location directly from the ECMWF Analysis-Ready Cloud-Optimised (ARCO) Zarr archive. It is a
#' client-side alternative to [geopressure_timeseries()]: the requested Zarr chunks are read in R
#' instead of asking GeoPressureAPI to prepare the time series.
#'
#' The function can either return ERA5 surface pressure for an interval defined by `start_time`
#' and `end_time`, or combine ERA5 data with a geolocator pressure series supplied through
#' `pressure`. In the latter case it can also estimate the geolocator altitude and normalise the
#' ERA5 pressure to the mean pressure level measured by the tag.
#'
#' @section ERA5 datasets and spatial matching:
#' Two ARCO products are supported:
#'
#' - `era5_dataset = "land"` uses ERA5-Land on a 0.1 degree regular grid. ERA5-Land is masked over
#'   oceans. If the selected grid cell contains no pressure data, the function uses the official
#'   ERA5-Land land-sea mask to find the closest land cell and returns its coordinates in `lat`
#'   and `lon`.
#' - `era5_dataset = "single-levels"` uses the global ERA5 single-level product on a 0.25 degree
#'   regular grid. Surface pressure and temperature are defined over both land and water, so the
#'   requested location is not moved onshore.
#'
#' The input coordinates are rounded to the nearest cell of the selected grid. Except when the
#' ERA5-Land closest-cell procedure is used, the returned `lat` and `lon` remain the input
#' coordinates rather than the rounded grid-cell coordinates.
#'
#' ERA5-Land is available through this implementation from 2 January 1950, whereas ERA5 single
#' levels is available from 2 January 1940. These starting dates reflect the time coordinates of
#' the current ARCO stores.
#'
#' @section Temporal matching:
#' When `pressure` is not supplied, `start_time` and `end_time` are converted to UTC and rounded
#' upwards to complete hours. The result contains every hourly ERA5 value in the resulting closed
#' interval.
#'
#' When `pressure` is supplied, every `pressure$date` is converted to UTC and matched to the
#' closest available ERA5 hour, with half-hours matched to the earlier hour. Because the requested
#' ERA5 interval starts after the first tag timestamp, the first observation is matched to the next
#' hour if the preceding hour lies outside that interval. The returned `date` is then restored to
#' the original tag timestamp. No temporal interpolation is performed.
#'
#' @section Altitude computation:
#' If `pressure` is supplied and `compute_altitude = TRUE`, the altitude of the geolocator above
#' mean sea level, \eqn{z_{tag}}, is calculated with the barometric equation
#' \deqn{
#' z_{tag}(t) = z_{ERA5} + \frac{T_{ERA5}(t)}{L_b}
#' \left[\left(\frac{P_{tag}(t)}{P_{ERA5}(t)}\right)^{-R L_b/(g M)} - 1\right],
#' }
#' where \eqn{P_{tag}} is the pressure measured by the geolocator, \eqn{P_{ERA5}} is ERA5 surface
#' pressure, \eqn{T_{ERA5}} is ERA5 2 m temperature, and \eqn{z_{ERA5}} is the model surface
#' geopotential height. The constants are the standard temperature lapse rate
#' \eqn{L_b=-0.0065\ \mathrm{K\,m^{-1}}}, universal gas constant
#' \eqn{R=8.31432\ \mathrm{J\,mol^{-1}\,K^{-1}}}, standard gravity
#' \eqn{g=9.80665\ \mathrm{m\,s^{-2}}}, and molar mass of dry air
#' \eqn{M=0.0289644\ \mathrm{kg\,mol^{-1}}}.
#'
#' Pressure in `pressure$value` is expected in hPa and is converted internally to Pa for this
#' calculation. ERA5 temperature, surface pressure, and geopotential are taken from the same
#' dataset and grid cell. The returned `altitude` is in metres above mean sea level. Set
#' `compute_altitude = FALSE` to avoid reading temperature and geopotential when only pressure
#' comparison and normalisation are required.
#'
#' @section Pressure normalisation:
#' When tag pressure is supplied, ERA5 surface pressure is shifted to the mean tag-pressure level
#' so that their temporal variations can be compared. Within each elevation-label group \eqn{e},
#' the function calculates
#' \deqn{
#' P_{ERA5,0,e}(t) = P_{ERA5,e}(t) - \overline{P}_{ERA5,e} + \overline{P}_{tag,e}.
#' }
#' The means use observations for which `stap_id != 0` and `label != "discard"`. Labels beginning
#' with `"elev_"` define separate elevation groups; all other labels belong to the default group.
#' If `stap_id` or `label` is absent, the function adds `stap_id = 1` or `label = ""`, respectively.
#' The result is returned as `surface_pressure_norm`.
#'
#' @section ARCO access, credentials, and local data:
#' The geo-chunked ARCO layout is used because it is optimised for long time series at a single
#' location. The Rarr package downloads only the Zarr chunks intersecting the requested time and
#' grid cell. ARCO requests use the CDS API key returned by [ecmwfr::wf_get_key()]; users should
#' configure it beforehand with [ecmwfr::wf_set_key()]. The optional Rarr and ecmwfr packages must
#' both be installed; Rarr is distributed through Bioconductor and ecmwfr through CRAN.
#'
#' Altitude calculation also requires a time-invariant surface-geopotential grid. The ERA5-Land
#' 0.1 degree invariant is downloaded from the official ERA5-Land documentation. The ERA5
#' 0.25 degree invariant is requested once from the CDS `reanalysis-era5-single-levels` dataset.
#' These replaceable files are stored in `tools::R_user_dir("GeoPressureR", "cache")` and reused
#' across R sessions and package updates. They can safely be deleted; the next altitude request
#' will download them again. No invariant file is downloaded when `compute_altitude = FALSE`.
#'
#' ARCO access is currently an ECMWF beta service. Its availability, store structure, and rate
#' limits may change independently of GeoPressureR.
#'
#' @param lat Numeric scalar latitude to query, between -90 and 90 degrees.
#' @param lon Numeric scalar longitude to query, between -180 and 180 degrees.
#' @param pressure Optional data.frame containing the geolocator pressure series, with at least a
#'   POSIXct or POSIXlt `date` column and a numeric `value` column in hPa. Additional columns are
#'   retained.
#' @param start_time Start of the requested interval. Used only when `pressure` is `NULL` and
#'   interpreted in UTC.
#' @param end_time End of the requested interval. Used only when `pressure` is `NULL` and
#'   interpreted in UTC.
#' @param compute_altitude Logical scalar. If `TRUE`, compute altitude from tag pressure, ERA5
#'   surface pressure, 2 m temperature, and surface geopotential. It defaults to `TRUE` when
#'   `pressure` is supplied and `FALSE` otherwise.
#' @param quiet Logical scalar to suppress progress messages.
#' @param debug Logical scalar to display the selected Zarr paths and array indices.
#' @param era5_dataset ERA5 product to use: `"land"` for ERA5-Land at 0.1 degree resolution or
#'   `"single-levels"` for global ERA5 at 0.25 degree resolution.
#'
#' @return A data.frame. Without `pressure`, it contains:
#'
#' - `date`: hourly POSIXct timestamp in UTC;
#' - `surface_pressure`: ERA5 surface pressure in hPa;
#' - `lat`, `lon`: queried coordinates, or the selected closest-land coordinates for ERA5-Land.
#'
#' With `pressure`, the input rows and additional columns are retained, `value` is renamed to
#' `pressure_tag`, and the ERA5 variables are joined by date. The result additionally contains
#' `surface_pressure_norm` when normalisation observations are available and `altitude` when
#' `compute_altitude = TRUE`.
#'
#' @examplesIf FALSE
#' # Hourly ERA5-Land surface pressure
#' x <- geopressure_timeseries_arco(
#'   lat = 46,
#'   lon = 6,
#'   start_time = "2020-01-01 00:00",
#'   end_time = "2020-01-02 00:00"
#' )
#'
#' # Global ERA5 pressure over water
#' ocean <- geopressure_timeseries_arco(
#'   lat = 0,
#'   lon = -30,
#'   start_time = "2020-01-01 00:00",
#'   end_time = "2020-01-02 00:00",
#'   era5_dataset = "single-levels"
#' )
#'
#' # Add ERA5 pressure, normalised pressure, and estimated altitude to tag data
#' pressure <- data.frame(
#'   date = as.POSIXct(c("2020-01-01 00:10", "2020-01-01 01:10"), tz = "UTC"),
#'   value = c(980, 979)
#' )
#' pressurepath <- geopressure_timeseries_arco(
#'   lat = 46,
#'   lon = 6,
#'   pressure = pressure,
#'   era5_dataset = "land"
#' )
#'
#' @family pressurepath
#' @references
#' Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, and Felix Liechti. 2023. Global
#' Positioning with Animal-borne Pressure Sensors. *Methods in Ecology and Evolution*, 14,
#' 1118-1129. \doi{10.1111/2041-210X.14043}.
#'
#' ECMWF ERA5 hourly data on single levels, \doi{10.24381/cds.adbb2d47}.
#'
#' ECMWF ERA5-Land hourly data, \doi{10.24381/cds.e2161bac}.
#' @export
geopressure_timeseries_arco <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  compute_altitude = !is.null(pressure),
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = c("land", "single-levels")
) {
  assertthat::assert_that(is.numeric(lon))
  assertthat::assert_that(is.numeric(lat))
  assertthat::assert_that(lon >= -180 & lon <= 180)
  assertthat::assert_that(lat >= -90 & lat <= 90)
  assertthat::assert_that(is.logical(compute_altitude), length(compute_altitude) == 1)
  assertthat::assert_that(!compute_altitude || !is.null(pressure))
  assertthat::assert_that(is.logical(quiet))
  era5_dataset <- match.arg(era5_dataset)
  dataset_name <- if (era5_dataset == "land") "ERA5-Land" else "ERA5 single levels"
  resolution <- if (era5_dataset == "land") 0.1 else 0.25
  cache_dir <- tools::R_user_dir("GeoPressureR", "cache")
  query_lon <- floor(lon / resolution + 0.5) * resolution
  query_lat <- floor(lat / resolution + 0.5) * resolution

  if (!requireNamespace("Rarr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg Rarr} is required for {.fun geopressure_timeseries_arco}.",
      "i" = "Install it with {.run BiocManager::install('Rarr')}."
    ))
  }
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ecmwfr} is required for {.fun geopressure_timeseries_arco}.",
      "i" = "Install it with {.run install.packages('ecmwfr')}."
    ))
  }
  cds_token <- ecmwfr::wf_get_key()
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
    first_hour <- ceiling(min(as.numeric(requested_date)) / 3600)
    requested_hour <- as.POSIXct(
      pmax(first_hour, ceiling(as.numeric(requested_date) / 3600 - 0.5)) * 3600,
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
    cli::cli_progress_step("Read {dataset_name} pressure from the ECMWF ARCO archive")
  }
  surface_pressure <- era5_arco_read(
    variable = "sp",
    era5_dataset = era5_dataset,
    lon = query_lon,
    lat = query_lat,
    date = date,
    arco_client = arco_client,
    debug = debug
  )

  # The ARCO archive is masked over oceans. Match GeoPressureAPI by moving an ocean point inland.
  if (era5_dataset == "land" && all(is.na(surface_pressure))) {
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
    query_lon <- round(lon * 10) / 10
    query_lat <- round(lat * 10) / 10
    if (!quiet) {
      cli::cli_alert_warning(
        "Requested position is over water; using the closest ERA5-Land cell ({round(distance[nearest] / 1000)} km away)."
      )
    }
    surface_pressure <- era5_arco_read(
      variable = "sp",
      era5_dataset = era5_dataset,
      lon = query_lon,
      lat = query_lat,
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
    nearest_time <- match(requested_hour, date)
    out <- out[nearest_time, ]
    out$date <- requested_date

    if (compute_altitude) {
      if (!quiet) {
        cli::cli_progress_step("Read {dataset_name} temperature and compute altitude")
      }
      temperature <- era5_arco_read(
        variable = "t2m",
        era5_dataset = era5_dataset,
        lon = query_lon,
        lat = query_lat,
        date = date,
        arco_client = arco_client,
        debug = debug
      )
      if (era5_dataset == "land") {
        geopotential_file <- file.path(cache_dir, "geo_1279l4_0.1x0.1.grib2")
        if (!file.exists(geopotential_file)) {
          dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
          httr2::request(
            "https://confluence.ecmwf.int/download/attachments/140385202/geo_1279l4_0.1x0.1.grib2?version=1&modificationDate=1582901403445&api=v2"
          ) |>
            httr2::req_perform(path = geopotential_file)
        }
      } else {
        geopotential_file <- file.path(cache_dir, "era5-geopotential-0.25.grib")
        if (!file.exists(geopotential_file)) {
          dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
          ecmwfr::wf_request(
            list(
              dataset_short_name = "reanalysis-era5-single-levels",
              product_type = "reanalysis",
              variable = "geopotential",
              year = "2000",
              month = "01",
              day = "01",
              time = "00:00",
              data_format = "grib",
              download_format = "unarchived",
              target = basename(geopotential_file)
            ),
            path = cache_dir,
            verbose = !quiet
          )
        }
      }
      geopotential <- terra::rast(geopotential_file)
      terra::ext(geopotential) <- if (era5_dataset == "land") {
        c(-0.05, 359.95, -90.05, 90.05)
      } else {
        c(-0.125, 359.875, -90.125, 90.125)
      }
      geopotential <- terra::rotate(geopotential)
      elevation <- terra::extract(
        geopotential,
        matrix(c(query_lon, query_lat), ncol = 2)
      )[[1]] /
        9.80665
      out$altitude <- elevation +
        temperature[nearest_time] /
          -0.0065 *
          ((pressure$value * 100 / surface_pressure[nearest_time])^(-8.31432 *
            -0.0065 /
            9.80665 /
            0.0289644) -
            1)
    }
    out <- out[c(
      "date",
      "surface_pressure",
      if (compute_altitude) "altitude",
      "lat",
      "lon"
    )]

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

era5_arco_read <- function(
  variable,
  era5_dataset,
  lon,
  lat,
  date,
  arco_client,
  debug
) {
  store <- if (era5_dataset == "land") {
    switch(
      variable,
      sp = "cadl-arco-geo-009/arco/reanalysis_era5_land/sfc-pressure-precipitation",
      t2m = "cadl-arco-geo-007/arco/reanalysis_era5_land/sfc-2m-temperature"
    )
  } else {
    "cadl-arco-geo-002/arco/reanalysis_era5_single_levels/sfc"
  }
  array <- glue::glue(
    "https://arco.datastores.ecmwf.int/{store}/geoChunked.zarr/{variable}"
  )
  if (era5_dataset == "land") {
    time_index <- as.integer(as.numeric(date) / 3600 - (-175296) + 1)
    lat_index <- as.integer(round((lat + 90) * 10) + 1)
    lon_index <- if (lon == -180) 3600L else as.integer(round((lon + 179.9) * 10) + 1)
  } else {
    time_index <- as.integer((as.numeric(date) - (-946771200)) / 3600 + 1)
    lat_index <- as.integer(round((lat + 90) * 4) + 1)
    lon_index <- if (lon == 180) 1L else as.integer(round((lon + 180) * 4) + 1)
  }
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
