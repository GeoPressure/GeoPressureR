#' Download flight data
#'
#' @description
#' Download ERA5 hourly data on pressure levels (\doi{10.24381/cds.bd0915c6}) for each flight of
#' `tag` and save one NetCDF file per flight, covering the flight hours and the pressure levels
#' flown. These files are then read by [edge_add_wind()] and [graph_add_wind()].
#'
#' More information [in the GeoPressureManual](
#' https://geopressure.org/GeoPressureManual/trajectory-with-wind.html).
#'
#' @section Choosing a source:
#' Both sources provide the same ERA5 data and write the same files.
#' - `"cds"` (default) requests one file per flight from the
#' [Climate Data Store (CDS)](https://cds.climate.copernicus.eu/) with
#' [`ecmwfr`](https://bluegreen-labs.github.io/ecmwfr/index.html). It needs an ECMWF key (see
#' below). The CDS queues requests and runs only about 2 at a time per account, so when it is busy
#' a tag can take hours to days.
#' - `"gcs"` reads the public [Google ARCO-ERA5](https://github.com/google-research/arco-era5)
#' copy of ERA5 on Google Cloud Storage. It needs no key and has no queue, so a tag usually takes
#' minutes. Each hour of each variable is downloaded as one global field (~120 MB), so the time
#' depends on the flight durations and your bandwidth, not on `extent`. It needs the package `Rarr`
#' and lacks a few variables (see [`variable`](#arg-variable)).
#'
#' Use `"gcs"` when the CDS is slow, unless you need one of these variables or have a slow
#' connection.
#'
#' @section File format:
#' The downloaded data are processed to take less space. Each file keeps only the flight hours.
#' Values are stored as 16-bit integers with `scale_factor` and `add_offset` (wind to 0.01 m/s),
#' which NetCDF readers unpack automatically. Variables use the CDS short names, units and
#' attributes, following the CF conventions. Writing the files needs the package `ncdf4`.
#'
#' @template ecmwf-key
#' @param tag a GeoPressureR `tag` object.
#' @param extent geographical extent of the area for which wind data is downloaded.
#' Vector of length 4 `c(xmin, xmax, ymin, ymax)` or `c(W, E, S, N)`.
#' @param include_stap_id stationary period identifiers of the start of the flight to download.
#' Default is to download all flights.
#' @param variable ERA5 [pressure-level variables
#' ](https://confluence.ecmwf.int/display/CKB/ERA5:+data+documentation#ERA5:datadocumentation-Table9)
#' to download, using the same names as [edge_add_wind()]. Available from both sources:
#' `"u_component_of_wind"`, `"v_component_of_wind"`, `"temperature"`, `"geopotential"`,
#' `"specific_humidity"`, `"vertical_velocity"`, `"fraction_of_cloud_cover"`,
#' `"specific_cloud_liquid_water_content"`, `"specific_cloud_ice_water_content"`,
#' `"ozone_mass_mixing_ratio"` and `"potential_vorticity"`. Only with `source = "cds"`:
#' `"relative_humidity"`, `"divergence"`, `"vorticity"`, `"specific_rain_water_content"` and
#' `"specific_snow_water_content"`.
#' @param file function returning the path of the wind file of a flight from (1) the stationary
#' period identifier and (2) the `tag_id`. The default, `wind_file_default`, gives
#' `./data/wind/{tag_id}/{tag_id}_{stap_id}.nc`.
#' @param overwrite logical. If `TRUE`, file is overwritten.
#' @param workers number of downloads run in parallel. Defaults to `2` for `"cds"`, which rejects
#' more queued requests per account (use `1` if other sessions also request data), and `6` for
#' `"gcs"`, where each download needs about 300 MB of memory.
#' @param source `"cds"` (default) or `"gcs"`. See [Choosing a source](#choosing-a-source).
#' @param cds_token `r lifecycle::badge("deprecated")` Enter the API token with
#' [`ecmwfr::wf_set_key()`]
#' @inheritParams ecmwfr::wf_request_batch
#' @inheritDotParams ecmwfr::wf_request_batch
#'
#' @return For `"cds"`, the output of [ecmwfr::wf_request_batch()]; for `"gcs"`, the paths of the
#' files written (invisibly).
#' @examplesIf FALSE
#'   tag_download_wind(tag)
#'   tag_download_wind(tag, source = "gcs")
#' @family movement
#' @seealso [`wf_request_batch()`
#' ](https://bluegreen-labs.github.io/ecmwfr/reference/wf_request.html),
#' [GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/trajectory-with-wind.html)
#' @export
tag_download_wind <- function(
  tag,
  extent = tag$param$tag_set_map$extent,
  include_stap_id = NULL,
  variable = c("u_component_of_wind", "v_component_of_wind"),
  file = wind_file_default,
  overwrite = FALSE,
  workers = if (source == "cds") 2 else 6,
  source = c("cds", "gcs"),
  cds_token = lifecycle::deprecated(),
  ...
) {
  source <- match.arg(source)
  pkg <- c(if (source == "cds") "ecmwfr" else "Rarr", "ncdf4")
  pkg <- pkg[!vapply(pkg, requireNamespace, TRUE, quietly = TRUE)]
  if (length(pkg) > 0) {
    cli::cli_abort(c(
      "x" = "Package{?s} {.pkg {pkg}} {?is/are} required for {.code tag_download_wind(source = \"{source}\")}.",
      "i" = "Install {?it/them} with {.code install.packages()} ({.code BiocManager::install()} for {.pkg Rarr})."
    ))
  }

  if (lifecycle::is_present(cds_token)) {
    lifecycle::deprecate_warn(
      "3.3.4",
      "tag_download_wind(cds_token)",
      "ecmwfr::wf_set_key(key)"
    )
    ecmwfr::wf_set_key(key = cds_token)
  }

  tag_assert(tag, "setmap")

  tag_id <- tag$param$id

  stap <- tag$stap

  assertthat::assert_that(length(extent) == 4)

  assertthat::assert_that(is.function(file))
  directory <- dirname(file(1, tag_id))
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    cli::cli_warn(c(
      "!" = "The directory {.file {directory}} did not exist.",
      ">" = "We created the directory."
    ))
  }

  if (is.null(include_stap_id)) {
    include_stap_id <- utils::head(tag$stap$stap_id, -1)

    # Take all stap_id without an existing wind file
    if (!overwrite) {
      include_stap_id <- include_stap_id[
        !file.exists(file(include_stap_id, tag_id))
      ]
    }
  }
  assertthat::assert_that(is.numeric(include_stap_id))
  assertthat::assert_that(all(include_stap_id %in% stap$stap_id))

  # CDS only understands the long names; reject short ones with the right spelling.
  variable <- era5_variable_canonical(variable)
  tbl <- era5_variable_pressure_level()
  gcs_variable <- tbl$variable[tbl$gcs]
  if (source == "gcs" && !all(variable %in% gcs_variable)) {
    cli::cli_abort(c(
      "x" = "{.val {setdiff(variable, gcs_variable)}} {?is/are} not available from Google ARCO-ERA5.",
      ">" = "Use {.code source = \"cds\"} instead."
    ))
  }

  # remove the last include_stap_id if it was added by mistake
  if (utils::tail(tag$stap$stap_id, 1) %in% include_stap_id) {
    include_stap_id <- utils::head(sort(include_stap_id), -1)
    cli::cli_warn(c(
      "!" = "{.var include_stap_id} included the last stationary period for which no wind can be computed.",
      ">" = "We removed this stationary period."
    ))
  }

  if (any(file.exists(file(include_stap_id, tag_id))) && !overwrite) {
    tmp <- file.exists(file(include_stap_id, tag_id))
    cli::cli_abort(c(
      "x" = "There are already ERA5 data file for stationary periods {.var {include_stap_id[tmp]}}",
      ">" = "Delete the corresponding file or use the argument {.code overwrite = TRUE}."
    ))
  }

  # see https://confluence.ecmwf.int/display/CKB/ERA5%3A+data+documentation#ERA5:datadocumentation-Levellistings
  possible_pressure <- c(
    1,
    2,
    3,
    5,
    7,
    10,
    20,
    30,
    50,
    70,
    seq(100, 250, 25),
    seq(300, 750, 50),
    seq(775, 1000, 25)
  )

  # create list of request
  request_list <- list()
  flight <- list()

  for (i_s in include_stap_id) {
    # Get the time series of the flight on a 1 hour resolution
    flight_time <- seq(
      round.POSIXt(stap$end[i_s] - 30 * 60, units = "hours"),
      round.POSIXt(stap$start[i_s + 1] + 30 * 60, units = "hours"),
      by = 60 * 60
    )

    # Find the pressure level needed during this flight
    flight_id <- flight_time[1] <= tag$pressure$date &
      tag$pressure$date <= utils::tail(flight_time, 1)
    flight_pressure <- tag$pressure$value[flight_id]
    flight_pressure <- flight_pressure[is.finite(flight_pressure)]
    if (length(flight_pressure) == 0) {
      cli::cli_abort(c(
        "x" = "No finite pressure observations are available for the flight between stationary periods {.val {stap$stap_id[i_s]}} and {.val {stap$stap_id[i_s + 1]}}.",
        "i" = "Flight period: {.val {format(flight_time[1], '%Y-%m-%d %H:%M UTC')}} to {.val {format(utils::tail(flight_time, 1), '%Y-%m-%d %H:%M UTC')}}.",
        "i" = "Pressure observations are required to select the ERA5 pressure levels for the wind download.",
        ">" = "Review the pressure data or exclude this flight with {.arg include_stap_id}."
      ))
    }
    pres_id_min <- min(
      sum(min(flight_pressure) >= possible_pressure),
      length(possible_pressure) - 1
    )
    pres_id_max <- min(
      sum(max(flight_pressure) > possible_pressure) + 1,
      length(possible_pressure)
    )
    flight_pres_id <- seq(pres_id_min, pres_id_max)

    # Make some check
    assertthat::assert_that(length(possible_pressure[flight_pres_id]) > 1)
    assertthat::assert_that(length(flight_time) > 1)

    flight[[i_s]] <- list(time = flight_time, pres_id = flight_pres_id)

    # Prepare the query
    request_list[[i_s]] <- list(
      dataset_short_name = "reanalysis-era5-pressure-levels",
      product_type = "reanalysis",
      data_format = "netcdf",
      variable = variable,
      pressure_level = possible_pressure[flight_pres_id],
      year = sort(unique(format(flight_time, "%Y"))),
      month = sort(unique(format(flight_time, "%m"))),
      day = sort(unique(format(flight_time, "%d"))),
      time = sort(unique(format(flight_time, "%H:%M"))),
      area = c(extent[4], extent[1], extent[3], extent[2]), # N, W, S, E
      target = basename(file(i_s, tag_id))
    )
  }

  if (source == "gcs") {
    # Google ARCO-ERA5 stores one chunk per hour and variable with all 37 levels (same order as
    # possible_pressure) over the globe: fetch `workers` chunks in parallel, crop, keep in memory.
    host <- "https://storage.googleapis.com"
    array <- "gcp-public-data-arco-era5/ar/full_37-1h-0p25deg-chunk-1.zarr-v3/{variable}"
    client <- era5_arco_client(cache = TRUE, host = host, auth = FALSE)

    # ERA5 0.25° grid inside extent, latitude from north to south as in CDS files
    lon <- seq(ceiling(extent[1] * 4), floor(extent[2] * 4)) / 4
    lat <- seq(floor(extent[4] * 4), ceiling(extent[3] * 4)) / 4
    id_lon <- round(lon %% 360 * 4) + 1
    id_lat <- round((90 - lat) * 4) + 1

    cli::cli_progress_bar(
      "Download ERA5 from Google ARCO-ERA5",
      total = sum(vapply(flight[include_stap_id], \(f) length(f$time), 0)) * length(variable)
    )
    for (i_s in include_stap_id) {
      ft <- flight[[i_s]]$time
      pres_id <- flight[[i_s]]$pres_id
      # zarr time index: hours since 1900-01-01
      id_time <- as.integer(difftime(ft, as.POSIXct("1900-01-01", tz = "UTC"), units = "hours"))
      data <- sapply(era5_variable_short(variable), simplify = FALSE, \(v) {
        array(NA_real_, c(length(lon), length(lat), length(pres_id), length(ft)))
      })
      # One chunk per hour and variable, all variables of the flight in the same parallel batches
      job <- expand.grid(t = seq_along(ft), v = seq_along(variable))
      path <- glue::glue("{glue::glue(array, variable = variable[job$v])}/{id_time[job$t]}.0.0.0")
      for (k in split(seq_along(path), ceiling(seq_along(path) / workers))) {
        resp <- httr2::req_perform_parallel(
          lapply(path[k], client$request),
          max_active = workers,
          progress = FALSE
        )
        for (i in seq_along(k)) {
          t <- job$t[k[i]]
          v <- job$v[k[i]]
          assign(path[k[i]], httr2::resp_body_raw(resp[[i]]), envir = client$cache)
          x <- Rarr::read_zarr_array(
            glue::glue("{host}/{glue::glue(array, variable = variable[v])}"),
            index = list(id_time[t] + 1, pres_id, id_lat, sort(id_lon)),
            s3_client = client
          )
          rm(list = path[k[i]], envir = client$cache)
          # time x level x lat x lon -> lon x lat x level (descending hPa)
          data[[v]][,,, t] <- aperm(x, 4:1)[
            match(id_lon, sort(id_lon)),
            ,
            rev(seq_along(pres_id)),
            1
          ]
          cli::cli_progress_update()
        }
      }
      era5_file_write(
        file(i_s, tag_id),
        data,
        lon,
        lat,
        rev(possible_pressure[pres_id]),
        ft,
        "Google ARCO-ERA5"
      )
    }
    cli::cli_progress_done()
    return(invisible(file(include_stap_id, tag_id)))
  }

  out <- ecmwfr::wf_request_batch(
    request_list[include_stap_id],
    workers = workers,
    path = directory,
    time_out = 3 * 3600,
    ...
  )

  # CDS returns every day x hour combination of the request: keep the flight hours and pack values
  for (i_s in include_stap_id[file.exists(file(include_stap_id, tag_id))]) {
    nc <- ncdf4::nc_open(file(i_s, tag_id))
    info <- edge_add_wind_nc_info(nc, file(i_s, tag_id))
    id_time <- match(flight[[i_s]]$time, info$time)
    data <- sapply(era5_variable_short(variable), simplify = FALSE, \(v) {
      ncdf4::ncvar_get(nc, v, collapse_degen = FALSE)[,,, id_time, drop = FALSE]
    })
    ncdf4::nc_close(nc)
    era5_file_write(
      file(i_s, tag_id),
      data,
      info$lon,
      info$lat,
      info$pres,
      flight[[i_s]]$time,
      "CDS"
    )
  }
  out
}


#' Default path of the ERA5 wind file of a stationary period
#'
#' @noRd
wind_file_default <- \(stap_id, tag_id) {
  glue::glue("./data/wind/{tag_id}/{tag_id}_{stap_id}.nc")
}


#' Write ERA5 pressure-level data as a compact CF NetCDF file
#'
#' Same layout as the CDS NetCDF read by [edge_add_wind()] (dimensions `longitude`, `latitude`,
#' `pressure_level` in descending hPa, `valid_time`; variables under their short names with the
#' attributes of [era5_variable_pressure_level()]), but values are stored as 16-bit integers with
#' `scale_factor` and `add_offset` (CF packing), which NetCDF readers unpack transparently. Wind
#' (`u`, `v`) is stored to 0.01 m/s; other variables use the full 16-bit resolution of their range.
#'
#' @param file path of the file to write.
#' @param data named list (short names) of arrays longitude x latitude x pressure level x time.
#' @param lon,lat,pres,time coordinates (`time` as POSIXct).
#' @param source data source written in the global attribute `source`.
#' @noRd
era5_file_write <- function(file, data, lon, lat, pres, time, source) {
  tbl <- era5_variable_pressure_level()
  tbl <- tbl[match(names(data), tbl$short_name), ]
  dims <- list(
    ncdf4::ncdim_def("longitude", "degrees_east", lon, longname = "longitude"),
    ncdf4::ncdim_def("latitude", "degrees_north", lat, longname = "latitude"),
    ncdf4::ncdim_def("pressure_level", "hPa", pres, longname = "pressure"),
    ncdf4::ncdim_def(
      "valid_time",
      "seconds since 1970-01-01",
      as.numeric(time),
      calendar = "proleptic_gregorian",
      longname = "time"
    )
  )
  def <- lapply(seq_len(nrow(tbl)), \(i) {
    ncdf4::ncvar_def(
      tbl$short_name[i],
      tbl$units[i],
      dims,
      -32768L,
      longname = tbl$long_name[i],
      prec = "short",
      compression = 9,
      shuffle = TRUE
    )
  })
  # Write to a temporary file so an interrupted write is not mistaken for a complete file
  tmp <- glue::glue("{file}.part")
  nc <- ncdf4::nc_create(tmp, def, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.7")
  ncdf4::ncatt_put(nc, 0, "source", glue::glue("ERA5 hourly data on pressure levels ({source})"))
  for (d in c("longitude", "latitude")) {
    ncdf4::ncatt_put(nc, d, "standard_name", d)
  }
  ncdf4::ncatt_put(nc, "pressure_level", "standard_name", "air_pressure")
  ncdf4::ncatt_put(nc, "pressure_level", "positive", "down")
  ncdf4::ncatt_put(nc, "valid_time", "standard_name", "time")
  for (i in seq_along(def)) {
    if (!is.na(tbl$standard_name[i])) {
      ncdf4::ncatt_put(nc, def[[i]], "standard_name", tbl$standard_name[i])
    }
    r <- range(data[[i]], na.rm = TRUE)
    # 65534 steps keep packed values within +/-32767, clear of the -32768 fill value; the 1e-12
    # floor avoids a zero scale (division by zero) for a constant field
    step <- max(if (tbl$short_name[i] %in% c("u", "v")) 0.01 else 0, diff(r) / 65534, 1e-12)
    ncdf4::ncatt_put(nc, def[[i]], "scale_factor", step, prec = "double")
    ncdf4::ncatt_put(nc, def[[i]], "add_offset", mean(r), prec = "double")
    ncdf4::ncvar_put(nc, def[[i]], round((data[[i]] - mean(r)) / step))
  }
  ncdf4::nc_close(nc)
  file.rename(tmp, file)
}
