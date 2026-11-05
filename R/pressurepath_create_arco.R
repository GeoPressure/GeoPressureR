#' @rdname pressurepath_create
#' @export
pressurepath_create_arco <- function(
  tag,
  path = tag2path(tag),
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "single-levels",
  preprocess = FALSE,
  quiet = FALSE,
  debug = FALSE,
  altitude_formula = "virtual"
) {
  pressurepath_create(
    tag = tag,
    path = path,
    variable = variable,
    solar_dep = solar_dep,
    era5_dataset = era5_dataset,
    preprocess = preprocess,
    source = "arco",
    quiet = quiet,
    debug = debug,
    altitude_formula = altitude_formula
  )
}

pressurepath_create_arco_impl <- function(
  tag,
  path = tag2path(tag),
  pressurepath,
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "single-levels",
  preprocess = FALSE,
  quiet = FALSE,
  debug = FALSE,
  altitude_formula = "virtual"
) {
  dataset <- rep(era5_dataset, nrow(pressurepath))
  if (era5_dataset == "both") {
    # Same grid cell as the ERA5-Land data read below
    land <- era5_grid_snap(pressurepath$lon, pressurepath$lat, "land")
    land_value <- terra::extract(era5_land_sea_mask(), cbind(land$lon, land$lat))[[1]]
    dataset <- ifelse(!is.na(land_value) & land_value > 0.5, "land", "single-levels")
  }

  query <- era5_grid_snap(pressurepath$lon, pressurepath$lat, dataset)
  requested_date <- as.POSIXct(pressurepath$date, tz = "UTC")
  requested_hour <- era5_hour_round(requested_date)

  if (!quiet) {
    cli::cli_progress_step("Read ERA5 surface pressure from the ECMWF ARCO archive")
  }
  surface_pressure <- rep(NA_real_, nrow(pressurepath))
  for (dataset_i in unique(dataset)) {
    id <- dataset == dataset_i
    surface_pressure[id] <- era5_arco_read_points(
      variable = "surface_pressure",
      era5_dataset = dataset_i,
      lon = query$lon[id],
      lat = query$lat[id],
      date = requested_hour[id],
      debug = debug
    )
  }
  if (era5_dataset == "both" && anyNA(surface_pressure)) {
    id <- is.na(surface_pressure)
    dataset[id] <- "single-levels"
    query <- era5_grid_snap(pressurepath$lon, pressurepath$lat, dataset)
    surface_pressure[id] <- era5_arco_read_points(
      variable = "surface_pressure",
      era5_dataset = "single-levels",
      lon = query$lon[id],
      lat = query$lat[id],
      date = requested_hour[id],
      debug = debug
    )
  }

  # `dataset` is final from here on: the "both" fallback above has already moved any point the
  # land stores could not serve onto single levels, so every variable is read from the same
  # product as the surface pressure it will sit beside.
  read_variable <- function(v) {
    out <- rep(NA_real_, nrow(pressurepath))
    for (dataset_i in unique(dataset)) {
      id <- dataset == dataset_i
      out[id] <- era5_arco_read_points(
        variable = v,
        era5_dataset = dataset_i,
        lon = query$lon[id],
        lat = query$lat[id],
        date = requested_hour[id],
        debug = debug
      )
    }
    out
  }

  if ("altitude" %in% variable) {
    if (!quiet) {
      cli::cli_progress_step("Read ERA5 temperature and compute altitude")
    }
    temperature <- read_variable("temperature_2m")
    elevation <- era5_surface_elevation(query$lon, query$lat, dataset, quiet)
    pressurepath$altitude <- pressure_to_altitude(
      pressurepath$pressure_tag * 100,
      surface_pressure,
      temperature,
      elevation,
      dewpoint = if (altitude_formula == "virtual") read_variable("dewpoint_temperature_2m"),
      lon = pressurepath$lon,
      lat = pressurepath$lat,
      date = requested_date,
      altitude_formula = altitude_formula
    )
  }
  # Everything else the caller asked for. `surface_pressure` is already in hand and `altitude` is
  # derived, so neither is re-read; the rest keep their native ERA5 units.
  extra <- setdiff(variable, c("altitude", "surface_pressure"))
  if (length(extra) > 0) {
    if (!quiet) {
      cli::cli_progress_step("Read {length(extra)} further ERA5 variable{?s} from ARCO")
    }
    for (v in extra) {
      pressurepath[[v]] <- read_variable(v)
    }
  }

  # Handed over in Pa; `pressurepath_finalize()` is the single place that converts to hPa. Only
  # attached when asked for, matching the API backend.
  if ("surface_pressure" %in% variable) {
    pressurepath$surface_pressure <- surface_pressure
  }
  pressurepath_finalize(
    pressurepath,
    tag,
    path,
    preprocess,
    solar_dep,
    variable = variable,
    era5_dataset = era5_dataset,
    source = "arco"
  )
}

era5_arco_read_points <- function(variable, era5_dataset, lon, lat, date, debug) {
  out <- rep(NA_real_, length(date))
  # Geometry depends only on the product: every ERA5-Land store shares one grid and time axis,
  # and so does every single-levels array. Only the store path varies with the variable.
  if (era5_dataset == "land") {
    time_index <- as.integer(as.numeric(date) / 3600 - (-175296) + 1)
    lat_index <- as.integer(round((lat + 90) * 10) + 1)
    lon_index <- ifelse(lon == -180, 3600L, as.integer(round((lon + 179.9) * 10) + 1))
    chunk_shape <- c(33792L, 4L, 8L)
  } else {
    time_index <- as.integer((as.numeric(date) - (-946771200)) / 3600 + 1)
    lat_index <- as.integer(round((lat + 90) * 4) + 1)
    lon_index <- ifelse(lon == 180, 1L, as.integer(round((lon + 180) * 4) + 1))
    chunk_shape <- c(67584L, 4L, 4L)
  }
  array <- era5_arco_array(variable, era5_dataset)

  # Read each physical chunk once when several path cells share it.
  chunks <- split(
    seq_along(date),
    paste(
      (time_index - 1L) %/% chunk_shape[1],
      (lat_index - 1L) %/% chunk_shape[2],
      (lon_index - 1L) %/% chunk_shape[3],
      sep = "."
    )
  )
  arco_client <- era5_arco_client(cache = TRUE)
  for (id in chunks) {
    time_i <- sort(unique(time_index[id]))
    lat_i <- sort(unique(lat_index[id]))
    lon_i <- sort(unique(lon_index[id]))
    if (debug) {
      cli::cli_text(
        "Read {.field {variable}} chunk for {length(id)} point{?s}: indexes {range(time_i)}, {range(lat_i)}, {range(lon_i)} from {.url {array}}"
      )
    }
    value <- Rarr::read_zarr_array(
      array,
      index = list(time_i, lat_i, lon_i),
      s3_client = arco_client
    )
    out[id] <- value[cbind(
      match(time_index[id], time_i),
      match(lat_index[id], lat_i),
      match(lon_index[id], lon_i)
    )]
  }
  out
}
