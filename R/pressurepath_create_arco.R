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
  debug = FALSE
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
    debug = debug
  )
}

pressurepath_create_arco_impl <- function(
  tag,
  path = tag2path(tag),
  pressurepath,
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = c("single-levels", "land", "both"),
  preprocess = FALSE,
  quiet = FALSE,
  debug = FALSE
) {
  era5_dataset <- match.arg(era5_dataset)
  era5_dataset_deprecate_altitude(era5_dataset, "altitude" %in% variable)
  pressurepath_variable_check(variable, "arco", era5_dataset)

  cache_dir <- tools::R_user_dir("GeoPressureR", "cache")
  dataset <- rep(era5_dataset, nrow(pressurepath))
  if (era5_dataset == "both") {
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
    land_lon <- floor(pressurepath$lon / 0.1 + 0.5) * 0.1
    land_lat <- floor(pressurepath$lat / 0.1 + 0.5) * 0.1
    land_value <- terra::extract(land, cbind(land_lon, land_lat))[[1]]
    dataset <- ifelse(!is.na(land_value) & land_value > 0.5, "land", "single-levels")
  }

  resolution <- ifelse(dataset == "land", 0.1, 0.25)
  query_lon <- floor(pressurepath$lon / resolution + 0.5) * resolution
  query_lat <- ifelse(
    dataset == "land",
    ceiling(pressurepath$lat / resolution - 0.5) * resolution,
    floor(pressurepath$lat / resolution + 0.5) * resolution
  )
  requested_date <- as.POSIXct(pressurepath$date, tz = "UTC")
  first_hour <- ceiling(min(as.numeric(requested_date)) / 3600)
  requested_hour <- as.POSIXct(
    pmax(first_hour, ceiling(as.numeric(requested_date) / 3600 - 0.5)) * 3600,
    origin = "1970-01-01",
    tz = "UTC"
  )

  if (!quiet) {
    cli::cli_progress_step("Read ERA5 surface pressure from the ECMWF ARCO archive")
  }
  surface_pressure <- rep(NA_real_, nrow(pressurepath))
  for (dataset_i in unique(dataset)) {
    id <- dataset == dataset_i
    surface_pressure[id] <- era5_arco_read_points(
      variable = "surface_pressure",
      era5_dataset = dataset_i,
      lon = query_lon[id],
      lat = query_lat[id],
      date = requested_hour[id],
      debug = debug
    )
  }
  if (era5_dataset == "both" && anyNA(surface_pressure)) {
    id <- is.na(surface_pressure)
    dataset[id] <- "single-levels"
    resolution[id] <- 0.25
    query_lon[id] <- floor(pressurepath$lon[id] / resolution[id] + 0.5) * resolution[id]
    query_lat[id] <- floor(pressurepath$lat[id] / resolution[id] + 0.5) * resolution[id]
    surface_pressure[id] <- era5_arco_read_points(
      variable = "surface_pressure",
      era5_dataset = "single-levels",
      lon = query_lon[id],
      lat = query_lat[id],
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
        lon = query_lon[id],
        lat = query_lat[id],
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
    elevation <- era5_surface_elevation(query_lon, query_lat, dataset, quiet)
    pressurepath$altitude <- pressure_to_altitude(
      pressurepath$pressure_tag * 100,
      surface_pressure,
      temperature,
      elevation
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
