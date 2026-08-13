#' Create a pressure path directly from ERA5 ARCO
#'
#' `pressurepath_create_arco()` is a client-side alternative to [pressurepath_create()] for its
#' standard surface-pressure and altitude workflow. It interpolates the supplied path during
#' flights, groups observations by ERA5 grid cell, and reads the required Zarr chunks directly
#' from the ECMWF Analysis-Ready Cloud-Optimised (ARCO) archive.
#'
#' Surface pressure is retrieved for every retained tag observation. Altitude is computed from
#' tag pressure, ERA5 surface pressure, 2 m temperature, and the time-invariant surface
#' geopotential using the same barometric equation as [geopressure_timeseries_arco()]. Set
#' `compute_altitude = FALSE` when altitude is not needed.
#'
#' The returned `lat` and `lon` remain the supplied or interpolated bird coordinates. ERA5 values
#' are sampled at the nearest 0.1 degree ERA5-Land cell or 0.25 degree ERA5 cell. With
#' `era5_dataset = "both"`, the ERA5-Land land-sea mask selects ERA5-Land over land and global ERA5
#' elsewhere.
#'
#' The function currently supports the standard `surface_pressure` and optional `altitude`
#' variables only. Use [pressurepath_create()] to retrieve its wider set of ERA5 variables.
#'
#' @inheritParams pressurepath_create
#' @param compute_altitude Logical scalar. If `TRUE`, compute altitude from tag pressure, ERA5
#'   surface pressure, 2 m temperature, and surface geopotential.
#' @param era5_dataset ERA5 product to use: `"land"` for ERA5-Land, `"single-levels"` for global
#'   ERA5, or `"both"` to use ERA5-Land over land and global ERA5 elsewhere.
#' @param quiet Logical scalar to suppress progress messages.
#' @param debug Logical scalar to display the selected Zarr paths and array indices.
#'
#' @return A `pressurepath` data.frame with the same standard columns and attributes as
#'   [pressurepath_create()]. It contains `surface_pressure`, `surface_pressure_norm`, and, when
#'   `compute_altitude = TRUE`, `altitude`.
#'
#' @examplesIf FALSE
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#' path <- data.frame(
#'   stap_id = tag$stap$stap_id,
#'   lat = c(48.5, 32.5, 30.5, 49.5, 41.6),
#'   lon = c(17.5, 13.5, 16.5, 21.5, 12.7)
#' )
#' pressurepath <- pressurepath_create_arco(tag, path, quiet = TRUE)
#'
#' @family pressurepath
#' @export
pressurepath_create_arco <- function(
  tag,
  path = tag2path(tag),
  compute_altitude = TRUE,
  solar_dep = 0,
  era5_dataset = c("both", "land", "single-levels"),
  preprocess = FALSE,
  quiet = FALSE,
  debug = FALSE
) {
  tag_assert(tag, "stap")
  assertthat::assert_that(is.logical(compute_altitude), length(compute_altitude) == 1)
  assertthat::assert_that(is.logical(preprocess))
  assertthat::assert_that(is.logical(quiet))
  era5_dataset <- match.arg(era5_dataset)

  if (!requireNamespace("Rarr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg Rarr} is required for {.fun pressurepath_create_arco}.",
      "i" = "Install it with {.run BiocManager::install('Rarr')}."
    ))
  }
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ecmwfr} is required for {.fun pressurepath_create_arco}.",
      "i" = "Install it with {.run install.packages('ecmwfr')}."
    ))
  }

  if (!quiet) {
    cli::cli_progress_step("Prepare pressure path")
  }
  pressure <- if (preprocess) {
    geopressure_map_preprocess(tag, compute_known = TRUE)
  } else {
    tag$pressure
  }
  assertthat::assert_that(nrow(pressure) > 0)
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon", "stap_id")))
  if (nrow(path) == 0) {
    cli::cli_abort("{.var path} is empty.")
  }
  if (!all(path$stap_id %in% pressure$stap_id)) {
    cli::cli_warn("Some {.field stap_id} of {.var path} are not present in {.var tag$pressure}.")
  }

  stap_id_interp <- pressure$stap_id
  id <- stap_id_interp == 0
  sequence <- seq_len(nrow(pressure))
  stap_id_interp[id] <- stats::approx(
    sequence[!id],
    stap_id_interp[!id],
    sequence[id],
    rule = 2
  )$y
  id <- ceiling(stap_id_interp) %in%
    path$stap_id[!is.na(path$lon)] &
    floor(stap_id_interp) %in% path$stap_id[!is.na(path$lon)]
  pressurepath <- merge(
    pressure[id, ],
    path[, intersect(c("stap_id", "lat", "lon", "j"), names(path)), drop = FALSE],
    by = "stap_id",
    all.x = TRUE
  )
  pressurepath <- pressurepath[order(pressurepath$date), ]
  names(pressurepath)[names(pressurepath) == "value"] <- "pressure_tag"

  id <- pressurepath$stap_id != round(pressurepath$stap_id)
  sequence <- seq_len(nrow(pressurepath))
  pressurepath$lat[id] <- stats::approx(
    sequence[!id],
    pressurepath$lat[!id],
    sequence[id],
    rule = 1
  )$y
  pressurepath$lon[id] <- stats::approx(
    sequence[!id],
    pressurepath$lon[!id],
    sequence[id],
    rule = 1
  )$y

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
      variable = "sp",
      era5_dataset = dataset_i,
      lon = query_lon[id],
      lat = query_lat[id],
      date = requested_hour[id],
      debug = debug
    )
  }

  if (compute_altitude) {
    if (!quiet) {
      cli::cli_progress_step("Read ERA5 temperature and compute altitude")
    }
    temperature <- elevation <- rep(NA_real_, nrow(pressurepath))
    for (dataset_i in unique(dataset)) {
      id <- dataset == dataset_i
      temperature[id] <- era5_arco_read_points(
        variable = "t2m",
        era5_dataset = dataset_i,
        lon = query_lon[id],
        lat = query_lat[id],
        date = requested_hour[id],
        debug = debug
      )
      if (dataset_i == "land") {
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
      terra::ext(geopotential) <- if (dataset_i == "land") {
        c(-0.05, 359.95, -90.05, 90.05)
      } else {
        c(-0.125, 359.875, -90.125, 90.125)
      }
      geopotential <- terra::rotate(geopotential)
      elevation[id] <- terra::extract(
        geopotential,
        cbind(query_lon[id], query_lat[id])
      )[[1]] /
        9.80665
    }
    pressurepath$altitude <- elevation +
      temperature /
        -0.0065 *
        ((pressurepath$pressure_tag * 100 / surface_pressure)^(-8.31432 *
          -0.0065 /
          9.80665 /
          0.0289644) -
          1)
  }
  pressurepath$surface_pressure <- surface_pressure / 100

  pp <- pressurepath
  pp$stapelev <- paste(
    pp$stap_id,
    ifelse(startsWith(pp$label, "elev_"), gsub("^.*?elev_", "", pp$label), "0"),
    sep = "|"
  )
  pp$stapelev_label <- pp$stapelev
  pp$stapelev_label[pp$label == "discard"] <- 0
  agg <- merge(
    stats::aggregate(
      surface_pressure ~ stapelev_label,
      data = pp,
      FUN = \(x) mean(x, na.rm = TRUE)
    ),
    stats::aggregate(
      pressure_tag ~ stapelev_label,
      data = pp,
      FUN = \(x) mean(x, na.rm = TRUE)
    )
  )
  id <- match(pp$stapelev, agg$stapelev)
  pressurepath$surface_pressure_norm <- pressurepath$surface_pressure -
    agg$surface_pressure[id] +
    agg$pressure_tag[id]

  if (!is.null(solar_dep)) {
    twl <- path2twilight(pressurepath, solar_dep = solar_dep, return_long = FALSE)
    pressurepath <- merge(pressurepath, twl[, c("date", "sunset", "sunrise")])
  }
  pressurepath <- pressurepath[c("date", setdiff(names(pressurepath), "date"))]

  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "sd") <- tag$param$geopressure_map$sd
  attr(pressurepath, "type") <- attr(path, "type")
  return(pressurepath)
}

era5_arco_read_points <- function(variable, era5_dataset, lon, lat, date, debug) {
  out <- rep(NA_real_, length(date))
  if (era5_dataset == "land") {
    store <- switch(
      variable,
      sp = "cadl-arco-geo-009/arco/reanalysis_era5_land/sfc-pressure-precipitation",
      t2m = "cadl-arco-geo-007/arco/reanalysis_era5_land/sfc-2m-temperature"
    )
    time_index <- as.integer(as.numeric(date) / 3600 - (-175296) + 1)
    lat_index <- as.integer(round((lat + 90) * 10) + 1)
    lon_index <- ifelse(lon == -180, 3600L, as.integer(round((lon + 179.9) * 10) + 1))
    chunk_shape <- c(33792L, 4L, 8L)
  } else {
    store <- "cadl-arco-geo-002/arco/reanalysis_era5_single_levels/sfc"
    time_index <- as.integer((as.numeric(date) - (-946771200)) / 3600 + 1)
    lat_index <- as.integer(round((lat + 90) * 4) + 1)
    lon_index <- ifelse(lon == 180, 1L, as.integer(round((lon + 180) * 4) + 1))
    chunk_shape <- c(67584L, 4L, 4L)
  }
  array <- glue::glue(
    "https://arco.datastores.ecmwf.int/{store}/geoChunked.zarr/{variable}"
  )

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
