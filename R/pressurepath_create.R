#' Create a pressure path
#'
#' `pressurepath_create()` combines a tag pressure series with a path and retrieves matching ERA5
#' variables. Positions during flights are linearly interpolated between stationary periods.
#'
#' @section Data sources:
#' `source = "arco"` reads ECMWF's Analysis-Ready Cloud-Optimised (ARCO) archive directly. It
#' requires an ECMWF API key and the optional `Rarr` and `ecmwfr` packages. `variable` uses the
#' same names as the hosted API, but the archive carries fewer of them: 20 for
#' `era5_dataset = "single-levels"`, 16 for `"land"`, and the 8 they share for `"both"`, plus the
#' derived `"altitude"`. Use [pressurepath_variable_available()] to list them.
#'
#' `source = "api"` uses the hosted GeoPressureAPI. It needs no ECMWF key or `Rarr` installation
#' and reaches the full Earth Engine band set: 292 variables for `era5_dataset = "single-levels"`
#' and `"both"`, 70 for `"land"`.
#'
#' The default, `source = "auto"`, uses ARCO when a key stored by [ecmwfr::wf_set_key()] is
#' available and GeoPressureAPI otherwise. Use [pressurepath_create_arco()] or
#' [pressurepath_create_api()] to select a backend explicitly.
#'
#' @section Path and ERA5 processing:
#' Measurements are retained only when `path` contains the surrounding stationary periods.
#' Coordinates during flights are interpolated linearly, while ERA5 values are sampled at the
#' nearest grid cell and hour. `era5_dataset = "single-levels"` uses 0.25 degree global ERA5,
#' `"land"` uses 0.1 degree ERA5-Land, and `"both"` selects ERA5-Land over land and global ERA5
#' over water.
#'
#' Surface pressure is returned in hPa and normalised to the mean tag pressure within each
#' stationary-period and elevation-label group. Observations labelled `"discard"` are excluded
#' from those means. If `solar_dep` is not `NULL`, local sunrise and sunset are added.
#'
#' @template pressure-altitude
#'
#' @template ecmwf-key
#' @param tag A GeoPressureR `tag` object.
#' @param path A GeoPressureR `path` data.frame.
#' @param variable ERA5 variables to retrieve, named as in the
#'   [ERA5 catalogue](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels),
#'   plus the derived `"altitude"`. The same names work for every `source`, but the available set
#'   depends on both `source` and `era5_dataset` — see [pressurepath_variable_available()].
#' @param solar_dep Solar depression angle used to compute sunrise and sunset, or `NULL` to skip
#'   this computation.
#' @param era5_dataset ERA5 product: `"both"` (default) to use ERA5-Land over land and global ERA5
#'   elsewhere, `"land"`, or `"single-levels"`. Prefer `"single-levels"` whenever `variable`
#'   includes `"altitude"`; see the *Choosing `era5_dataset`* section.
#' @param preprocess Whether to preprocess pressure with [geopressure_map_preprocess()].
#' @param workers Number of parallel GeoPressureAPI requests, or `"auto"`.
#' @param source Data source: `"auto"`, `"arco"`, or `"api"`.
#' @param quiet Logical to suppress progress messages.
#' @param debug Logical to display request details.
#'
#' @return A `pressurepath` data.frame containing tag pressure, coordinates, requested ERA5
#'   variables, normalised surface pressure, and optional sunrise and sunset times.
#'
#' @examplesIf FALSE
#' pressurepath <- pressurepath_create(tag, path)
#'
#' @family pressurepath
#' @export
pressurepath_create <- function(
  tag,
  path = tag2path(tag),
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "both",
  preprocess = FALSE,
  workers = "auto",
  quiet = FALSE,
  debug = FALSE,
  source = c("auto", "arco", "api")
) {
  era5_dataset <- match.arg(era5_dataset, c("both", "land", "single-levels"))
  assertthat::assert_that(is.logical(quiet))
  source <- ecmwf_select_source(source, quiet)

  # Validate once, here, where both the backend and the product are resolved. The available sets
  # differ per (source, era5_dataset) pair, and an unavailable variable makes GeoPressureAPI
  # return an empty response that blanks every column rather than erroring.
  pressurepath_variable_check(variable, source, era5_dataset)

  if (source == "arco") {
    arco_require_dependencies()
    pressurepath <- pressurepath_prepare(tag, path, preprocess, quiet)
    return(pressurepath_create_arco_impl(
      tag = tag,
      path = path,
      pressurepath = pressurepath,
      variable = variable,
      solar_dep = solar_dep,
      era5_dataset = era5_dataset,
      preprocess = preprocess,
      quiet = quiet,
      debug = debug
    ))
  }

  pressurepath <- pressurepath_prepare(tag, path, preprocess, quiet)
  pressurepath_create_api_impl(
    tag = tag,
    path = path,
    pressurepath = pressurepath,
    variable = variable,
    solar_dep = solar_dep,
    era5_dataset = era5_dataset,
    preprocess = preprocess,
    workers = workers,
    quiet = quiet,
    debug = debug
  )
}

pressurepath_prepare <- function(tag, path, preprocess, quiet) {
  tag_assert(tag, "stap")
  assertthat::assert_that(is.logical(preprocess))
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
  pressurepath
}

#' Shape and annotate a pressure path
#'
#' Both backends hand over `surface_pressure` in Pa, the unit every ERA5 source uses, and this is
#' the one place it becomes hPa. The column order and the recorded provenance are likewise defined
#' here alone, so a path is indistinguishable whichever `source` produced it.
#'
#' @param pressurepath Assembled path, with ERA5 variables attached and `surface_pressure` in Pa.
#' @param variable Variables the caller requested, in that order.
#' @param era5_dataset,source Resolved ERA5 product and backend, recorded as attributes because
#'   they change the result materially and cannot otherwise be recovered from a saved path.
#' @noRd
pressurepath_finalize <- function(
  pressurepath,
  tag,
  path,
  preprocess,
  solar_dep,
  variable = character(),
  era5_dataset = NA_character_,
  source = NA_character_
) {
  if ("surface_pressure" %in% names(pressurepath) && !all(is.na(pressurepath$surface_pressure))) {
    pressurepath$surface_pressure <- pressurepath$surface_pressure / 100
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
  }

  if (!is.null(solar_dep)) {
    twl <- path2twilight(pressurepath, solar_dep = solar_dep, return_long = FALSE)
    pressurepath <- merge(pressurepath, twl[, c("date", "sunset", "sunrise")])
  }

  # Fixed column order: identifiers, position, the requested variables in the order asked for,
  # anything else the path carried, then the derived columns. `setdiff` keeps unknown columns
  # rather than dropping them.
  lead <- c("date", "stap_id", "pressure_tag", "label", "lat", "lon")
  trail <- c("surface_pressure_norm", "sunset", "sunrise")
  pressurepath <- pressurepath[c(
    intersect(lead, names(pressurepath)),
    intersect(variable, names(pressurepath)),
    setdiff(names(pressurepath), c(lead, variable, trail)),
    intersect(trail, names(pressurepath))
  )]

  attr(pressurepath, "id") <- tag$param$id
  attr(pressurepath, "preprocess") <- preprocess
  attr(pressurepath, "sd") <- tag$param$geopressure_map$sd
  attr(pressurepath, "type") <- attr(path, "type")
  # Provenance: altitude differs by tens of metres between ERA5 products, so a saved path has to
  # say which one made it.
  attr(pressurepath, "variable") <- variable
  attr(pressurepath, "era5_dataset") <- era5_dataset
  attr(pressurepath, "source") <- source
  pressurepath
}
