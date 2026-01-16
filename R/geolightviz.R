#' Start the GeoLightViz shiny app
#'
#' @param x a GeoPressureR `tag` object, a `.Rdata` file or the
#' unique identifier `id` with a `.Rdata` file located in `"./data/interim/{id}.RData"`.
#' @param path a GeoPressureR `path` or `pressurepath` data.frame.
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @param run_bg If true (by default), the app runs in a background R process using `callr::r_bg()`,
#' allowing you to continue using the R console. If false, the app blocks the console until closed.
#'
#' @export
geolightviz <- function(
  x,
  path = NULL,
  launch_browser = TRUE,
  run_bg = TRUE,
  quiet = FALSE,
  ...
) {
  # Resolve tag input from object, file, or id.
  is_x_char <- is.character(x) && length(x) == 1
  is_x_file <- is_x_char && file.exists(x)
  is_x_interim <- is_x_char &&
    file.exists(glue::glue("./data/interim/{x}.RData"))
  is_x_rawtag <- is_x_char &&
    dir.exists(glue::glue("./data/raw-tag/{x}"))

  if (inherits(x, "tag")) {
    tag <- x
  } else if (is_x_file) {
    tag <- file2obj(x, obj = "tag")
  } else if (is_x_interim) {
    tag <- file2obj(glue::glue("./data/interim/{x}.RData"), obj = "tag")
  } else if (is_x_rawtag) {
    tag <- list(param = list(id = x))
  } else {
    cli::cli_abort(
      "The first argument {.var x} needs to be a {.cls tag}, a interim {.field file} or an {.field id}"
    )
  }

  id <- tag$param$id

  # Load config and ensure tag availability.
  if (file.exists(Sys.getenv("R_CONFIG_FILE", "config.yml"))) {
    # Create the config file
    config <- GeoPressureR::geopressuretemplate_config(
      id,
      config = config::get(config = id),
      ...
    )
  } else {
    config <- GeoPressureR::param_create(id, default = TRUE)
  }

  config$tag_create$assert_pressure <- FALSE
  if (!inherits(tag, "tag")) {
    tag <- do.call(
      GeoPressureR::tag_create,
      c(
        list(id = id, quiet = quiet),
        config$tag_create
      )
    )
  }

  GeoPressureR::tag_assert(tag, "light")

  # Ensure geolight parameters and map defaults are present.
  if (is.null(tag$param)) {
    tag$param <- list()
  }
  if (is.null(tag$param$geolight_map)) {
    tag$param$geolight_map <- list()
  }
  default_twl_calib_adjust <- formals(GeoPressureR::geolight_map)[["twl_calib_adjust"]]
  if (!is.null(config$geolight_map[["twl_calib_adjust"]])) {
    default_twl_calib_adjust <- config$geolight_map[["twl_calib_adjust"]]
  }
  if (is.null(tag$param$geolight_map[["compute_known"]])) {
    tag$param$geolight_map[["compute_known"]] <- FALSE
  }
  if (is.null(tag$param$geolight_map[["fitted_location_duration"]])) {
    tag$param$geolight_map[["fitted_location_duration"]] <- Inf
  }
  if (is.null(tag$param$geolight_map[["twl_calib_adjust"]])) {
    tag$param$geolight_map[["twl_calib_adjust"]] <- default_twl_calib_adjust
  }
  if (is.null(tag$param$geolight_map[["twl_calib"]])) {
    tag$param$geolight_map[["twl_calib"]] <- NULL
  }
  if (is.null(tag$param$tag_set_map)) {
    tag$param$tag_set_map <- config$tag_set_map
  }
  if (!is.null(tag$param$tag_set_map)) {
    if (
      is.null(tag$param$tag_set_map$extent) ||
        is.null(tag$param$tag_set_map$scale)
    ) {
      cli::cli_abort(
        "tag_set_map must include {.field extent} and {.field scale}."
      )
    }
  }

  # If twilight has not been computed we need to be able to display it
  if (!("twilight" %in% names(tag))) {
    tag <- do.call(
      twilight_create,
      c(
        list(tag = tag),
        config$twilight_create
      )
    )
  }

  if (!("label" %in% names(tag$twilight))) {
    tryCatch(
      {
        tag <- do.call(
          twilight_label_read,
          c(
            list(tag = tag),
            config$twilight_label_read
          )
        )
      },
      error = function(e) NA_character_
    )
  }

  # Build map grid for known-position bounds checks.
  g <- NULL
  if (!is.null(tag$param$tag_set_map)) {
    g <- GeoPressureR::map_expand(
      tag$param$tag_set_map$extent,
      tag$param$tag_set_map$scale
    )
  }

  # Prepare stapath for app input.
  if (!is.null(path)) {
    stap_input <- path
  } else if ("stap" %in% names(tag)) {
    stap_input <- tag$stap
  } else {
    stap_input <- tag
  }

  stapath <- GeoPressureR::read_stap(stap_input)

  if (!("stap_id" %in% names(stapath))) {
    stapath$stap_id <- seq_len(nrow(stapath))
  }
  if (!("lat" %in% names(stapath))) {
    stapath$lat <- NA_real_
  }
  if (!("lon" %in% names(stapath))) {
    stapath$lon <- NA_real_
  }
  if (!("known_lat" %in% names(stapath))) {
    stapath$known_lat <- NA_real_
  }
  if (!("known_lon" %in% names(stapath))) {
    stapath$known_lon <- NA_real_
  }
  stapath$lat[!is.na(stapath$known_lat)] <- stapath$known_lat[
    !is.na(stapath$known_lat)
  ]
  stapath$lon[!is.na(stapath$known_lon)] <- stapath$known_lon[
    !is.na(stapath$known_lon)
  ]
  if (!("duration" %in% names(stapath))) {
    stapath$duration <- GeoPressureR::stap2duration(stapath)
  }

  tag$stap <- stapath

  # Validate known positions are inside map extent.
  if (!is.null(g)) {
    known_idx <- !is.na(stapath$known_lat) & !is.na(stapath$known_lon)
    if (any(known_idx)) {
      out_of_bounds <- !(stapath$known_lon[known_idx] >= g$extent[1] &
        stapath$known_lon[known_idx] <= g$extent[2] &
        stapath$known_lat[known_idx] >= g$extent[3] &
        stapath$known_lat[known_idx] <= g$extent[4])
      if (any(out_of_bounds)) {
        cli::cli_abort(c(
          x = "The known latitude and longitude are not inside the map extent",
          i = "Modify {.var extent} or {.var known} to match this requirement."
        ))
      }
    }
  }

  tag$twilight$stap_id <- GeoPressureR:::find_stap(
    stapath,
    tag$twilight$twilight
  )

  compute_known <- tag$param$geolight_map[["compute_known"]]

  # Build light and twilight traces for the app.
  light_trace <- light_matrix(tag)
  twl <- prepare_twilight(
    tag,
    ref = light_trace$time[1],
    compute_known = compute_known
  )

  # Run the app in a background process or in the current session.
  if (run_bg) {
    return(shiny_run_app_bg(
      system.file("geolightviz", package = "GeoPressureR"),
      shiny_opts = list(
        tag = tag,
        stapath = stapath,
        light_trace = light_trace,
        twl = twl,
        stop_on_session_end = FALSE
      ),
      launch_browser = launch_browser,
      proc_option = "GeoPressureR.geolightviz_processes",
      proc_id = tag$param$id,
      app_label = "GeoLightViz"
    ))
  }

  shiny_run_app_fg(
    system.file("geolightviz", package = "GeoPressureR"),
    shiny_opts = list(
      tag = tag,
      stapath = stapath,
      light_trace = light_trace,
      twl = twl,
      stop_on_session_end = TRUE
    ),
    launch_browser = launch_browser
  )
}

#' @noRd
file2obj <- function(file, obj = "tag") {
  if (!file.exists(file)) {
    cli::cli_abort(
      "The file {.field {file}} does not exist."
    )
  }
  objects <- load(file)
  if (!(obj %in% objects)) {
    cli::cli_abort(
      "The object {.var {obj}} is not found in the file {.field {file}}."
    )
  }
  get(obj)
}


#' @noRd
prepare_twilight <- function(tag, ref, compute_known = NULL) {
  GeoPressureR::tag_assert(tag, "twilight")
  twl <- tag$twilight

  # Add inclusion mask consistent with GeoPressureR::geolight_map()
  twl_include <- GeoPressureR:::twilight_include(tag$twilight)
  if ("include" %in% names(twl_include)) {
    twl$include <- twl_include$include
  }

  if (is.null(compute_known)) {
    if (
      "geolight_map" %in% names(tag$param) && "compute_known" %in% names(tag$param$geolight_map)
    ) {
      compute_known <- tag$param$geolight_map[["compute_known"]]
    } else {
      compute_known <- FALSE
    }
  }

  if (!compute_known && "stap" %in% names(tag) && "known_lat" %in% names(tag$stap)) {
    if (!("stap_id" %in% names(twl))) {
      twl$stap_id <- GeoPressureR:::find_stap(tag$stap, twl$twilight)
    }
    known_stap_id <- tag$stap$stap_id[
      !is.na(tag$stap$known_lat) & !is.na(tag$stap$known_lon)
    ]
    twl$include <- twl$include & !(twl$stap_id %in% known_stap_id)
  }

  # twl$date <- as.Date(twl$twilight)
  twl$plottime <- time2plottime(twl$twilight, ref = ref)

  if (!("label" %in% names(twl))) {
    twl$label <- ""
  }

  # twl$id <- seq(1, nrow(twl))
  twl
}

#' @noRd
time2plottime <- function(x, ref = x[1]) {
  floathour <- datetime2floathour(x)
  time_hour <- floathour + 24 * (floathour < datetime2floathour(ref))
  as.POSIXct(Sys.Date()) + time_hour * 3600
}

#' @noRd
datetime2floathour <- function(x) {
  if (!is.character(x)) {
    x <- format(x, "%H:%M")
  }
  as.numeric(substr(x, 1, 2)) + as.numeric(substr(x, 4, 5)) / 60
}

#' @noRd
light_matrix <- function(tag) {
  GeoPressureR::tag_assert(tag, "twilight")

  ## Same as geolight_map()
  light <- tag$light

  # Transform light value for better display
  light$value <- GeoPressureR:::twilight_create_transform(light$value)

  # Use by order of priority: (1) tag$param$twilight_create$twl_offset or (2) guess from light data
  if ("twl_offset" %in% names(tag$param$twilight_create)) {
    twl_offset <- tag$param$twilight_create[["twl_offset"]]
  } else {
    twl_offset <- GeoPressureR:::twilight_create_guess_offset(light)
  }

  # Compute the matrix representation of light
  mat <- GeoPressureR::ts2mat(light, twl_offset = twl_offset)

  mat$plottime <- time2plottime(mat$time)

  # Export for plotly object
  mat
}
