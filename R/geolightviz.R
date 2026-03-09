#' Start the GeoLightViz shiny app
#'
#' @param x a GeoPressureR `tag` object, a `.Rdata` file or the
#' unique identifier `id` with a `.Rdata` file located in `"./data/interim/{id}.RData"`.
#' @param stapath optional stationary path data.frame (defaults to `tag$stap` when available).
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @param run_bg If true (by default), the app runs in a background R process using `callr::r_bg()`,
#' allowing you to continue using the R console. If false, the app blocks the console until closed.
#' @param quiet logical, currently unused.
#' @param ... currently unused.
#'
#' @return When \code{run_bg = TRUE}, an invisible \code{callr} \code{r_process} running the app.
#' When \code{run_bg = FALSE}, the return value of \code{shiny::runApp()}.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read()
#' })
#' geolightviz(tag, run_bg = FALSE, launch_browser = FALSE)
#'
#' @export
geolightviz <- function(
  x,
  stapath = NULL,
  launch_browser = TRUE,
  run_bg = TRUE,
  quiet = FALSE,
  ...
) {
  if (inherits(x, "tag")) {
    tag <- x
  } else if (is.character(x) && length(x) == 1) {
    load_interim(
      x,
      var = "tag",
      envir = environment()
    )
  }

  tag_assert(tag, "twilight")

  # Build light and twilight traces for the app.
  light_trace <- light_matrix(tag)

  # Build map grid for known-position bounds checks.
  g <- NULL
  if (GeoPressureR::tag_assert(tag, "setmap", "logical")) {
    g <- GeoPressureR::map_expand(
      tag$param$tag_set_map$extent,
      tag$param$tag_set_map$scale
    )
  }

  twl <- prepare_twilight(
    tag,
    ref = light_trace$time[1]
  )

  # Resolve save folders based on launch context.
  label_dir <- file.path(getwd(), "data", "twilight-label")
  stap_dir <- file.path(getwd(), "data", "stap-label")

  # Run the app in a background process or in the current session.
  if (run_bg) {
    return(shiny_run_app_bg(
      system.file("geolightviz", package = "GeoPressureR"),
      shiny_opts = list(
        tag = tag,
        stapath = stapath,
        light_trace = light_trace,
        twl = twl,
        stop_on_session_end = FALSE,
        label_dir = label_dir,
        stap_dir = stap_dir
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
      stop_on_session_end = TRUE,
      label_dir = label_dir,
      stap_dir = stap_dir
    ),
    launch_browser = launch_browser
  )
}

#' @noRd
prepare_twilight <- function(tag, ref, compute_known = NULL) {
  tag_assert(tag, "twilight")
  twl <- tag$twilight

  # Add inclusion mask consistent with GeoPressureR::geolight_map()
  twl_include <- tryCatch(
    twilight_include(tag$twilight),
    error = function(e) NULL
  )
  if (!is.null(twl_include) && "include" %in% names(twl_include)) {
    twl$include <- twl_include$include
  } else {
    twl$include <- rep(FALSE, nrow(twl))
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
      twl$stap_id <- find_stap(tag$stap, twl$twilight)
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
#' @keywords internal
#' @export
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
  light$value <- twilight_create_transform(light$value)

  # Use by order of priority: (1) tag$param$twilight_create$twl_offset or (2) guess from light data
  if ("twl_offset" %in% names(tag$param$twilight_create)) {
    twl_offset <- tag$param$twilight_create[["twl_offset"]]
  } else {
    twl_offset <- twilight_create_guess_offset(light)
  }

  # Compute the matrix representation of light
  mat <- GeoPressureR::ts2mat(light, twl_offset = twl_offset)

  mat$plottime <- time2plottime(mat$time)

  # Export for plotly object
  mat
}
