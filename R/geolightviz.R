#' Start the GeoLightViz Shiny app
#'
#' GeoLightViz is an interactive tool for reviewing twilight observations and
#' preparing light-based geolocation analyses. It displays the light record and
#' detected twilight events so that observations affected by shading or other
#' light obstructions can be marked as discarded. It can also be used to define
#' or edit stationary periods from twilight patterns, which is useful when
#' pressure or acceleration data are unavailable. When a map has been defined
#' with [tag_set_map()] and stationary periods are available, the app links the
#' light plot to a map of twilight likelihoods. You can explore candidate
#' locations and tune twilight calibration and likelihood aggregation settings
#' with visual feedback. Twilight labels and stationary periods can be saved or
#' exported from the app.
#'
#' See the [GeoLightViz chapter of the GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/geolightviz.html) for an overview
#' of the app's controls and workflow.
#'
#' @template map-tiles
#' @param x a GeoPressureR `tag` object, a `.RData` file or the
#' unique identifier `id` with a `.RData` file located in `"./data/interim/{id}.RData"`.
#' @param stapath Optional stationary path data.frame with `start` and `end` columns.
#'   Defaults to `tag$stap` when available. Use `read_stap(tag$param$id)` to load a path from
#'   `./data/stap-label/{id}.csv`.
#' @param launch_browser If `TRUE` (default), the app opens in your web browser; otherwise, only the
#' app URL is printed.
#' @param run_bg If true (by default), the app runs in a background R process using `callr::r_bg()`,
#' allowing you to continue using the R console. If false, the app blocks the console until closed.
#' @param quiet `r lifecycle::badge("deprecated")` Had no effect.
#' @param ... Unused; extra arguments are rejected.
#'
#' @return When \code{run_bg = TRUE}, an invisible \code{callr} \code{r_process} running the app.
#' When \code{run_bg = FALSE}, the return value of \code{shiny::runApp()}.
#'
#' @seealso [GeoPressureManual | GeoLightViz
#' ](https://geopressure.org/GeoPressureManual/geolightviz.html)
#'
#' @examplesIf FALSE
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read()
#' })
#' geolightviz(tag, run_bg = FALSE, launch_browser = FALSE)
#'
#' # Load stationary periods from ./data/stap-label/{id}.csv in a GeoPressureR project
#' geolightviz(tag, stapath = read_stap(tag$param$id))
#'
#' @export
geolightviz <- function(
  x,
  stapath = NULL,
  launch_browser = TRUE,
  run_bg = TRUE,
  quiet = lifecycle::deprecated(),
  ...
) {
  if (lifecycle::is_present(quiet)) {
    lifecycle::deprecate_warn("3.8.0", "geolightviz(quiet)", details = "It had no effect.")
  }
  if (...length() > 0) {
    cli::cli_abort("{.arg ...} must be empty; extra arguments are not supported.")
  }

  if (inherits(x, "tag")) {
    tag <- x
  } else if (is.character(x) && length(x) == 1) {
    load_interim(
      x,
      var = "tag",
      envir = environment()
    )
  } else {
    cli::cli_abort("{.arg x} must be a tag object, an ID, or an .Rdata file.")
  }

  tag_assert(tag, "twilight")
  tag_assert(tag, "light")

  for (name in c("stapath", "tag$stap")) {
    periods <- if (name == "stapath") stapath else tag$stap
    if (is.null(periods)) {
      next
    }
    if (!is.data.frame(periods)) {
      cli::cli_abort(
        "{.arg {name}} must be a data frame of stationary periods or {.code NULL}."
      )
    }
    if (!all(c("start", "end") %in% names(periods))) {
      cli::cli_abort(
        "{.arg {name}} must contain {.field start} and {.field end} columns."
      )
    }
    if (!inherits(periods$start, "POSIXct") || !inherits(periods$end, "POSIXct")) {
      cli::cli_abort(
        "{.field start} and {.field end} in {.arg {name}} must be POSIXct datetimes."
      )
    }
    if (
      anyNA(periods$start) ||
        anyNA(periods$end) ||
        any(periods$start > periods$end) ||
        is.unsorted(periods$start) ||
        any(utils::head(periods$end, -1) > utils::tail(periods$start, -1))
    ) {
      cli::cli_abort(
        "Stationary periods in {.arg {name}} must have ordered, non-overlapping start and end times without missing values."
      )
    }
  }

  if (!is.null(stapath)) {
    stapath <- geolightviz_normalize_stap_id(stapath)
  } else if ("stap" %in% names(tag)) {
    tag$stap <- geolightviz_normalize_stap_id(tag$stap)
  }

  # Build light and twilight traces for the app.
  light_trace <- light_matrix(tag)

  twl <- prepare_twilight(
    tag,
    ref = light_trace$time[1]
  )
  # Day of the light matrix column of each twilight (columns are centred on twl_offset)
  twl$day <- light_trace$day[pmax(
    1,
    findInterval(as.numeric(twl$twilight), light_trace$date[1, ])
  )]

  # Resolve save folders based on launch context.
  label_dir <- file.path(getwd(), "data", "twilight-label")
  stap_dir <- file.path(getwd(), "data", "stap-label")

  # Run the app in a background process or in the current session.
  # nocov start
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
  # nocov end
}

#' @noRd
geolightviz_normalize_stap_id <- function(stapath) {
  if (!("stap_id" %in% names(stapath))) {
    stapath$stap_id <- seq_len(nrow(stapath))
    return(stapath)
  }

  n <- nrow(stapath)
  if (isTRUE(all.equal(as.numeric(stapath$stap_id), seq_len(n)))) {
    return(stapath)
  }

  cli::cli_warn(
    "GeoLightViz renumbered {.field stap_id} from 1 to {n} for navigation and editing compatibility."
  )
  stapath$stap_id <- seq_len(n)

  stapath
}

#' @noRd
prepare_twilight <- function(tag, ref) {
  twl <- tag$twilight

  # twl$date <- as.Date(twl$twilight)
  twl$plottime <- time2plottime(twl$twilight, ref = ref)

  if (!("label" %in% names(twl))) {
    twl$label <- ""
  }

  # twl$id <- seq(1, nrow(twl))
  twl
}

#' Convert datetimes to a plotting time-of-day scale
#'
#' @param x a POSIXct vector of datetimes.
#' @param ref reference datetime used to keep ordering around midnight.
#' @return POSIXct vector on an arbitrary day encoding plotted time-of-day.
#' @keywords internal
#' @export
time2plottime <- function(x, ref = x[1]) {
  floathour <- datetime2floathour(x)
  time_hour <- floathour + 24 * (floathour < datetime2floathour(ref))
  as.POSIXct("2000-01-01", tz = "UTC") + time_hour * 3600
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

  # Use the same offset and time tolerance as plot_tag_twilight()
  if ("twl_time_tolerance" %in% names(tag$param$twilight_create)) {
    twl_time_tolerance <- tag$param$twilight_create$twl_time_tolerance
  } else {
    twl_time_tolerance <- formals(twilight_create)$twl_time_tolerance
  }

  # Compute the matrix representation of light
  mat <- GeoPressureR::ts2mat(
    light,
    sensor = "light",
    twl_offset = resolve_twl_offset(tag, light, NULL),
    twl_time_tolerance = twl_time_tolerance
  )

  mat$plottime <- time2plottime(mat$time)

  # Export for plotly object
  mat
}
