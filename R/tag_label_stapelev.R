#' Label the elevation levels of a stationary period from a position
#'
#' @description
#' Run TRAINSET's automatic stapelev on one stationary period from a script, and return the `tag`
#' with its pressure labels replaced.
#'
#' The residual is the tag pressure minus the ERA5 surface pressure (not normalised) at the position
#' `lat`, `lon`. Each elevation level is a band of `± range` hPa around an offset of this residual.
#' Offsets are initialised on the densest bands in turn, then adjusted to the median residual of
#' their points (TRAINSET's Initialize and Adjust). Each point is assigned to the nearest level
#' within its band, and runs of a level shorter than `continuity` hours become `"discard"`.
#'
#' As TRAINSET's Apply, it overwrites every non-flight label of the stationary period, including
#' manual `"discard"` and `"elev_n"` labels, and leaves flight labels and stationary periods
#' unchanged. The proposal is only as good as the position: at a wrong position, the levels and
#' discards confirm that position.
#'
#' @param tag a GeoPressureR `tag` object with labels and stationary periods (see [`tag_label()`]),
#' before [`tag_set_map()`].
#' @param stap_id stationary period to label.
#' @param lat,lon position of the stationary period (degrees).
#' @param timeseries output of [`geopressure_timeseries()`] for this stationary period, with
#' `pressure`. Default queries it at `lat`, `lon`.
#' @param n_level number of elevation levels.
#' @param range half-width of the band of each level (hPa).
#' @param continuity minimum duration of a run of points of the same level (hours). `0` keeps every
#' run.
#' @param gap largest gap bridged within a run (minutes). Default is the sampling interval.
#' @param discard_solar `"none"`, `"day"` or `"night"`: points of this part of the day are not
#' assigned to any level.
#' @param solar_angle solar elevation separating day from night (degrees).
#' @param quiet logical to hide messages.
#' @param ... arguments passed to [`geopressure_timeseries()`] (e.g., `source`).
#'
#' @return The `tag` with the pressure labels of stationary period `stap_id` replaced by `""`,
#' `"elev_n"` or `"discard"`.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE)
#' })
#'
#' \dontrun{
#' tag <- tag_label_stapelev(tag, stap_id = 1, lat = 48.9, lon = 17.05)
#' table(tag$pressure$label[tag$pressure$stap_id == 1])
#' }
#'
#' @family tag_label
#' @seealso [`trainset()`], [`geopressure_timeseries()`]
#' @keywords internal
#' @export
tag_label_stapelev <- function(
  tag,
  stap_id,
  lat,
  lon,
  timeseries = NULL,
  n_level = 1,
  range = 2,
  continuity = 6,
  gap = NULL,
  discard_solar = "none",
  solar_angle = -6,
  quiet = FALSE,
  ...
) {
  tag_assert(tag, "stap")
  if (tag_assert(tag, "setmap", "logical")) {
    cli::cli_abort(c(
      "x" = "{.fun tag_set_map} has already been run on this {.var tag}.",
      ">" = "Labels change the pressure maps: label the {.var tag} before {.fun tag_set_map}."
    ))
  }
  assertthat::assert_that(assertthat::is.number(stap_id))
  assertthat::assert_that(stap_id %in% tag$stap$stap_id)
  assertthat::assert_that(assertthat::is.number(lat), assertthat::is.number(lon))
  assertthat::assert_that(assertthat::is.count(n_level))
  assertthat::assert_that(assertthat::is.number(range), range > 0)
  assertthat::assert_that(assertthat::is.number(continuity), continuity >= 0)
  assertthat::assert_that(is.null(gap) || assertthat::is.number(gap))
  assertthat::assert_that(discard_solar %in% c("none", "day", "night"))
  assertthat::assert_that(assertthat::is.number(solar_angle))

  # Pressure of the stap and ERA5 surface pressure at the position (the returned ERA5 cell)
  id <- which(tag$pressure$stap_id == stap_id)
  pressure <- tag$pressure[id, ]
  if (is.null(timeseries)) {
    timeseries <- geopressure_timeseries(lat, lon, pressure = pressure, quiet = TRUE, ...)
  }
  assertthat::assert_that(
    assertthat::has_name(timeseries, c("date", "surface_pressure", "lat", "lon"))
  )
  proposal <- stapelev_proposal(
    pressure$date,
    pressure$value,
    timeseries$surface_pressure[match(as.numeric(pressure$date), as.numeric(timeseries$date))],
    pressure$label
  )
  solar_elevation <- suntools::solarpos(
    matrix(c(timeseries$lon[1], timeseries$lat[1]), ncol = 2),
    pressure$date
  )[, 2]

  # Settings of TRAINSET's modal, each level over the whole stap
  date <- as.numeric(pressure$date)
  settings <- list(
    offsets = rep(0, n_level),
    windows = matrix(range(date), n_level, 2, byrow = TRUE),
    range = range,
    continuity = continuity,
    gap = if (is.null(gap)) stats::median(diff(date)) / 60 else gap,
    discard_solar = discard_solar,
    solar_angle = solar_angle
  )
  eligible <- stapelev_eligible(proposal, solar_elevation, settings)

  # Initialize then adjust the offsets of all levels
  offsets <- stapelev_initialize_offsets(proposal, settings$windows, range, eligible)
  if (is.null(offsets)) {
    cli::cli_abort(c(
      "x" = "Not enough unexplained pressure in stap {.val {stap_id}} for {.val {n_level}} levels.",
      ">" = "Use fewer levels or a smaller {.arg range}."
    ))
  }
  settings$offsets <- stats::setNames(
    offsets,
    c("", if (n_level > 1) glue::glue("elev_{seq_len(n_level - 1)}"))
  )
  settings$offsets <- stapelev_fit_offsets(
    proposal,
    settings,
    eligible,
    names(settings$offsets)
  )$offsets

  # Assign the points and write the labels back (flight labels are unchanged)
  proposal <- stapelev_assign_offsets(
    proposal,
    settings$offsets,
    settings$windows,
    range,
    continuity,
    eligible,
    settings$gap
  )
  tag$pressure$label[id] <- proposal$label

  if (!quiet) {
    level <- ifelse(names(settings$offsets) == "", "elev_0", names(settings$offsets))
    n <- vapply(names(settings$offsets), \(l) sum(proposal$label == l), integer(1))
    txt <- glue::glue_collapse(glue::glue("{level}: {settings$offsets} hPa ({n} points)"), ", ")
    n_discard <- sum(proposal$label == "discard")
    cli::cli_bullets(c("i" = "Stap {stap_id} offsets {txt}; {n_discard} points discarded."))
  }
  tag
}
