#' Check the labels of a `tag`
#'
#' @description
#' Return, as data.frames, the checks that TRAINSET shows in its Check panel: short stationary
#' periods, short flights and large hourly pressure changes within a stationary period. Use it to
#' review labels from a script, with the same rules and defaults as TRAINSET.
#'
#' The hourly pressure changes are computed on the series sent to the pressure map
#' ([`geopressure_map_preprocess()`]), per stationary period and elevation level (stapelev). A large
#' change usually means a flight, a take-off or a landing left inside a stationary period, or two
#' elevation levels that need different `elev_n` labels.
#'
#' @param tag a GeoPressureR `tag` object with labels and stationary periods (see
#' [`tag_label()`]).
#' @param warning_stap_duration Threshold of stationary period duration (hours). Stationary
#' periods lasting this long or less are reported.
#' @param warning_flight_duration Threshold of flight duration (hours). Flights lasting this long
#' or less are reported.
#' @param warning_pressure_diff Threshold of hourly pressure change (hPa). Changes of this size or
#' more are reported.
#'
#' @return A list with
#' - `stap`: short stationary periods, with `stap_id`, `start`, `end` and `duration` (hours).
#' - `flight`: short flights between consecutive stationary periods, with `stap_s`, `stap_t`,
#'   `start`, `end` and `duration` (hours).
#' - `pressure`: large hourly pressure changes, largest first, with `date` (midpoint of the hour),
#'   `stap_id`, `stapelev`, `pressure_diff` (absolute change, hPa) and `pressure` (hPa). `NULL`
#'   when pressure has no label or cannot be pre-processed.
#' - `error`: the reason why pressure could not be pre-processed (e.g., a stationary period with
#'   every point discarded), or `NULL`.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE)
#' })
#'
#' tag_label_check(tag)
#'
#' @family tag_label
#' @seealso [`trainset()`], [`plot_tag_pressure()`]
#' @export
tag_label_check <- function(
  tag,
  warning_stap_duration = 6,
  warning_flight_duration = 2,
  warning_pressure_diff = 3
) {
  tag_assert(tag, "stap")
  assertthat::assert_that(assertthat::is.number(warning_stap_duration))
  assertthat::assert_that(assertthat::is.number(warning_flight_duration))
  assertthat::assert_that(assertthat::is.number(warning_pressure_diff))

  # Short stationary periods
  stap <- tag$stap[, c("stap_id", "start", "end")]
  stap$duration <- stap2duration(stap, units = "hours")
  stap <- stap[stap$duration <= warning_stap_duration, ]

  # Short flights between all consecutive stationary periods, whatever `stap$include`
  flight <- if (nrow(tag$stap) > 1) {
    f <- stap2flight(tag$stap, include_stap_id = tag$stap$stap_id, units = "hours")
    f[f$duration <= warning_flight_duration, c("stap_s", "stap_t", "start", "end", "duration")]
  }

  # Hourly pressure changes on the pre-processed series. Pre-processing aborts on labels that the
  # check should report instead (e.g., a stap with every point discarded), so keep its message.
  pressure <- NULL
  error <- NULL
  if (assertthat::has_name(tag, "pressure") && assertthat::has_name(tag$pressure, "label")) {
    pres <- tryCatch(geopressure_map_preprocess(tag, compute_known = TRUE), error = identity)
    if (inherits(pres, "error")) {
      error <- conditionMessage(pres)
    } else {
      pressure <- pressure_diff_warning_data(pres, warning_pressure_diff)
    }
  }

  list(stap = stap, flight = flight, pressure = pressure, error = error)
}
