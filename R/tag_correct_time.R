#' Correct sensor clock drift before starting an analysis
#'
#' @description
#' Apply a linear clock correction to selected sensors of an unlabelled `tag`. This is a one-off
#' preparation step for tags with known clock drift, outside the standard analysis pipeline.
#' Measurement values and sampling order are preserved; only sensor timestamps are corrected.
#'
#' @details
#' Read the original files with [tag_create()] without cropping or labelling, correct the clock,
#' and save the corrected sensor tables as CSV files in a separate raw-tag directory. Rename
#' `date` to `datetime` and format timestamps as `"%Y-%m-%dT%H:%M:%OS6"` in UTC when saving.
#' Start the normal workflow from those CSVs with `tag_create(manufacturer = "tabular")`, then
#' crop, label and analyse the data. Keep the original files and the correction parameters.
#'
#' The correction added to each recorded timestamp is `time_shift` plus `time_drift` multiplied
#' by the elapsed number of months or years since `reference`. Both corrections are in hours;
#' positive values add time and negative values subtract time, as with `tag_create(time_shift)`.
#' A month is exactly 30 days and a year is exactly 365.25 days. The same reference and rate apply
#' to every selected sensor, regardless of its start date or recording duration. The correction
#' also extends linearly to measurements before the reference. `reference` is expressed in the
#' timestamp system of the input tag, including any shift already applied when reading it.
#'
#' Corrected sampling intervals may contain fractional seconds. Light analysis and actograms
#' match observations to a grid with an integer number of samples per day; pressure preprocessing
#' selects smoothed observations on an explicit hourly grid. This function itself does not
#' resample measurements or write files.
#'
#' Do not apply the same correction again when reading the corrected CSVs. Existing labels,
#' twilights and maps must be regenerated from corrected data; this function requires a tag
#' without those derived results. Apply different sensor corrections in separate calls using
#' `sensors` and the appropriate reference and rate for each clock.
#'
#' @param tag An unlabelled GeoPressureR `tag` object.
#' @param time_drift Correction in hours per `time_unit`. For example, `-12 / 60` subtracts
#'   12 minutes per year when `time_unit = "year"`.
#' @param reference Recorded timestamp at which the drift correction is zero, as POSIXct or
#'   character in UTC. At this timestamp, only `time_shift` is added.
#' @param time_unit Unit of the drift rate: `"year"` (365.25 days) or `"month"` (30 days).
#' @param time_shift Constant correction added to selected sensor dates, in hours. Default is zero.
#' @param sensors Character vector of sensors to correct. By default, all available sensors.
#'
#' @return A `tag` with corrected sensor dates and the correction parameters in
#'   `tag$param$tag_correct_time`. Other sensor columns and unselected sensors are unchanged.
#'
#' @examples
#' pressure <- data.frame(
#'   date = as.POSIXct("2025-01-01", tz = "UTC") + (0:48) * 1800,
#'   value = rep(1000, 49)
#' )
#' tag <- tag_create("clock-example", pressure_file = pressure, quiet = TRUE)
#' corrected <- tag_correct_time(tag, time_drift = -12 / 60, reference = "2025-01-01")
#'
#' # Save in a separate directory, keeping fractional seconds.
#' directory <- tempfile("corrected-raw-tag-")
#' dir.create(directory)
#' pressure <- corrected$pressure
#' names(pressure)[names(pressure) == "date"] <- "datetime"
#' pressure$datetime <- format(pressure$datetime, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")
#' utils::write.csv(pressure, file.path(directory, "pressure.csv"), row.names = FALSE)
#'
#' # Start the normal pipeline from corrected files, without repeating the correction.
#' tag <- tag_create("clock-example", directory = directory, quiet = TRUE)
#' unlink(directory, recursive = TRUE)
#'
#' @family tag
#' @export
tag_correct_time <- function(
  tag,
  time_drift,
  reference,
  time_unit = c("year", "month"),
  time_shift = 0,
  sensors = c(
    "pressure",
    "light",
    "acceleration",
    "temperature_external",
    "temperature_internal",
    "magnetic"
  )
) {
  tag_assert(tag)
  if (
    any(c("label", "stap", "twilight", "setmap", "map_pressure", "map_light") %in% tag_status(tag))
  ) {
    cli::cli_abort("Read an unlabelled tag with {.fun tag_create} before correcting its clock.")
  }
  time_unit <- match.arg(time_unit)
  sensors <- match.arg(
    sensors,
    choices = c(
      "pressure",
      "light",
      "acceleration",
      "temperature_external",
      "temperature_internal",
      "magnetic"
    ),
    several.ok = TRUE
  )
  reference <- as.POSIXct(reference, tz = "UTC")
  assertthat::assert_that(
    is.numeric(time_drift),
    length(time_drift) == 1,
    is.finite(time_drift),
    is.numeric(time_shift),
    length(time_shift) == 1,
    is.finite(time_shift),
    length(reference) == 1,
    !is.na(reference)
  )
  period <- if (time_unit == "year") 365.25 else 30
  assertthat::assert_that(time_drift > -period * 24)

  for (sensor in intersect(sensors, names(tag))) {
    tag[[sensor]]$date[] <- tag[[sensor]]$date +
      time_shift * 3600 +
      as.numeric(difftime(tag[[sensor]]$date, reference, units = "secs")) *
        time_drift /
        (period * 24)
  }
  tag$param$tag_correct_time <- list(
    time_drift = time_drift,
    reference = reference,
    time_unit = time_unit,
    time_shift = time_shift,
    sensors = intersect(sensors, names(tag))
  )
  tag
}
