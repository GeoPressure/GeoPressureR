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
#' The correction added to each recorded timestamp is `time_drift` multiplied by the elapsed
#' number of years since `reference`. A year is exactly 365.25 days. Positive values add time
#' and negative values subtract time, as with `tag_create(time_shift)`. Apply any constant offset
#' with `tag_create(time_shift)` when reading the original files; it is not repeated here.
#' The same reference and rate apply to every selected sensor, regardless of its start date or
#' recording duration. By default, the reference is the earliest measurement across all available
#' sensors, including unselected sensors, and its drift correction is zero. Supply `reference` if
#' the clock was synchronised at another time. The correction also extends linearly to measurements
#' before an explicit reference.
#' `reference` is expressed in the timestamp system of the input tag, including any shift already
#' applied when reading it.
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
#' @param time_drift Correction in hours per year (365.25 days). For example, `-12 / 60`
#'   subtracts 12 minutes per year. Convert a rate in hours per 30-day month by multiplying it
#'   by `365.25 / 30`.
#' @param reference Recorded timestamp at which the drift correction is zero, as POSIXct or
#'   character in UTC. Default (`NULL`) uses the earliest measurement across all available sensors.
#' @param sensors `"all"` (default) or a character vector of sensors to correct: `"pressure"`,
#'   `"light"`, `"acceleration"`, `"temperature_external"`, `"temperature_internal"` or `"magnetic"`.
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
#' corrected <- tag_correct_time(tag, time_drift = -12 / 60)
#'
#' # Alternatively, use the recorded clock synchronisation time as the reference.
#' corrected <- tag_correct_time(tag, time_drift = -12 / 60, reference = "2024-12-31")
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
tag_correct_time <- function(tag, time_drift, reference = NULL, sensors = "all") {
  tag_assert(tag)
  if (
    any(c("label", "stap", "twilight", "setmap", "map_pressure", "map_light") %in% tag_status(tag))
  ) {
    cli::cli_abort("Read an unlabelled tag with {.fun tag_create} before correcting its clock.")
  }
  available <- intersect(
    c(
      "pressure",
      "light",
      "acceleration",
      "temperature_external",
      "temperature_internal",
      "magnetic"
    ),
    names(tag)
  )
  sensors <- if (identical(sensors, "all")) available else intersect(sensors, available)
  reference <- if (is.null(reference)) {
    do.call(min, lapply(tag[available], function(sensor) min(sensor$date)))
  } else {
    as.POSIXct(reference, tz = "UTC")
  }
  assertthat::assert_that(
    is.numeric(time_drift),
    length(time_drift) == 1,
    is.finite(time_drift),
    length(reference) == 1,
    !is.na(reference)
  )
  assertthat::assert_that(time_drift > -365.25 * 24)

  for (sensor in sensors) {
    tag[[sensor]]$date[] <- tag[[sensor]]$date +
      as.numeric(difftime(tag[[sensor]]$date, reference, units = "secs")) *
        time_drift /
        (365.25 * 24)
  }
  tag$param$tag_correct_time <- list(
    time_drift = time_drift,
    reference = reference,
    sensors = sensors
  )
  tag
}
