#' Create a `tag` object
#'
#' @description
#' Create a GeoPressureR `tag` object from the data collected by a tracking device. The function
#' can read data formatted according to three manufacturers SOI, Migratetech or Lund CAnMove, as
#' well as BAS and PresTag formats, and also accepts manual tabular input. Pressure data is
#' required for the GeoPressureR workflow but can be allowed to be missing with
#' `assert_pressure = FALSE`.
#'
#' @details
#' The current implementation can read files from the following sources:
#' - [Swiss Ornithological Institute (`soi`)](https://www.vogelwarte.ch/en/research/bird-migration/geolocators/)
#'    - `pressure_file = "*.pressure"`
#'    - `light_file = "*.glf"` (optional)
#'    - `acceleration_file = "*.acceleration"` (optional)
#'    - `temperature_internal_file = "*.temperature"` (optional)
#'    - `temperature_external_file = "*.airtemperature"` (optional)
#'    - `magnetic_file = "*.magnetic"` (optional)
#' - [Migrate Technology (`migratetech`)](https://www.migratetech.co.uk/):
#'    - `pressure_file = "*.deg"`
#'    - `light_file = "*.lux"` (optional)
#'    - `acceleration_file = "*.deg"` (optional)
#' - British Antarctic Survey (`bas`), acquired by Biotrack Ltd in 2011, [renamed Lotek in 2019
#' ](https://www.lotek.com/about-us/history/). Only works for light data (`assert_pressure = FALSE`)
#'    - `light_file = "*.lig"`
#' - Lund CAnMove (`lund`)
#'    - `pressure_file = "*_press.xlsx"`
#'    - `light_file = "*_acc.xlsx"` (optional)
#'    - `acceleration_file = "*_acc.xlsx"` (optional)
#' - [BitTag/PresTag (`prestag`)](https://geoffreymbrown.github.io/ultralight-tags/)
#'    - `pressure_file = "*.txt"`
#'
#' You can also enter tabular data manually (`manufacturer = "tabular"`) by providing, for each
#' sensor argument, either an in-memory table (`data.frame` or tibble) or a CSV path:
#'   - `pressure_file`: columns `date` and `value` in hPa.
#'   - `light_file`: (optional) columns `date` and `value`.
#'   - `acceleration_file`: (optional) columns `date` and `value`.
#'   - `temperature_external_file`: (optional) columns `date` and `value`.
#'   - `temperature_internal_file`: (optional) columns `date` and `value`.
#'   - `magnetic_file`: (optional) columns `date`, `magnetic_x`, `magnetic_y`,
#'    `magnetic_z`, `acceleration_x`, `acceleration_y` and `acceleration_z`.
#'
#' You can still create a `tag` without pressure data using `assert_pressure = FALSE`. This `tag`
#' won't be able to run the traditional GeoPressureR workflow, but you can still do some analysis.
#'
#' By default `manufacturer = NULL`, the manufacturer is determined automatically from the content
#' of the `directory`. You can also specify manually the file with a full pathname or the file
#' extension using a regex expression (e.g., `"*.pressure"` matches any file ending with
#' `pressure`).
#'
#' Please create [an issue on Github](https://github.com/GeoPressure/GeoPressureR/issues/new) if you
#' have data in a format that is not yet supported.
#'
#' This function can be used to crop the data at specific date, for instance to remove pre-equipment
#' or post-retrieval data.
#'
#' Sensor clocks are corrected after reading the original data and before cropping. The correction
#' is `time_shift` plus `time_drift` multiplied by the elapsed years since `time_reference`.
#' Drift is computed from the original recorded dates before adding the constant shift. A year
#' is exactly 365.25 days. The default reference is the earliest original measurement across all
#' available sensors, including sensors with no drift correction, before cropping or shifting.
#' Each sensor uses this shared reference regardless of its recording start or duration. Provide
#' `time_reference` if the clock was synchronised at another recorded time; correction extends
#' linearly to measurements before that reference.
#'
#' Corrections apply to every sensor table read from the original data, including additional
#' sensor channels supported by future readers. Use one value for all sensors, or named lists for
#' sensor-specific shifts and drift rates. Sensors omitted from a list receive zero for that
#' correction; entries for sensors absent from the data are ignored.
#' Measurement values and sampling order are preserved. Corrected sampling intervals may contain
#' fractional seconds; light analysis and actograms use full-day grids, and pressure preprocessing
#' selects smoothed observations on an explicit hourly grid.
#'
#' Keep the original files and set the correction in `tag_create()` as part of the reproducible
#' pipeline. No corrected raw files need to be saved. The corrections and the resolved reference
#' (when drift is nonzero) are stored in `tag$param$tag_create`. Do not apply the corrections again
#' if reading data already corrected elsewhere. Changing `time_shift`, `time_drift` or
#' `time_reference` requires regenerating derived results and may require regenerating TRAINSET
#' label files because label matching uses exact timestamps.
#'
#' @param id unique identifier of a tag.
#' @param manufacturer One of `NULL`, `"soi"`, `"migratetech"`, `"bas"`, `"lund"`, `"prestag"` or
#' `"tabular"`.
#' @param directory path of the directory where the tag files can be read.
#' @param pressure_file name of the file with pressure data. Full pathname or finishing with
#' extensions (e.g., `"*.pressure"`, `"*.deg"` or `"*_press.xlsx"`). For
#' `manufacturer = "tabular"`, provide an in-memory table with columns `date` and `value`, or a
#' CSV path with columns `datetime` and `value`.
#' @param light_file name of the file with light data. Full pathname or finishing with extensions
#' (e.g., `"*.glf"`, `"*.lux"` or `"*_acc.xlsx"`). For `manufacturer = "tabular"`, provide an
#' in-memory table with columns `date` and `value`, or a CSV path with columns `datetime` and
#' `value`.
#' @param acceleration_file name of the file with acceleration data. Full pathname or finishing with
#' extensions (e.g., `"*.acceleration"`, `"*.deg"` or `"*_acc.xlsx"`). For
#' `manufacturer = "tabular"`, provide an in-memory table with columns `date` and `value`, or a
#' CSV path with columns `datetime` and `value`.
#' @param temperature_external_file name of the file with temperature data. Full pathname or
#' finishing with extensions (e.g., `"*.temperature"`, `"*.airtemperature"` or `"*.deg"`). External
#' or air temperature is generally for temperature sensor on directed outward from the bird. For
#' `manufacturer = "tabular"`, provide an in-memory table with columns `date` and `value`, or a
#' CSV path with columns `datetime` and `value`.
#' @param temperature_internal_file name of the file with temperature data . Full pathname or
#' finishing with extensions (e.g., `"*.bodytemperature"`). Internal or body temperature is
#' generally for temperature sensor on directed inward (between bird and tag). For
#' `manufacturer = "tabular"`, provide an in-memory table with columns `date` and `value`, or a
#' CSV path with columns `datetime` and `value`.
#' @param magnetic_file name of the file with magnetic/accelerometer data. Full pathname or
#' finishing with extensions (e.g., `"*.magnetic"`). For `manufacturer = "tabular"`, provide an
#' in-memory table with columns `date`, `magnetic_x`, `magnetic_y`, `magnetic_z`,
#' `acceleration_x`, `acceleration_y` and `acceleration_z`, or a CSV path with `datetime` plus
#' these sensor columns.
#' @param time_shift Raw timestamp correction added to sensor dates, in hours. Use a single numeric
#' value for all sensors or a named list for sensor-specific shifts. Positive values add time.
#' @param time_drift Linear clock correction in hours per year (365.25 days), added to sensor dates.
#' Use one numeric value for all sensors or a named list for sensor-specific rates. Positive values
#' add time and negative values subtract time. For example, `-12 / 60` subtracts 12 minutes per year.
#' Convert hours per 30-day month to hours per year by multiplying by `365.25 / 30`.
#' @param time_reference Original recorded timestamp at which the drift correction is zero,
#' as POSIXct or character in UTC. Default (`NULL`) uses the earliest original measurement across
#' all available sensors, before shifts or cropping. At this time only `time_shift` is added.
#' @param crop_start remove all data before this date (POSIXct or character in UTC).
#' @param crop_end remove all data after this date (POSIXct or character in UTC).
#' @param quiet logical to hide messages about the progress.
#' @param assert_pressure logical to check that the return tag has pressure data.
#'
#' @return a GeoPressureR `tag` object containing
#' - `param` parameter object (see [param_create()])
#' - `pressure` data.frame with columns: `date` and `value`
#' - `light` (optional) same structure as pressure
#' - `temperature_external` (optional) same structure as pressure
#' - `temperature_internal` (optional) same structure as pressure
#' - `acceleration` (optional) data.frame with columns: `date`, `value` and optionally
#'   `mean_acceleration_z`.
#'    - `value` is the activity computed as the sum of the difference in acceleration on the z-axis
#'    (i.e. jiggle). In the SOI sensor, it is summarised from 32 measurements at 10Hz
#'    - `mean_acceleration_z` is the mean acceleration on the z axis. In the SOI sensor, it is an
#'    average over 32 measurements at 10Hz.
#' - `magnetic` (optional) data.frame with columns: `date`, `magnetic_x`, `magnetic_y`, `magnetic_z`
#'    , `acceleration_x`, `acceleration_y` and `acceleration_z`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   # Read all sensor file
#'   tag <- tag_create("18LX")
#'
#'   print(tag)
#'
#'   # Read only pressure and crop date
#'   tag <- tag_create("18LX",
#'     light_file = NULL,
#'     acceleration_file = NULL,
#'     crop_start = "2017-08-01",
#'     crop_end = "2017-08-05"
#'   )
#'
#'   print(tag)
#'
#'   # You can also specify the exact file in case multiple files with the
#'   # same extension exist in your directory (migratetech data)
#'   tag <- tag_create("CB621",
#'     pressure_file = "CB621_BAR.deg",
#'     light_file = "CB621.lux",
#'     acceleration_file = NULL
#'   )
#'
#'   print(tag)
#'
#'   # You can specify the data manually with
#'   pressure <- data.frame(
#'     date = as.POSIXct(c(
#'       "2017-06-20 00:00:00 UTC", "2017-06-20 01:00:00 UTC",
#'       "2017-06-20 02:00:00 UTC", "2017-06-20 03:00:00 UTC"
#'     ), tz = "UTC"),
#'     value = c(1000, 1000, 1000, 1000)
#'   )
#'   tag_create(id = "xxx", pressure_file = pressure)
#'
#'   # Correct all sensors by -12 minutes/year, then add a constant two-hour shift.
#'   tag_create("xxx", pressure_file = pressure, time_drift = -12 / 60, time_shift = 2)
#'
#'   # Correct only pressure, anchored at an explicit recorded reference time.
#'   tag_create("xxx", pressure_file = pressure,
#'     time_drift = list(pressure = -12 / 60), time_reference = "2017-06-20"
#'   )
#' })
#'
#' @family tag
#' @seealso [GeoPressureManual](https://geopressure.org/GeoPressureManual/tag-object.html#create-tag)
#' @export
tag_create <- function(
  id,
  manufacturer = NULL,
  crop_start = NULL,
  crop_end = NULL,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  light_file = NULL,
  acceleration_file = NULL,
  temperature_external_file = NULL,
  temperature_internal_file = NULL,
  magnetic_file = NULL,
  assert_pressure = TRUE,
  quiet = FALSE,
  time_shift = 0,
  time_drift = 0,
  time_reference = NULL
) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))
  for (correction in list(time_shift, time_drift)) {
    if (is.numeric(correction)) {
      assertthat::assert_that(length(correction) == 1, is.finite(correction))
    } else {
      assertthat::assert_that(
        is.list(correction),
        !is.null(names(correction)),
        all(vapply(correction, is.numeric, logical(1))),
        all(lengths(correction) == 1),
        all(is.finite(unlist(correction)))
      )
    }
  }
  assertthat::assert_that(all(unlist(time_drift) > -365.25 * 24))
  if (!is.null(time_reference)) {
    time_reference <- as.POSIXct(time_reference, tz = "UTC")
    assertthat::assert_that(length(time_reference) == 1, !is.na(time_reference))
  }
  if (!is.null(crop_start) && !is.null(crop_end)) {
    if (as.POSIXct(crop_start, tz = "UTC") >= as.POSIXct(crop_end, tz = "UTC")) {
      cli::cli_abort(c(
        "x" = "{.arg crop_start} must be strictly earlier than {.arg crop_end}.",
        "i" = "Received {.arg crop_start} = {.val {crop_start}} and {.arg crop_end} = {.val {crop_end}}."
      ))
    }
  }

  if (is.null(manufacturer)) {
    if (is.data.frame(pressure_file)) {
      manufacturer <- "tabular"
    } else {
      assertthat::assert_that(assertthat::is.dir(directory))
      if (any(grepl("\\.(pressure|glf)$", list.files(directory)))) {
        manufacturer <- "soi"
      } else if (any(grepl("\\.(deg|lux)$", list.files(directory)))) {
        manufacturer <- "migratetech"
      } else if (any(grepl("\\.lig$", list.files(directory)))) {
        manufacturer <- "bas"
      } else if (any(grepl("_press\\.xlsx$", list.files(directory)))) {
        manufacturer <- "lund"
      } else if (any(grepl("\\.csv$", list.files(directory), ignore.case = TRUE))) {
        csv_files <- list.files(
          directory,
          pattern = "\\.csv$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        csv_names <- tolower(basename(csv_files))
        manufacturer <- "tabular"
        pressure_file <- csv_files[match("pressure.csv", csv_names)]
        light_file <- csv_files[match("light.csv", csv_names)]
        acceleration_file <- csv_files[match("acceleration.csv", csv_names)]
        temperature_external_file <- csv_files[match("temperature_external.csv", csv_names)]
        temperature_internal_file <- csv_files[match("temperature_internal.csv", csv_names)]
        magnetic_file <- csv_files[match("magnetic.csv", csv_names)]
      } else {
        cli::cli_abort(c(
          "x" = "We were not able to determine the {.var manufacturer} of tag from the directory
        {.file {directory}}",
          ">" = "Check that this directory contains the file with pressure data (i.e., with
        extension {.val .pressure}, {.val .glf}, {.val .deg}, {.val _press.xlsx} or {.val .csv})"
        ))
      }
    }
  }
  assertthat::assert_that(is.character(manufacturer))
  manufacturer_possible <- c(
    "soi",
    "migratetech",
    "bas",
    "prestag",
    "lund",
    "tabular"
  )
  manufacturer <- match.arg(manufacturer, choices = manufacturer_possible)

  if (manufacturer == "soi") {
    tag <- tag_create_soi(
      id,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_external_file = temperature_external_file,
      temperature_internal_file = temperature_internal_file,
      magnetic_file = magnetic_file,
      quiet = quiet
    )
  } else if (manufacturer == "migratetech") {
    tag <- tag_create_migratetech(
      id,
      directory = directory,
      deg_file = pressure_file,
      light_file = light_file,
      quiet = quiet
    )
  } else if (manufacturer == "bas") {
    tag <- tag_create_bas(
      id,
      directory = directory,
      lig_file = light_file,
      quiet = quiet
    )
  } else if (manufacturer == "lund") {
    tag <- tag_create_lund(
      id,
      directory = directory,
      pressure_file = pressure_file,
      acceleration_light_file = acceleration_file,
      quiet = quiet
    )
  } else if (manufacturer == "prestag") {
    tag <- tag_create_prestag(
      id,
      directory = directory,
      pressure_file = pressure_file,
      quiet = quiet
    )
  } else if (manufacturer == "tabular") {
    tag <- tag_create_tabular(
      id,
      directory = directory,
      pressure_file = pressure_file,
      light_file = light_file,
      acceleration_file = acceleration_file,
      temperature_external_file = temperature_external_file,
      temperature_internal_file = temperature_internal_file,
      magnetic_file = magnetic_file,
      quiet = quiet
    )
  }

  if (assert_pressure) {
    if (!assertthat::has_name(tag, "pressure")) {
      cli::cli_abort(c(
        "x" = "The {.var tag} object does not contain pressure data.",
        ">" = "You can set {.code assert_pressure = FALSE} to create a tag without pressure data."
      ))
    }
  }

  tag <- tag_create_time_correct(tag, time_shift, time_drift, time_reference)

  # Crop date
  tag <- tag_create_crop(
    tag,
    crop_start = crop_start,
    crop_end = crop_end,
    quiet = quiet
  )

  tag$param$tag_create$time_shift <- time_shift
  tag$param$tag_create$time_drift <- time_drift

  return(tag)
}

# Detect full path from the argument file.
#' @noRd
tag_create_detect <- function(file, directory, quiet = TRUE) {
  if (is.null(file)) {
    return(NULL)
  }
  if (is.na(file)) {
    return(NULL)
  }
  if (file.exists(file)) {
    return(file)
  }
  if (file == "") {
    return(NULL)
  }

  # Find files in directory ending with `file`
  path <- list.files(
    directory,
    pattern = glue::glue(file, "$"),
    full.names = TRUE
  )

  # Remove temporary file and those with word "test"
  path <- path[!grepl("~\\$|test", path, ignore.case = TRUE)]
  path <- path[!grepl("(?i)(?<!mag)calib", path, perl = TRUE)]

  if (length(path) == 0) {
    if (!quiet) {
      cli::cli_warn(c(
        "!" = glue::glue("No file is matching '", file, "'."),
        ">" = "This sensor will be ignored."
      ))
    }
    return(NULL)
  }
  if (length(path) > 1) {
    cli::cli_warn(c(
      "!" = "Multiple files matching {.var {file}}: {.file {path}}",
      ">" = "The function will continue with the first one."
    ))
    return(path[1])
  }
  return(path)
}


#' Read data file with a DTO format (Date Time Observation)
#'
#' @param sensor_path Full path of the file (directory + file)
#' @param skip Number of lines of the data file to skip before beginning to read data.
#' @param colIndex of the column of the data to take as observation.
#' @param date_format Format of the date (see [`strptime()`]).
#' @noRd
tag_create_dto <- function(
  sensor_path,
  skip = 6,
  col = 3,
  date_format = "%d.%m.%Y %H:%M",
  quiet = FALSE
) {
  data_raw <- utils::read.delim(
    sensor_path,
    skip = skip,
    sep = "",
    header = FALSE
  )

  # Remove Invalid byte: FD from migratech
  data_raw <- data_raw[!data_raw[, 1] == "Invalid", ]

  sensor_data <- data.frame(
    date = as.POSIXct(strptime(
      paste(data_raw[, 1], data_raw[, 2]),
      tz = "UTC",
      format = date_format
    ))
  )

  for (i in seq_along(col)) {
    name <- if (i == 1) "value" else paste0("value", i)
    sensor_data[[name]] <- as.numeric(data_raw[[col[i]]])
  }

  if (anyNA(sensor_data$value)) {
    cli::cli_abort(c(
      x = "Invalid data in {.file {sensor_path)} at line(s): {skip + which(is.na(sensor_data$value))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  if (!quiet) {
    cli::cli_bullets(c("v" = "Read {.file {sensor_path}}"))
  }
  return(sensor_data)
}

#' Correct sensor timestamps with a constant shift and linear drift
#' @noRd
tag_create_time_correct <- function(tag, time_shift, time_drift, time_reference) {
  sensors <- setdiff(names(tag), "param")
  shift <- drift <- stats::setNames(rep(0, length(sensors)), sensors)
  if (is.numeric(time_shift)) {
    shift[] <- time_shift
  } else {
    shift[names(time_shift)] <- unlist(time_shift)
  }
  if (is.numeric(time_drift)) {
    drift[] <- time_drift
  } else {
    drift[names(time_drift)] <- unlist(time_drift)
  }
  if (is.null(time_reference) && any(drift[sensors] != 0)) {
    time_reference <- do.call(min, lapply(tag[sensors], function(sensor) min(sensor$date)))
  }
  for (sensor in sensors) {
    if (drift[[sensor]] != 0) {
      tag[[sensor]]$date[] <- tag[[sensor]]$date +
        shift[[sensor]] * 3600 +
        as.numeric(difftime(tag[[sensor]]$date, time_reference, units = "secs")) *
          drift[[sensor]] /
          (365.25 * 24)
    } else {
      tag[[sensor]]$date[] <- tag[[sensor]]$date + shift[[sensor]] * 3600
    }
  }
  tag$param$tag_create["time_reference"] <- list(time_reference)
  tag
}

#' Crop sensor data.frame
#' @noRd
tag_create_crop <- function(tag, crop_start, crop_end, quiet = TRUE) {
  has_data <- FALSE
  for (sensor in setdiff(names(tag), "param")) {
    if (sensor %in% names(tag)) {
      # Crop time
      if (!is.null(crop_start)) {
        tag[[sensor]] <- tag[[sensor]][
          tag[[sensor]]$date >= as.POSIXct(crop_start, tz = "UTC"),
        ]
      }
      if (!is.null(crop_end)) {
        tag[[sensor]] <- tag[[sensor]][
          tag[[sensor]]$date < as.POSIXct(crop_end, tz = "UTC"),
        ]
      }
      has_data <- has_data || nrow(tag[[sensor]]) > 0

      if (!quiet) {
        # Check irregular time
        dtime <- as.numeric(diff(tag[[sensor]]$date), units = "secs")
        if (any(abs(dtime - dtime[1]) > 1e-6)) {
          cli::cli_warn(
            "Irregular time spacing for {.field {sensor}}: {tag[[sensor]]$date[which(abs(dtime - dtime[1]) > 1e-6)]}."
          )
        }

        if (nrow(tag[[sensor]]) == 0) {
          cli::cli_warn(c(
            "!" = "Empty {.field {sensor}} sensor dataset",
            ">" = "Check crop date."
          ))
        }
      }
    }
  }

  if ((!is.null(crop_start) || !is.null(crop_end)) && !has_data) {
    cli::cli_abort(c(
      "x" = "No data left after cropping.",
      "i" = "Check {.arg crop_start} and {.arg crop_end}."
    ))
  }

  # Add parameter information
  tag$param$tag_create$crop_start <- crop_start
  tag$param$tag_create$crop_end <- crop_end

  tag
}
