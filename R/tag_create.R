#' Create a `tag` object
#'
#' @description
#' Read sensor measurements into a GeoPressureR `tag` object. Optionally correct timestamps
#' for clock offset and drift, then crop to the recording period of interest.
#'
#' @section Workflow:
#' Data are **read -> corrected -> cropped**. Keep the original files and specify clock
#' corrections in `tag_create()`. All sensor timestamps are rounded to the nearest millisecond
#' after correction and before cropping, including when no correction is supplied. A warning
#' identifies sensors where rounding makes distinct timestamps identical; all measurements are kept.
#' Pressure is required by default; use `assert_pressure = FALSE`
#' for tags without pressure data.
#'
#' @section Sensor files:
#'
#' By default, files are read from `./data/raw-tag/{id}` and the manufacturer is detected from
#' the directory where possible. Set `manufacturer` explicitly when needed, including for PresTag.
#' Sensor file arguments accept a file path or filename pattern, such as `"*.pressure"`.
#' Supported formats are:
#'
#' - SOI (`"soi"`): pressure `*.pressure`, light `*.glf`, acceleration `*.acceleration`,
#'   external temperature `*.temperature` or `*.airtemperature`, internal temperature
#'   `*.bodytemperature`, and magnetic data `*.magnetic`.
#' - Migratetech (`"migratetech"`): pressure and acceleration `*.deg`, light `*.lux`.
#' - BAS (`"bas"`): light `*.lig` only; use `assert_pressure = FALSE`.
#' - Lund CAnMove (`"lund"`): pressure `*_press.xlsx`, light and acceleration `*_acc.xlsx`.
#' - PresTag (`"prestag"`): pressure `*.txt`.
#'
#' Manufacturer readers detect available sensor files when their arguments are `NULL`.
#' For SOI, use `NA` to skip a sensor.
#'
#' @section Tabular input:
#'
#' With `manufacturer = "tabular"`, each sensor argument accepts a data.frame, tibble, or CSV path.
#' Tables need a UTC POSIXct `date` column and a `value` column; pressure values must be in hPa.
#' Magnetic tables instead use the axis columns listed under Value below.
#'
#' CSV files use `datetime` instead of `date`, with UTC timestamps such as
#' `"2025-01-01T00:00:00"`; fractional seconds are supported. CSV files named `pressure.csv`,
#' `light.csv`, etc. can be detected automatically from `directory`. Omit optional tabular sensors
#' with `NULL`. An in-memory `pressure_file` also selects tabular input automatically.
#'
#' @section Clock correction:
#' - `time_shift` adds a constant offset in hours.
#' - `time_drift` adds a linear correction in hours per year (365.25 days).
#' - `time_reference` is the original recorded time at which drift correction is zero. By default,
#'   it is the earliest measurement across all sensors, before shifts or cropping.
#'
#' Positive corrections add time; negative corrections subtract time. Each correction accepts
#' one value for all sensors or a named list, such as `list(pressure = -12 / 60, light = 0)`.
#' Sensors omitted from a list receive zero; entries for absent sensors are ignored. All sensors
#' share the reference, regardless of recording duration. Drift uses the original timestamps,
#' before adding the shift, and extends linearly before an explicit reference.
#'
#' Correction settings and the reference used for drift are recorded in `tag$param$tag_create`.
#' When changing corrections, rerun the analysis from the original data and regenerate labels
#' that rely on exact timestamps, including TRAINSET labels.
#'
#' @section Cropping:
#' Cropping uses corrected UTC timestamps: `crop_start` is inclusive and `crop_end` is exclusive.
#' Leave either boundary as `NULL` to keep all data on that side.
#'
#' @param id Unique tag identifier.
#' @param manufacturer Data format: `NULL` (automatic), `"soi"`, `"migratetech"`, `"bas"`,
#'   `"lund"`, `"prestag"`, or `"tabular"`.
#' @param directory Directory containing the original sensor files.
#' @param pressure_file Pressure input; tabular values must be in hPa. See Sensor files and Tabular input for input formats.
#' @param light_file Optional light input.
#' @param acceleration_file Optional acceleration input.
#' @param temperature_external_file Optional external or air-temperature input.
#' @param temperature_internal_file Optional internal or body-temperature input.
#' @param magnetic_file Optional magnetic and acceleration input with axis columns.
#' @param time_shift Constant correction in hours. A numeric value for all sensors or a named list
#'   for individual sensors. Default is zero.
#' @param time_drift Linear correction in hours per year (365.25 days). A numeric value for all
#'   sensors or a named list for individual sensors. Default is zero; `-12 / 60` subtracts
#'   12 minutes per year. Convert hours per 30-day month by multiplying by `365.25 / 30`.
#' @param time_reference Original recorded timestamp at which drift correction is zero, as POSIXct
#'   or character in UTC. `NULL` uses the earliest measurement across all sensors. Supply a timestamp
#'   if the clock was synchronised at another time. Only `time_shift` is added at the reference.
#' @param crop_start Inclusive start of the corrected recording period, as POSIXct or character
#'   in UTC. `NULL` keeps all earlier data.
#' @param crop_end Exclusive end of the corrected recording period, as POSIXct or character in UTC.
#'   `NULL` keeps all later data.
#' @param quiet Logical; hide progress messages and sampling-interval warnings.
#' @param assert_pressure Logical; require pressure data in the returned tag.
#'
#' @return A GeoPressureR `tag` containing `param` (see [param_create()]) and the available sensor
#'   tables. Each sensor table has a UTC POSIXct `date` column and the following measurement columns:
#' - `pressure`: `value` in hPa.
#' - `light`, `temperature_external`, `temperature_internal`: `value`.
#' - `acceleration`: `value` and optionally `mean_acceleration_z`. For SOI, `value` is the sum of
#'   successive z-axis acceleration differences over 32 measurements at 10 Hz (jiggle);
#'   `mean_acceleration_z` is their mean z-axis acceleration.
#' - `magnetic`: `magnetic_x`, `magnetic_y`, `magnetic_z`, `acceleration_x`, `acceleration_y`,
#'   `acceleration_z`.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   # Read available sensors.
#'   tag <- tag_create("18LX", quiet = TRUE)
#'
#'   # Crop to the period of interest.
#'   tag <- tag_create("18LX",
#'     crop_start = "2017-08-01", crop_end = "2017-08-05", quiet = TRUE
#'   )
#'
#'   # Select files explicitly when several match the same pattern.
#'   tag <- tag_create("CB621",
#'     pressure_file = "CB621_BAR.deg", light_file = "CB621.lux", quiet = TRUE
#'   )
#'
#'   # Correct selected sensors using a shared recorded reference.
#'   tag <- tag_create("18LX",
#'     time_shift = list(light = 2), time_drift = list(pressure = -12 / 60),
#'     time_reference = "2017-06-20", quiet = TRUE
#'   )
#' })
#'
#' # Read an in-memory table and apply the same correction to all sensors.
#' pressure <- data.frame(
#'   date = as.POSIXct("2025-01-01", tz = "UTC") + (0:48) * 1800,
#'   value = rep(1000, 49)
#' )
#' tag <- tag_create("example", pressure_file = pressure,
#'   time_shift = 2, time_drift = -12 / 60, quiet = TRUE
#' )
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

  distinct_times <- vapply(
    tag[setdiff(names(tag), "param")],
    function(sensor) length(unique(sensor$date)),
    integer(1)
  )
  tag <- tag_create_time_correct(tag, time_shift, time_drift, time_reference)
  for (sensor in names(distinct_times)) {
    collisions <- distinct_times[[sensor]] - length(unique(tag[[sensor]]$date))
    if (collisions > 0) {
      cli::cli_warn(c(
        "!" = "Millisecond rounding created {.val {collisions}} additional duplicate timestamp{?s} for {.field {sensor}}.",
        "i" = "All measurements are retained, but timestamp-based label matching cannot distinguish them."
      ))
    }
  }

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
    tag[[sensor]]$date[] <- round(as.numeric(tag[[sensor]]$date) * 1000) / 1000
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
        # Allow one millisecond of timestamp rounding and floating-point noise.
        if (any(abs(dtime - dtime[1]) > 1.001e-3)) {
          cli::cli_warn(
            "Irregular time spacing for {.field {sensor}}: {tag[[sensor]]$date[which(abs(dtime - dtime[1]) > 1.001e-3)]}."
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
