# Read Swiss Ornithological Institute (SOI) tag files
#' @noRd
tag_create_soi <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  light_file = NULL,
  acceleration_file = NULL,
  temperature_external_file = NULL,
  temperature_internal_file = NULL,
  magnetic_file = NULL,
  mag_axis = c("right", "forward", "down"),
  quiet = FALSE
) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Read SOI settings
  setting_path <- tag_create_detect("*.settings", directory, quiet = TRUE)
  if (!is.null(setting_path)) {
    settings <- jsonlite::fromJSON(setting_path)
    # Normalised logger model and firmware (older SOI files name them HWVersion and SWVersion)
    settings$model <- if (is.null(settings[["HW Version"]])) {
      settings$HWVersion
    } else {
      settings[["HW Version"]]
    }
    settings$firmware <- if (is.null(settings[["FW Version"]])) {
      settings$SWVersion
    } else {
      settings[["FW Version"]]
    }
    tag$param$tag_create$settings <- settings

    # Check for drift: tag clock (StopTimeRTC) minus computer clock (StopTimeReference) at
    # download, in minutes. Positive when the tag clock is ahead.
    settings_time <- function(key) {
      x <- settings[[key]]
      if (!is.character(x) || length(x) != 1) {
        return(as.POSIXct(NA))
      }
      as.POSIXct(strptime(x, tz = "UTC", format = "%d.%m.%Y %H:%M:%S"))
    }
    stop_time_ref <- settings_time("StopTimeReference")
    stop_time_rtc <- settings_time("StopTimeRTC")
    recording_valid <- !identical(
      settings$RecordingStop,
      "timestamp invalid"
    )
    if (recording_valid && !is.na(stop_time_ref) && !is.na(stop_time_rtc)) {
      tag$param$tag_create$settings$clock_drift <- as.numeric(difftime(
        stop_time_rtc,
        stop_time_ref,
        units = "mins"
      ))
      if (abs(tag$param$tag_create$settings$clock_drift) > 30) {
        cli::cli_warn(c(
          "!" = "The SOI setting file {.file {setting_path}} is recording a drift of {format_minutes(abs(tag$param$tag_create$settings$clock_drift))} which seems suspicious.",
          ">" = "Check for error (e.g. timezone)"
        ))
      }
    }
  }

  # Read Pressure
  pressure_path <- tag_create_detect(pressure_file, directory, quiet, default = "*.pressure")
  if (!is.null(pressure_path)) {
    tag$pressure <- tag_create_dto(pressure_path, quiet = quiet)
    tag$pressure$value[tag$pressure$value < 0] <- NA
  }

  # Read light
  light_path <- tag_create_detect(light_file, directory, quiet, default = "*.glf")
  if (!is.null(light_path)) {
    tag$light <-
      tag_create_dto(light_path, quiet = quiet)
    tag$light$value[tag$light$value < 0] <- NA
  }

  # Read acceleration
  acceleration_path <- tag_create_detect(
    acceleration_file,
    directory,
    quiet,
    default = "*.acceleration"
  )
  if (!is.null(acceleration_path)) {
    tag$acceleration <- tag_create_dto(
      acceleration_path,
      col = c(4, 3),
      quiet = quiet
    )
    names(tag$acceleration) <- c("date", "value", "mean_acceleration_z")
    tag$acceleration <- tag_create_soi_gap(tag$acceleration)
    tag$acceleration$value[tag$acceleration$value < 0] <- NA
  }

  # Read External temperature
  temperature_external_path <- tag_create_detect(
    temperature_external_file,
    directory,
    quiet,
    default = "*.temperature"
  )
  if (is.null(temperature_external_file) && is.null(temperature_external_path)) {
    temperature_external_path <- tag_create_detect("*.airtemperature", directory)
  }
  if (!is.null(temperature_external_path)) {
    tag$temperature_external <- tag_create_dto(
      temperature_external_path,
      col = 3,
      quiet = quiet
    )
  }

  # Read Internal temperature
  temperature_internal_path <- tag_create_detect(
    temperature_internal_file,
    directory,
    quiet,
    default = "*.bodytemperature"
  )
  if (!is.null(temperature_internal_path)) {
    tag$temperature_internal <- tag_create_dto(
      temperature_internal_path,
      col = 3,
      quiet = quiet
    )
    # Body temperature cannot be below about 0.3 °C, so -2 can only be a gap
    tag$temperature_internal <- tag_create_soi_gap(tag$temperature_internal)
  }

  # Read magnetism
  magnetic_path <- tag_create_detect(magnetic_file, directory, quiet, default = "*.magnetic")
  if (!is.null(magnetic_path)) {
    tag$magnetic <- tag_create_soi_mag(
      magnetic_path,
      hw_version = tag$param$tag_create$settings$model,
      mag_axis = mag_axis,
      quiet = quiet
    )
    # Add information
    tag$param$tag_create$mag_axis <- attr(tag$magnetic, "mag_axis")
    attr(tag$magnetic, "mag_axis") <- NULL
  }

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$light_file <- light_path
  tag$param$tag_create$acceleration_file <- acceleration_path
  tag$param$tag_create$temperature_external_file <- temperature_external_path
  tag$param$tag_create$temperature_internal_file <- temperature_internal_path
  tag$param$tag_create$magnetic_file <- magnetic_path
  tag$param$tag_create$manufacturer <- "soi"
  tag$param$tag_create$directory <- directory

  return(tag)
}

#' Set SOI decoder gaps to NA
#'
#' The SOI decoder writes `-2` in every column of rows it could not reconstruct. Rows where all
#' measurement columns are `-2` are set to `NA`.
#' @noRd
tag_create_soi_gap <- function(sensor_data) {
  cols <- setdiff(names(sensor_data), "date")
  gap <- Reduce(`&`, lapply(sensor_data[cols], function(x) !is.na(x) & x == -2))
  sensor_data[gap, cols] <- NA
  sensor_data
}

#' Correct magnetic data from SOI data
#'
#' @description
#' The function applies the following three operations:
#'
#' 1. Convert magnetic values to Gauss (https://en.wikipedia.org/wiki/Gauss_(unit))
#' 2. Convert acceleration values to g (https://en.wikipedia.org/wiki/Standard_gravity)
#' 3. Compute mag and acc in the standard forward-right-down frame (raw y forward, raw x right)
#'
#' Two sensors are supported:
#' - LSM303D: hardware version v2.3 and lower
#' - LSM303AGR: hardware version 2.4 and higher
#'
#' Version is detected automatically from `tag$param$tag_create$settings`
#' @noRd
tag_create_soi_mag <- function(
  magnetic_path,
  hw_version,
  mag_axis = c("right", "forward", "down"),
  quiet = FALSE
) {
  assertthat::assert_that(is.logical(quiet))
  assertthat::assert_that(file.exists(magnetic_path))

  mag <- tag_create_dto(magnetic_path, col = seq(4, 9), quiet = quiet)
  names(mag) <- c("date", "mX", "mY", "mZ", "gX", "gY", "gZ")
  mag <- tag_create_soi_gap(mag)

  # Detect hardware version from the logger model (e.g. "HW_GDL3pam_V2.1")
  model <- hw_version
  hw_version <- suppressWarnings(as.double(sub(
    "^(hw_)?GDL3pam[-_]?v?[-_]?",
    "",
    hw_version,
    ignore.case = TRUE
  )))
  # Always warn (also when quiet) as the magnetic and acceleration data are dropped
  if (length(hw_version) != 1 || is.na(hw_version)) {
    cli::cli_warn(c(
      "!" = "The SOI hardware version {.val {model}} could not be detected.",
      "x" = "The magnetic and acceleration data of {.file {magnetic_path}} cannot be calibrated and are not imported.",
      ">" = "Add the {.file .settings} file with its {.field HWVersion} (e.g. {.val HW_GDL3pam_V2.1}) to the tag directory."
    ))
    return(NULL)
  }

  # Automatically define correction values
  if (hw_version <= 2.3) {
    # LSM303D
    corr_m <- 0.00016 # unit G. raw value stored of the ADC (analog-digital converter) to Gauss
    corr_g <- 4 / 65536 # 16-Bit resolution on a full scale of 4g (+/-2g)
  } else {
    # LSM303AGR
    corr_m <- 0.0015 # value stored to Gauss
    corr_g <- 4 / 4096 # 12-Bit resolution on a full scale of 4g (+/-2g)
  }

  # Apply correction
  mag[c("mX", "mY", "mZ")] <- mag[c("mX", "mY", "mZ")] * corr_m
  mag[c("gX", "gY", "gZ")] <- mag[c("gX", "gY", "gZ")] * corr_g

  # On all hardware versions, raw y points forward and raw x to the right (checked against
  # tracks), which is the default `mag_axis`. A tag mounted reversed has forward = -y; with a
  # leg-loop harness it then rests tilted backwards (y < 0 in static samples). This is not used to
  # flip the axes because breast-harness tags (swifts) can rest with y < 0 while forward = +y.
  gn <- sqrt(mag$gX^2 + mag$gY^2 + mag$gZ^2)
  fwd <- stats::median(mag$gY[abs(gn / stats::median(gn, na.rm = TRUE) - 1) < 0.05], na.rm = TRUE)
  if (!quiet && mag_axis[2] == "forward" && isTRUE(fwd < -0.3)) {
    cli::cli_alert_warning(
      "The tag rests tilted backwards along its body axis (median static y = {.val {round(fwd, 2)}} g). If it was mounted reversed, use {.code mag_axis = c(\"left\", \"backward\", \"down\")}."
    )
  }

  # Convert the axes orientation to have the format forward-right-down
  mag <- mag_axes(mag, mag_axis)

  # Name variables
  map <- c(
    gX = "acceleration_x",
    gY = "acceleration_y",
    gZ = "acceleration_z",
    mX = "magnetic_x",
    mY = "magnetic_y",
    mZ = "magnetic_z"
  )
  names(mag) <- ifelse(names(mag) %in% names(map), map[names(mag)], names(mag))

  return(mag)
}


#' Convert the mag axes to forward-right-down
#' @noRd
mag_axes <- function(mag, mag_axis) {
  # We use a forward-right-down system. Which means that the sensor needs to be oriented
  ref_axis_name <- matrix(
    c("forward", "backward", "right", "left", "down", "up"),
    2,
    3
  )

  # Assert that all axis are provided correctly, and one of each direction
  assertthat::assert_that(all(mag_axis %in% ref_axis_name))
  assertthat::assert_that(any(ref_axis_name[, 1] %in% mag_axis))
  assertthat::assert_that(any(ref_axis_name[, 2] %in% mag_axis))
  assertthat::assert_that(any(ref_axis_name[, 3] %in% mag_axis))

  # Raw axis providing each forward-right-down axis (inverse permutation, needed for 3-cycles)
  axes_ind <- order(ceiling(match(mag_axis, ref_axis_name) / 2))

  # Find the sign conversion for each of these axes
  axes_sign <- ifelse((match(mag_axis[axes_ind], ref_axis_name)) %% 2, 1, -1)

  # Reorder and sign the axes (works for any number of rows)
  mag[c("gX", "gY", "gZ")] <- Map(`*`, mag[c("gX", "gY", "gZ")][axes_ind], axes_sign)
  mag[c("mX", "mY", "mZ")] <- Map(`*`, mag[c("mX", "mY", "mZ")][axes_ind], axes_sign)

  attr(mag, "mag_axis") <- mag_axis
  return(mag)
}
