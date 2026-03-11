# Create from tabular data (in-memory or CSV files)
#' @noRd
tag_create_tabular <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  light_file = NULL,
  acceleration_file = NULL,
  temperature_external_file = NULL,
  temperature_internal_file = NULL,
  magnetic_file = NULL,
  quiet = FALSE
) {
  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  pressure <- tag_create_tabular_read(
    pressure_file,
    directory = directory,
    col_name = c("datetime", "value"),
    quiet = quiet
  )
  if (!is.null(pressure$data)) {
    tag_create_tabular_check(
      pressure$data,
      c("date", "value")
    )
    if (
      nrow(pressure$data) > 0 &&
        (min(pressure$data$value, na.rm = TRUE) < 250 ||
          1100 < max(pressure$data$value, na.rm = TRUE))
    ) {
      cli::cli_warn(
        "Pressure observation should be between 250 hPa (~10000m) and 1100 hPa (sea level at 1013hPa). Check unit of pressure data.frame provided."
      )
    }
    tag$pressure <- pressure$data
    tag$param$tag_create$pressure_file <- pressure$meta
  }

  light <- tag_create_tabular_read(
    light_file,
    directory = directory,
    col_name = c("datetime", "value"),
    quiet = quiet
  )
  if (!is.null(light$data)) {
    tag_create_tabular_check(
      light$data,
      c("date", "value")
    )
    tag$light <- light$data
    tag$param$tag_create$light_file <- light$meta
  }

  acceleration <- tag_create_tabular_read(
    acceleration_file,
    directory = directory,
    col_name = c("datetime", "value"),
    quiet = quiet
  )
  if (!is.null(acceleration$data)) {
    tag_create_tabular_check(
      acceleration$data,
      c("date", "value")
    )
    tag$acceleration <- acceleration$data
    tag$param$tag_create$acceleration_file <- acceleration$meta
  }

  temperature_external <- tag_create_tabular_read(
    temperature_external_file,
    directory = directory,
    col_name = c("datetime", "value"),
    quiet = quiet
  )
  if (!is.null(temperature_external$data)) {
    tag_create_tabular_check(
      temperature_external$data,
      c("date", "value")
    )
    tag$temperature_external <- temperature_external$data
    tag$param$tag_create$temperature_external_file <- temperature_external$meta
  }

  temperature_internal <- tag_create_tabular_read(
    temperature_internal_file,
    directory = directory,
    col_name = c("datetime", "value"),
    quiet = quiet
  )
  if (!is.null(temperature_internal$data)) {
    tag_create_tabular_check(
      temperature_internal$data,
      c("date", "value")
    )
    tag$temperature_internal <- temperature_internal$data
    tag$param$tag_create$temperature_internal_file <- temperature_internal$meta
  }

  magnetic <- tag_create_tabular_read(
    magnetic_file,
    directory = directory,
    col_name = c(
      "datetime",
      "magnetic_x",
      "magnetic_y",
      "magnetic_z",
      "acceleration_x",
      "acceleration_y",
      "acceleration_z"
    ),
    quiet = quiet
  )
  if (!is.null(magnetic$data)) {
    tag_create_tabular_check(
      magnetic$data,
      c(
        "date",
        "acceleration_x",
        "acceleration_y",
        "acceleration_z",
        "magnetic_x",
        "magnetic_y",
        "magnetic_z"
      )
    )
    tag$magnetic <- magnetic$data
    tag$param$tag_create$magnetic_file <- magnetic$meta
  }

  tag$param$tag_create$manufacturer <- "tabular"
  tag$param$tag_create$directory <- directory

  tag
}

#' @noRd
tag_create_tabular_read <- function(x, directory, col_name, quiet = FALSE) {
  if (is.null(x) || isTRUE(is.na(x))) {
    return(list(data = NULL, meta = NULL))
  }

  if (inherits(x, "data.frame")) {
    return(list(data = x, meta = "in_memory"))
  }

  assertthat::assert_that(is.character(x), length(x) == 1)
  path <- if (file.exists(x)) x else file.path(directory, x)
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "x" = "File {.file {path}} does not exist.",
      "i" = "Provide an existing file path or an in-memory table."
    ))
  }

  list(
    data = tag_create_csv(path, col_name = col_name, quiet = quiet),
    meta = path
  )
}

#' @noRd
tag_create_tabular_check <- function(tabular_data, cols) {
  assertthat::assert_that(inherits(tabular_data, "data.frame"))
  assertthat::assert_that(assertthat::has_name(tabular_data, cols))
  assertthat::assert_that(inherits(tabular_data$date, "POSIXct"))
  assertthat::assert_that(assertthat::are_equal(attr(tabular_data$date, "tzone"), "UTC"))
}


#' Read data file with a CSV format
#' @noRd
tag_create_csv <- function(sensor_path, col_name, quiet = FALSE) {
  sensor_data <- utils::read.csv(sensor_path)

  # Check if all specified columns are present
  missing_cols <- setdiff(col_name, names(sensor_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "The following columns are missing in {.file {sensor_path}}: {glue::glue_collapse(missing_cols, ', ')}"
    )
  }

  # Rename column datetime to date and convert to posixct
  names(sensor_data)[names(sensor_data) == "datetime"] <- "date"
  sensor_data$date <- as.POSIXct(sensor_data$date, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  if (anyNA(sensor_data$date)) {
    sensor_data$date <- as.POSIXct(strptime(
      sensor_data$date,
      format = "%Y-%m-%dT%H:%M:%OS",
      tz = "UTC"
    ))
  }
  if (anyNA(sensor_data$date)) {
    cli::cli_abort(c(
      x = "Invalid date in {.file {sensor_path}} at line(s): {which(is.na(sensor_data$date))}",
      i = "Check and fix the corresponding lines"
    ))
  }

  if (!quiet) {
    cli::cli_bullets(c("v" = "Read {.file {sensor_path}}"))
  }

  return(sensor_data)
}
