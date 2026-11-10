# Read PresTag tag files
#' @noRd
tag_create_prestag <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  quiet = FALSE
) {
  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Find file path
  if (is.null(pressure_file)) {
    pressure_file <- ".txt"
  }
  pressure_path <- tag_create_detect(pressure_file, directory, quiet = quiet)
  if (is.null(pressure_path)) {
    cli::cli_abort(c(
      "x" = "No file matching {.val {pressure_file}} in {.file {directory}}.",
      "!" = "{.arg pressure_file} is required."
    ))
  }

  # Read
  data_raw <- utils::read.delim(
    pressure_path,
    header = FALSE,
    comment.char = "#",
    sep = ","
  )

  # convert epoch to Posixt
  timestamps <- as.POSIXct(data_raw$V1, origin = "1970-01-01", tz = "UTC")

  # Separate pressure and temperature
  sensor_data <- utils::read.table(
    text = data_raw$V2,
    sep = ":",
    col.names = c("sensor", "value")
  )
  sensor_data$date <- timestamps

  # Set to NA any negative pressure value
  sensor_data$value[sensor_data$sensor == "P" & sensor_data$value < 0] <- NA

  # Create sensor data.frame
  tag$pressure <- sensor_data[sensor_data$sensor == "P", c("date", "value")]
  tag$temperature_external <- sensor_data[sensor_data$sensor == "T", c("date", "value")]

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$manufacturer <- "prestag"
  tag$param$tag_create$directory <- directory

  return(tag)
}
