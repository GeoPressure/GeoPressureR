# Read PresTag tag files
#' @noRd
tag_create_prestag <- function(
  id,
  directory = glue::glue("./data/raw-tag/{id}"),
  pressure_file = NULL,
  quiet = FALSE
) {
  assertthat::assert_that(is.character(id))
  assertthat::assert_that(is.logical(quiet))

  # Create tag
  tag <- structure(list(param = param_create(id = id)), class = "tag")

  # Find file path
  if (is.null(pressure_file)) {
    pressure_file <- ".txt"
  }
  pressure_path <- tag_create_detect(pressure_file, directory, quiet = quiet)
  if (is.null(pressure_path)) {
    cli::cli_abort(c(
      "x" = "There are no file {.val {pressure_path}}",
      "!" = "{.var pressure_path} is required"
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

  # sensor_data2 <- read.table(text = data_raw$V3[data_raw$V3!=""],
  #                  sep = ":", col.names = c("sensor", "value"))
  # sensor_data2$date = timestamps[data_raw$V3!=""]
  # sensor_data = rbind(sensor_data, sensor_data2)

  # Set to NA any negative value
  sensor_data$value[sensor_data$value < 0] <- NA

  # Create sensor data.frame
  tag$pressure <- sensor_data[sensor_data$sensor == "P", -which(names(sensor_data) == "sensor")]
  tag$temperature <- sensor_data[sensor_data$sensor == "T", -which(names(sensor_data) == "sensor")]

  # Add parameter information
  tag$param$tag_create$pressure_file <- pressure_path
  tag$param$tag_create$manufacturer <- "prestag"
  tag$param$tag_create$directory <- directory

  return(tag)
}
