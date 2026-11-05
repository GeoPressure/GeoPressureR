#' @rdname geopressure_timeseries
#' @export
geopressure_timeseries_api <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = c("single-levels", "land"),
  altitude_formula = "virtual"
) {
  geopressure_timeseries(
    lat = lat,
    lon = lon,
    pressure = pressure,
    start_time = start_time,
    end_time = end_time,
    source = "api",
    quiet = quiet,
    debug = debug,
    era5_dataset = era5_dataset,
    altitude_formula = altitude_formula
  )
}

geopressure_timeseries_api_impl <- function(
  lat,
  lon,
  pressure = NULL,
  start_time = NULL,
  end_time = NULL,
  quiet = FALSE,
  debug = FALSE,
  era5_dataset = "single-levels",
  altitude_formula = "virtual"
) {
  # Format query
  body <- list(lon = lon, lat = lat, dataset = era5_dataset, altitudeFormula = altitude_formula)
  if (!is.null(pressure)) {
    body$time <- as.numeric(as.POSIXct(pressure$date))
    body$pressure <- pressure$value * 100
  } else {
    body$startTime <- as.numeric(as.POSIXct(start_time))
    body$endTime <- as.numeric(as.POSIXct(end_time))
  }

  if (!quiet) {
    cli::cli_progress_step(
      "Generate request on {.url glp.mgravey.com/GeoPressure/v2/timeseries}"
    )
  }

  if (debug) {
    temp_file <- tempfile("log_geopressure_timeseries_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request("https://glp.mgravey.com/GeoPressure/v2/timeseries/") |>
    httr2::req_body_json(body) |>
    httr2::req_error(body = function(resp) {
      if (debug) {
        print(httr2::resp_body_json(resp))
      }
      c(
        "x" = "Error with your request on \
        {.url https://glp.mgravey.com/GeoPressure/v2/timeseries/}",
        ">" = httr2::resp_body_json(resp)$errorMessage,
        "i" = "Please try again with `debug=TRUE`"
      )
    })

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform the request and convert the response to json
  resp <- httr2::req_perform(req)
  resp_data <- httr2::resp_body_json(resp)$data

  # Check for change in position
  if (!quiet && resp_data$distInter > 0) {
    cli::cli_bullets(c(
      "!" = "Requested position is on water and will be moved to the closest point on shore ({.url https://www.google.com/maps/dir/{lat},{lon}/{resp_data$lat},{resp_data$lon}}) located {round(resp_data$distInter / 1000)} km away."
    ))
  }

  if (!quiet) {
    cli::cli_progress_step("Sending request")
  }

  # Prepare request
  req <- httr2::request(resp_data$url)

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform request
  resp <- httr2::req_perform(req)

  # Convert the response to data.frame
  out <- utils::read.csv(text = httr2::resp_body_string(resp))

  # check for errors
  if (nrow(out) == 0) {
    temp_file <- tempfile("log_pressurepath_create", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_abort(c(
      x = "Returned csv file is empty.",
      i = "Check that the time range is non-empty. Log of your JSON request: {.file {temp_file}}"
    ))
  }

  # convert Pa to hPa and rename
  out$pressure <- out$pressure / 100
  names(out)[names(out) == "pressure"] <- "surface_pressure"

  # convert time into date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"

  # Add exact location
  out$lat <- resp_data$lat
  out$lon <- resp_data$lon

  if (!is.null(pressure)) {
    if (nrow(out) != nrow(pressure)) {
      cli::cli_warn(
        "The returned data.frame is had a different number of element than the requested pressure."
      )
    }
    out$date <- geopressure_api_restore_date(pressure$date, as.numeric(out$date))
  }
  return(out)
}

# Restore original dates after the API truncates timestamps to whole seconds.
geopressure_api_restore_date <- function(date, time) {
  if (length(time) == length(date)) {
    return(date)
  }
  # Missing responses cannot be matched safely when several measurements share one second.
  if (anyDuplicated(trunc(as.numeric(date)))) {
    cli::cli_abort(
      "The API returned fewer measurements and truncated timestamps; sub-second measurements cannot be matched unambiguously."
    )
  }
  date[match(time, trunc(as.numeric(date)))]
}
