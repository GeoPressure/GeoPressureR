#' @rdname pressurepath_create
#' @export
pressurepath_create_api <- function(
  tag,
  path = tag2path(tag),
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "both",
  preprocess = FALSE,
  workers = "auto",
  quiet = FALSE,
  debug = FALSE
) {
  pressurepath_create(
    tag = tag,
    path = path,
    variable = variable,
    solar_dep = solar_dep,
    era5_dataset = era5_dataset,
    preprocess = preprocess,
    workers = workers,
    source = "api",
    quiet = quiet,
    debug = debug
  )
}

pressurepath_create_api_impl <- function(
  tag,
  path = tag2path(tag),
  pressurepath,
  variable = c("altitude", "surface_pressure"),
  solar_dep = 0,
  era5_dataset = "both",
  preprocess = FALSE,
  workers = "auto",
  quiet = FALSE,
  debug = FALSE
) {
  era5_dataset <- match.arg(
    era5_dataset,
    choices = c("both", "land", "single-levels")
  )

  # Validate against what this ERA5 product actually carries. ERA5-Land has 70 bands against 292
  # for single levels, so the allowed set is not the same for every value of `era5_dataset`.
  pressurepath_variable_check(variable, "api", era5_dataset)

  # Check workers
  assertthat::assert_that(is.numeric(workers) | workers == "auto")
  # GEE allows up to 100 requests at the same time, so we set the workers a little bit below
  if (workers == "auto") {
    workers <- max(1, min(90, round(nrow(pressurepath) / 1500)))
  }
  assertthat::assert_that(workers > 0 & workers < 100)

  # Format query
  body <- list(
    lon = pressurepath$lon,
    lat = pressurepath$lat,
    time = as.numeric(as.POSIXct(pressurepath$date)),
    variable = variable,
    dataset = era5_dataset,
    pressure = pressurepath$pressure_tag * 100,
    workers = workers
  )

  if (!quiet) {
    cli::cli_progress_step(
      "Generate request on {.url glp.mgravey.com/GeoPressure/v2/pressurePath} and download csv"
    )
  }

  if (debug) {
    temp_file <- tempfile("log_pressurepath_", fileext = ".json")
    write(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), temp_file)
    cli::cli_text("Body request file: {.file {temp_file}}")
  }

  req <- httr2::request(
    "https://glp.mgravey.com/GeoPressure/v2/pressurePath/"
  ) |>
    httr2::req_body_json(body, digit = 5, auto_unbox = FALSE)

  if (debug) {
    req <- httr2::req_verbose(
      req,
      body_req = TRUE,
      body_resp = TRUE,
      info = TRUE
    )
  }

  # Perform the request and convert the response to data.frame
  resp <- httr2::req_perform(req)
  resp_body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  # GeoPressureAPI flags configurations whose `altitude` cannot be trusted.
  if (!is.null(resp_body$warning)) {
    cli::cli_warn(c("!" = "GeoPressureAPI: {resp_body$warning}"))
  }
  resp_data <- resp_body$data

  # If variable requested does not exist, the API return an empty list, which we
  # convert here as a NA
  out <- as.data.frame(lapply(resp_data, \(x) if (length(x) > 0) x else NA))

  # Check if the response is empty
  cols_with_na <- names(out)[vapply(out, function(x) anyNA(x), logical(1))]
  if (length(cols_with_na) > 0) {
    cli::cli_warn(
      "The following columns contain `NA` values: {.val {cols_with_na}}"
    )
  }

  if (!quiet) {
    cli::cli_progress_step("Post-process pressurepath")
  }

  # Convert time to date
  out$time <- as.POSIXct(out$time, origin = "1970-01-01", tz = "UTC")
  names(out)[names(out) == "time"] <- "date"

  # Add out to pressurepath
  pressurepath <- merge(
    pressurepath,
    out,
    all.x = TRUE
  )

  pressurepath_finalize(
    pressurepath,
    tag,
    path,
    preprocess,
    solar_dep,
    variable = variable,
    era5_dataset = era5_dataset,
    source = "api"
  )
}
