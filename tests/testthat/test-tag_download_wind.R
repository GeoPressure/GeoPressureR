library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

# Helper to create mock tag
create_mock_tag <- function() {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(extent = c(-16, 23, 0, 50), scale = 1)

  # Add pressure data
  tag$pressure <- data.frame(
    date = seq(
      as.POSIXct("2017-06-20 00:00:00", tz = "UTC"),
      as.POSIXct("2017-06-25 00:00:00", tz = "UTC"),
      by = "hour"
    ),
    value = runif(121, 950, 1013)
  )

  # Add stap data with required columns
  tag$stap <- data.frame(
    stap_id = 1:2,
    start = as.POSIXct(c("2017-06-20 00:00:00", "2017-06-23 00:00:00"), tz = "UTC"),
    end = as.POSIXct(c("2017-06-21 00:00:00", "2017-06-24 00:00:00"), tz = "UTC"),
    known_lat = c(NA, NA),
    known_lon = c(NA, NA),
    include = c(TRUE, TRUE)
  )

  # Ensure param$tag_set_map exists
  if (is.null(tag$param$tag_set_map)) {
    tag$param$tag_set_map <- list(extent = c(-16, 23, 0, 50), scale = 1)
  }

  tag
}

test_that("tag_download_wind() | input validation", {
  tag <- create_mock_tag()

  # No map
  expect_error(tag_download_wind(tag_create("18LX", quiet = TRUE)), "tag_set_map")

  # Mock API for subsequent checks
  local_mocked_bindings(
    wf_request_batch = function(...) list(code = 200, message = "Success"),
    .package = "ecmwfr"
  )

  # Invalid parameters
  tmp <- withr::local_tempdir()
  file_func <- function(s, t) file.path(tmp, paste0(t, "_", s, ".nc"))

  expect_error(tag_download_wind(tag, extent = 1:3), "length.*extent.*4")
  expect_error(tag_download_wind(tag, file = "not_func"), "file.*function")
  expect_error(tag_download_wind(tag, variable = 123, file = file_func), "variable.*character")
  # Suppress directory creation warning that happens before validation
  suppressWarnings(
    expect_error(
      tag_download_wind(tag, include_stap_id = 99, file = file_func),
      "include_stap_id.*stap"
    )
  )
})

test_that("tag_download_wind() | file handling and warnings", {
  tag <- create_mock_tag()
  tmp <- withr::local_tempdir()

  local_mocked_bindings(
    wf_request_batch = function(...) list(code = 200, message = "Success"),
    wf_set_key = function(...) invisible(NULL),
    .package = "ecmwfr"
  )

  # Directory creation warning
  file_func <- function(s, t) file.path(tmp, "subdir", paste0(t, "_", s, ".nc"))
  expect_warning(
    tag_download_wind(tag, include_stap_id = 1, file = file_func),
    "did not exist"
  )
  expect_true(dir.exists(file.path(tmp, "subdir")))

  # Last stap warning
  expect_warning(
    tag_download_wind(tag, include_stap_id = c(1, 2), file = file_func),
    "included the last stationary period"
  )

  # Deprecated token warning
  expect_warning(
    tag_download_wind(tag, include_stap_id = 1, cds_token = "x", file = file_func),
    "deprecated"
  )

  # Existing file error
  dir.create(file.path(tmp, "exist"))
  f_exist <- file.path(tmp, "exist", "18LX_1.nc")
  file.create(f_exist)
  expect_error(
    tag_download_wind(
      tag,
      include_stap_id = 1,
      file = function(s, t) file.path(tmp, "exist", paste0(t, "_", s, ".nc")),
      overwrite = FALSE
    ),
    "already ERA5 data file"
  )

  # Skip existing file (no error)
  expect_no_error(
    tag_download_wind(
      tag,
      include_stap_id = NULL,
      file = function(s, t) file.path(tmp, "exist", paste0(t, "_", s, ".nc")),
      overwrite = FALSE
    )
  )
})

test_that("tag_download_wind() | request generation", {
  tag <- create_mock_tag()
  # Set specific pressure to test level selection
  tag$pressure$value <- rep(c(1000, 500, 200), length.out = nrow(tag$pressure))

  captured <- new.env()

  local_mocked_bindings(
    wf_request_batch = function(request_list, workers, ...) {
      captured$request <- request_list
      captured$workers <- workers
      list(code = 200, message = "Success")
    },
    .package = "ecmwfr"
  )

  tmp <- withr::local_tempdir()

  tag_download_wind(
    tag,
    include_stap_id = 1,
    variable = c("temperature", "specific_humidity"),
    workers = 5,
    file = function(s, t) file.path(tmp, paste0(t, "_", s, ".nc"))
  )

  # Check workers
  expect_equal(captured$workers, 5)

  # Check request structure
  req <- captured$request[[1]]
  expect_type(req, "list")
  expect_equal(req$dataset_short_name, "reanalysis-era5-pressure-levels")
  expect_equal(req$product_type, "reanalysis")
  expect_equal(req$data_format, "netcdf")
  expect_equal(req$area, c(50, -16, 0, 23)) # N, W, S, E
  expect_equal(req$target, "18LX_1.nc")

  # Check variables are forwarded verbatim, using names CDS accepts
  expect_equal(req$variable, c("temperature", "specific_humidity"))

  # Check pressure levels (should cover 200-1000)
  expect_true(min(req$pressure_level) <= 200)
  expect_true(max(req$pressure_level) >= 1000)

  # Check time
  expect_true(all(c("year", "month", "day", "time") %in% names(req)))
  expect_gt(length(req$time), 0)
})

# Small ERA5-like file, as CDS returns it: float values, several days x hours
write_era5_float <- function(path, lon, lat, pres, time, value) {
  dims <- list(
    ncdf4::ncdim_def("longitude", "degrees_east", lon),
    ncdf4::ncdim_def("latitude", "degrees_north", lat),
    ncdf4::ncdim_def("pressure_level", "hPa", pres),
    ncdf4::ncdim_def("valid_time", "seconds since 1970-01-01", as.numeric(time))
  )
  def <- lapply(names(value), \(v) ncdf4::ncvar_def(v, "m s**-1", dims, NA, prec = "float"))
  nc <- ncdf4::nc_create(path, def, force_v4 = TRUE)
  for (i in seq_along(def)) {
    ncdf4::ncvar_put(nc, def[[i]], value[[i]])
  }
  ncdf4::nc_close(nc)
}

test_that("tag_download_wind() | CDS files keep the flight hours, packed CF", {
  skip_if_not_installed("ncdf4")
  tag <- create_mock_tag()
  tmp <- withr::local_tempdir()
  file_func <- function(s, t) file.path(tmp, glue::glue("{t}_{s}.nc"))
  lon <- seq(-1, 1, 0.25)
  lat <- seq(46, 45, -0.25)
  pres <- c(1000, 975, 950, 925)
  # CDS returns the day x hour product: 4 days for a flight over 3 days
  time <- seq(as.POSIXct("2017-06-20 00:00", tz = "UTC"), by = 3600, length.out = 96)
  set.seed(1)
  value <- replicate(2, array(rnorm(9 * 5 * 4 * 96, 0, 10), c(9, 5, 4, 96)), simplify = FALSE)
  names(value) <- c("u", "v")
  local_mocked_bindings(
    wf_request_batch = function(...) {
      write_era5_float(file_func(1, "18LX"), lon, lat, pres, time, value)
    },
    .package = "ecmwfr"
  )
  tag$pressure$value <- 960
  tag_download_wind(tag, include_stap_id = 1, file = file_func)

  nc <- ncdf4::nc_open(file_func(1, "18LX"))
  withr::defer(ncdf4::nc_close(nc))
  flight_time <- seq(as.POSIXct("2017-06-21 00:00", tz = "UTC"), by = 3600, length.out = 50)
  expect_equal(as.numeric(ncdf4::ncvar_get(nc, "valid_time")), as.numeric(flight_time))
  expect_equal(c(ncdf4::ncvar_get(nc, "pressure_level")), pres)
  expect_equal(nc$var$u$prec, "short")
  expect_equal(ncdf4::ncatt_get(nc, "u", "standard_name")$value, "eastward_wind")
  expect_equal(ncdf4::ncatt_get(nc, "u", "units")$value, "m s**-1")
  expect_equal(ncdf4::ncatt_get(nc, 0, "Conventions")$value, "CF-1.7")
  # 0.01 m/s steps: rounding error at most half a step
  id <- match(flight_time, time)
  expect_lte(max(abs(ncdf4::ncvar_get(nc, "u") - value$u[,,, id])), 0.005 + 1e-6)
  expect_lte(max(abs(ncdf4::ncvar_get(nc, "v") - value$v[,,, id])), 0.005 + 1e-6)
})
