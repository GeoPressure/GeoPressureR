test_that("ARCO time series matches tag times without a remote read", {
  local_mocked_bindings(
    era5_arco_client = function(...) list(),
    era5_arco_read = function(variable, era5_dataset, lon, lat, date, ...) {
      expect_equal(c(lon, lat), if (era5_dataset == "land") c(16.4, 46.4) else c(16.25, 46.25))
      if (variable == "surface_pressure") 100000 + 100 * seq_along(date) else 280 + seq_along(date)
    },
    era5_surface_elevation = function(lon, ...) rep(0, length(lon)),
    .package = "GeoPressureR"
  )

  impl <- get("geopressure_timeseries_arco_impl", asNamespace("GeoPressureR"))
  pressure <- data.frame(
    date = as.POSIXct(c("2020-01-01 00:15:00", "2020-01-01 01:45:00"), tz = "UTC"),
    value = c(990, 991)
  )
  out <- impl(46.37, 16.37, pressure = pressure, quiet = TRUE, era5_dataset = "single-levels")
  expect_equal(out$date, pressure$date)
  expect_equal(out$surface_pressure, c(1001, 1002))
  expect_equal(out$surface_pressure_norm, c(990, 991))
  expect_true(all(is.finite(out$altitude)))

  out <- impl(
    46.37,
    16.37,
    start_time = pressure$date[1],
    end_time = pressure$date[2],
    quiet = TRUE,
    era5_dataset = "land"
  )
  expect_equal(format(out$date, "%H:%M"), c("01:00", "02:00"))
  expect_equal(out$surface_pressure, c(1001, 1002))
})

test_that("ARCO pressure path assembles requested variables in memory", {
  local_mocked_bindings(
    era5_arco_read_points = function(variable, era5_dataset, lon, lat, date, debug) {
      expect_equal(era5_dataset, "single-levels")
      expect_equal(lon, c(16, 16.25))
      expect_equal(format(date, "%H:%M"), c("01:00", "02:00"))
      if (variable == "surface_pressure") c(100000, 100100) else c(280, 281)
    },
    era5_surface_elevation = function(lon, ...) rep(0, length(lon)),
    .package = "GeoPressureR"
  )

  pressurepath <- data.frame(
    stap_id = c(1L, 1L),
    label = c("", ""),
    date = as.POSIXct(c("2020-01-01 00:15:00", "2020-01-01 01:45:00"), tz = "UTC"),
    pressure_tag = c(990, 991),
    lon = c(16.12, 16.37),
    lat = c(46.12, 46.37)
  )
  tag <- list(param = list(id = "test", geopressure_map = list(sd = 1)))
  path <- data.frame(stap_id = 1L, lon = 16, lat = 46)
  impl <- get("pressurepath_create_arco_impl", asNamespace("GeoPressureR"))
  out <- impl(
    tag,
    path,
    pressurepath,
    variable = c("altitude", "surface_pressure", "temperature_2m"),
    solar_dep = NULL,
    era5_dataset = "single-levels",
    quiet = TRUE
  )

  expect_equal(out$surface_pressure, c(1000, 1001))
  expect_equal(out$surface_pressure_norm, c(990, 991))
  expect_equal(out$temperature_2m, c(280, 281))
  expect_true(all(is.finite(out$altitude)))
  expect_equal(attr(out, "source"), "arco")
  expect_equal(attr(out, "variable"), c("altitude", "surface_pressure", "temperature_2m"))
})

test_that("ARCO point reader groups cells by physical chunk", {
  skip_if_not_installed("Rarr")
  reads <- 0L
  local_mocked_bindings(
    era5_arco_client = function(...) list(),
    .package = "GeoPressureR"
  )
  local_mocked_bindings(
    read_zarr_array = function(array, index, s3_client) {
      reads <<- reads + 1L
      base::array(
        rep(index[[3]], each = length(index[[1]]) * length(index[[2]])),
        dim = lengths(index)
      )
    },
    .package = "Rarr"
  )

  read_points <- get("era5_arco_read_points", asNamespace("GeoPressureR"))
  date <- rep(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), 3)
  out <- read_points("surface_pressure", "single-levels", c(16, 16.25, 17), rep(46, 3), date, FALSE)
  expect_equal(out, c(785, 786, 789))
  expect_equal(reads, 2L)

  out <- read_points("surface_pressure", "land", c(-180, -179.9), rep(46, 2), date[1:2], FALSE)
  expect_equal(out, c(3600, 1))
  expect_equal(reads, 4L)
})
