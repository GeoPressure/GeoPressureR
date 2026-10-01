test_that("virtual altitude matches the independent GeoPressureAPI reference", {
  # Reference values computed independently from PR #32, including leap dates and both poles.
  lat <- c(46.95, 46.95, 0, -33.87, -33.87, 90, -90, -46, 46)
  date <- as.POSIXct(
    c(
      "2020-01-15",
      "2020-07-15",
      "2020-01-15",
      "2020-07-15",
      "2020-01-15",
      "2020-01-15",
      "2020-07-15",
      "2020-12-31",
      "2020-02-29"
    ),
    tz = "UTC"
  )
  expect_equal(
    pressure_to_altitude(90000, 100000, 288, 500, 280, lat, date),
    c(
      1387.298550585463,
      1382.959077983491,
      1382.389533885315,
      1385.927270830295,
      1382.856547552899,
      1388.800294606306,
      1388.800294606306,
      1383.009395068535,
      1386.592042419869
    ),
    tolerance = 1e-10
  )
  expect_equal(pressure_to_altitude(100000, 100000, 288, 500, 280, lat, date), rep(500, 9))
  expect_equal(
    pressure_to_altitude(90000, 100000, 288, 500, altitude_formula = "standard"),
    1379.358946332310,
    tolerance = 1e-10
  )
})

test_that("ARCO time series defaults to virtual and preserves its output structure", {
  reads <- character()
  local_mocked_bindings(
    era5_arco_client = function(...) list(),
    era5_arco_read = function(variable, date, ...) {
      reads <<- c(reads, variable)
      rep(
        switch(
          variable,
          surface_pressure = 100000,
          temperature_2m = 288,
          dewpoint_temperature_2m = 280
        ),
        length(date)
      )
    },
    era5_surface_elevation = function(...) 500,
    .package = "GeoPressureR"
  )
  pressure <- data.frame(
    date = as.POSIXct(c("2020-01-15 00:15:00", "2020-07-15 00:15:00"), tz = "UTC"),
    value = c(900, 900)
  )
  virtual <- geopressure_timeseries_arco_impl(46.95, 7.45, pressure, quiet = TRUE)
  expect_equal(virtual$altitude, c(1387.298550585463, 1382.959077983491), tolerance = 1e-10)
  expect_true("dewpoint_temperature_2m" %in% reads)
  expect_identical(virtual$date, pressure$date)
  expect_false("dewpoint_temperature_2m" %in% names(virtual))
  reads <- character()
  standard <- geopressure_timeseries_arco_impl(
    46.95,
    7.45,
    pressure,
    quiet = TRUE,
    altitude_formula = "standard"
  )
  expect_equal(standard$altitude, rep(1379.358946332310, 2), tolerance = 1e-10)
  expect_false("dewpoint_temperature_2m" %in% reads)
  expect_identical(attributes(virtual), attributes(standard))
})

test_that("ARCO pressure paths use virtual at the original coordinates and dates", {
  reads <- character()
  local_mocked_bindings(
    era5_arco_read_points = function(variable, date, ...) {
      reads <<- c(reads, variable)
      rep(
        switch(
          variable,
          surface_pressure = 100000,
          temperature_2m = 288,
          dewpoint_temperature_2m = 280
        ),
        length(date)
      )
    },
    era5_surface_elevation = function(lon, ...) rep(500, length(lon)),
    .package = "GeoPressureR"
  )
  pressurepath <- data.frame(
    date = as.POSIXct(c("2020-01-15 00:15:00", "2020-07-15 00:15:00"), tz = "UTC"),
    stap_id = 1L,
    pressure_tag = 900,
    label = "",
    lat = c(46.95, -33.87),
    lon = 16
  )
  tag <- list(param = list(id = "test", geopressure_map = list(sd = 1)))
  path <- data.frame(stap_id = 1L, lon = 16, lat = 46)
  virtual <- pressurepath_create_arco_impl(tag, path, pressurepath, solar_dep = NULL, quiet = TRUE)
  expect_equal(virtual$altitude, c(1387.298550585463, 1385.927270830295), tolerance = 1e-10)
  expect_true("dewpoint_temperature_2m" %in% reads)
  expect_false("dewpoint_temperature_2m" %in% names(virtual))
  reads <- character()
  standard <- pressurepath_create_arco_impl(
    tag,
    path,
    pressurepath,
    solar_dep = NULL,
    quiet = TRUE,
    altitude_formula = "standard"
  )
  expect_equal(standard$altitude, rep(1379.358946332310, 2), tolerance = 1e-10)
  expect_false("dewpoint_temperature_2m" %in% reads)
  expect_identical(attributes(virtual), attributes(standard))
  reads <- character()
  pressurepath_create_arco_impl(
    tag,
    path,
    pressurepath,
    variable = "surface_pressure",
    solar_dep = NULL,
    quiet = TRUE
  )
  expect_identical(reads, "surface_pressure")
})

test_that("all public altitude functions default to virtual and transmit the formula", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      cli::cli_abort("Captured request.", class = "altitude_request", request_body = req$body$data)
    },
    .package = "httr2"
  )
  pressure <- data.frame(date = as.POSIXct("2020-01-15", tz = "UTC"), value = 900)
  for (formula in c("virtual", "standard")) {
    body <- tryCatch(
      geopressure_timeseries_api(46.95, 7.45, pressure, quiet = TRUE, altitude_formula = formula),
      altitude_request = \(e) e$request_body
    )
    expect_equal(body$altitudeFormula, formula)
    expect_equal(body$dataset, "single-levels")
    body <- tryCatch(
      pressurepath_create_api_impl(
        tag = NULL,
        pressurepath = data.frame(
          lon = 7.45,
          lat = 46.95,
          date = pressure$date,
          pressure_tag = pressure$value
        ),
        quiet = TRUE,
        altitude_formula = formula
      ),
      altitude_request = \(e) e$request_body
    )
    expect_equal(body$altitudeFormula, formula)
  }
  for (fun in list(
    pressurepath_create,
    pressurepath_create_api,
    pressurepath_create_arco,
    geopressure_timeseries,
    geopressure_timeseries_api,
    geopressure_timeseries_arco
  )) {
    expect_equal(eval(formals(fun)$altitude_formula)[1], "virtual")
  }
  expect_error(geopressure_timeseries_api(46, 7, pressure, altitude_formula = "invalid"), "arg")
})
