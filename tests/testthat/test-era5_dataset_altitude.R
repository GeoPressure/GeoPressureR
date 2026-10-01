test_that("altitude-producing functions default to single-levels", {
  # The API backend sends `dataset` explicitly in the request body, so the server-side
  # default never applies to GeoPressureR: these defaults are what users actually get.
  expect_equal(eval(formals(pressurepath_create)$era5_dataset), "single-levels")
  expect_equal(eval(formals(pressurepath_create_api)$era5_dataset), "single-levels")
  expect_equal(eval(formals(pressurepath_create_arco)$era5_dataset), "single-levels")
  expect_equal(eval(formals(geopressure_timeseries)$era5_dataset)[1], "single-levels")
  expect_equal(eval(formals(geopressure_timeseries_arco)$era5_dataset)[1], "single-levels")
})
