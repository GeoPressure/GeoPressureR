# ERA5-Land surface pressure is not hydrostatically consistent with the orography ERA5-Land
# publishes, so altitude retrieved from it is wrong by tens to hundreds of metres. The
# deprecation must fire for that combination only -- ERA5-Land stays legitimate for every
# other variable and for geopressure_map().

test_that("altitude with ERA5-Land is deprecated", {
  for (dataset in c("land", "both")) {
    expect_snapshot(
      era5_dataset_deprecate_altitude(dataset, TRUE),
      variant = dataset
    )
  }
})

test_that("ERA5-Land stays silent when altitude is not requested", {
  expect_no_warning(expect_false(era5_dataset_deprecate_altitude("land", FALSE)))
  expect_no_warning(expect_false(era5_dataset_deprecate_altitude("both", FALSE)))
})

test_that("single-levels never warns", {
  expect_no_warning(expect_false(era5_dataset_deprecate_altitude("single-levels", TRUE)))
  expect_no_warning(expect_false(era5_dataset_deprecate_altitude("single-levels", FALSE)))
})

test_that("altitude-producing functions default to single-levels", {
  # The API backend sends `dataset` explicitly in the request body, so the server-side
  # default never applies to GeoPressureR: these defaults are what users actually get.
  expect_equal(eval(formals(pressurepath_create)$era5_dataset), "single-levels")
  expect_equal(eval(formals(pressurepath_create_api)$era5_dataset), "single-levels")
  expect_equal(eval(formals(pressurepath_create_arco)$era5_dataset), "single-levels")
  expect_equal(eval(formals(geopressure_timeseries)$era5_dataset)[1], "single-levels")
  expect_equal(eval(formals(geopressure_timeseries_arco)$era5_dataset)[1], "single-levels")
})

test_that("geopressure_map keeps ERA5-Land", {
  # geopressure_map's mismatch is differential (the mean error is removed), so the static
  # ERA5-Land bias cancels and its 0.1 degree resolution is the whole point.
  expect_equal(eval(formals(geopressure_map)$era5_dataset), "land")
})

test_that("all three era5_dataset values remain accepted", {
  for (dataset in c("single-levels", "land", "both")) {
    expect_equal(
      match.arg(dataset, c("single-levels", "land", "both")),
      dataset
    )
  }
})
