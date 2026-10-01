test_that("altitude-producing functions default to single-levels", {
  # The API backend sends `dataset` explicitly in the request body, so the server-side
  # default never applies to GeoPressureR: these defaults are what users actually get.
  expect_equal(eval(formals(pressurepath_create)$era5_dataset), "single-levels")
  expect_equal(eval(formals(pressurepath_create_api)$era5_dataset), "single-levels")
  expect_equal(eval(formals(pressurepath_create_arco)$era5_dataset), "single-levels")
  expect_equal(eval(formals(geopressure_timeseries)$era5_dataset)[1], "single-levels")
  expect_equal(eval(formals(geopressure_timeseries_arco)$era5_dataset)[1], "single-levels")
})

# Stop at the first request to GeoPressureAPI and return the dataset it would have asked for.
map_request_dataset <- function(...) {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      rlang::abort(class = "map_request", dataset = req$body$data$dataset)
    },
    .package = "httr2"
  )
  tag <- withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
    tag_create("18LX", quiet = TRUE) |>
      tag_label(quiet = TRUE) |>
      tag_set_map(extent = c(-16, 23, 0, 50), scale = 1)
  })
  tryCatch(geopressure_map(tag, quiet = TRUE, ...), map_request = \(e) e$dataset)
}

test_that("geopressure_map requests ERA5 single levels by default", {
  expect_equal(expect_no_warning(map_request_dataset()), "single-levels")
})

test_that("geopressure_map honours an explicit era5_dataset without warning", {
  expect_equal(expect_no_warning(map_request_dataset(era5_dataset = "land")), "land")
})

test_that("all three era5_dataset values remain accepted", {
  for (dataset in c("single-levels", "land", "both")) {
    expect_equal(
      match.arg(dataset, c("single-levels", "land", "both")),
      dataset
    )
  }
})
