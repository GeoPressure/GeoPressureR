library(testthat)
library(GeoPressureR)

test_that("pressure query cache retains chronology and filters by timestamps", {
  cache_dir <- withr::local_tempdir()
  withr::local_options(GeoPressureR.pressure_query_cache_dir = cache_dir)
  date <- as.POSIXct("2020-01-01", tz = "UTC") + 0:2 * 3600

  make_bundle <- function(requested_at, lat) {
    list(
      tag_id = "bird-1",
      stap_id = 1,
      date = date,
      pressure_tag = c(1000, 1001, 1002),
      surface_pressure = c(999, 1000, 1001),
      requested_lat = lat,
      requested_lon = 7,
      returned_lat = lat,
      returned_lon = 7,
      requested_at = as.POSIXct(requested_at, tz = "UTC"),
      completed_at = as.POSIXct(requested_at, tz = "UTC") + 5
    )
  }

  GeoPressureR:::pressure_query_cache_write(make_bundle("2020-01-01 02:00:00", 47))
  GeoPressureR:::pressure_query_cache_write(make_bundle("2020-01-01 01:00:00", 46))

  bundles <- GeoPressureR:::pressure_query_cache_read("bird-1", date)
  expect_length(bundles, 2)
  expect_equal(vapply(bundles, `[[`, numeric(1), "requested_lat"), c(46, 47))
  expect_length(
    GeoPressureR:::pressure_query_cache_read("bird-1", date + 60),
    0
  )
})
