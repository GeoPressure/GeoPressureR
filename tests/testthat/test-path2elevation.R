library(testthat)
library(GeoPressureR)

skip_if_test_level_below("online")

# Small synthetic case
path <- data.frame(
  lon = c(8.47, 9.41, 9.01, -0.91, 14.24, 27.30, 34.39, 30.00),
  lat = c(48.89, 44.78, 40.07, 37.68, 17.33, 7.32, 8.09, -23.13),
  start = as.POSIXct(
    c(
      "2017-05-01 00:42",
      "2017-05-03 01:22",
      "2017-05-03 22:47",
      "2017-05-06 03:32",
      "2017-05-07 01:12",
      "2017-05-07 22:32",
      "2017-05-09 21:52",
      "2017-05-10 21:12"
    ),
    tz = "UTC"
  ),
  end = as.POSIXct(
    c(
      "2017-05-02 22:12",
      "2017-05-03 20:12",
      "2017-05-06 02:47",
      "2017-05-06 23:22",
      "2017-05-07 17:42",
      "2017-05-09 20:27",
      "2017-05-10 19:57",
      "2017-05-11 21:17"
    ),
    tz = "UTC"
  ),
  stap_id = seq(1, 8)
)

test_that("path2elevation() | default output", {
  elevation <- path2elevation(path)
  expect_s3_class(elevation, "data.frame")
  expect_true(all(
    c("X10", "X50", "X90", "distance", "lat", "lon", "stap_id") %in%
      names(elevation)
  ))
})

test_that("path2elevation() | minimum, maximum and scale selection", {
  elevation <- path2elevation(path, scale = 3, percentile = c(0, 50, 100))
  expect_named(elevation, c("X0", "X50", "X100", "distance", "lat", "lon", "stap_id"))
  expect_true(all(elevation$X0 <= elevation$X50 & elevation$X50 <= elevation$X100))
  # `scale = 3` reads the 0.25° grid: the first sample is the centre of its cell
  expect_equal(c(elevation$lon[1], elevation$lat[1]), c(8.375, 48.875))
  expect_equal(range(elevation$stap_id), c(1, 8))
})

# test_that("path2elevation() | change default", {
#   elevation <- path2elevation(path, scale = 1)
#   elevation <- path2elevation(path, sampling_scale = 1)
# })
