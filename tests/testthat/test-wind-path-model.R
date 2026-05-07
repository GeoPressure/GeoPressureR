library(testthat)
library(GeoPressureR)

test_that("edge_add_wind_correct_path() keeps linear progress without wind", {
  fl_s <- data.frame(
    start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("2020-01-01 02:00:00", tz = "UTC"),
    duration = 2
  )
  t_q <- seq(fl_s$start, fl_s$end, by = 60 * 60)
  lat_s <- 0
  lon_s <- 0
  lat_e <- 0
  lon_e <- 2
  u <- matrix(0, nrow = length(t_q), ncol = 1)
  v <- matrix(0, nrow = length(t_q), ncol = 1)

  linear <- GeoPressureR:::edge_add_wind_interp_path(fl_s, 1, lat_s, lon_s, lat_e, lon_e, 60)
  corrected <- GeoPressureR:::edge_add_wind_correct_path(
    fl_s, 1, t_q, lat_s, lon_s, lat_e, lon_e, u, v
  )

  expect_equal(corrected$lat_int, linear$lat_int, tolerance = 1e-10)
  expect_equal(corrected$lon_int, linear$lon_int, tolerance = 1e-10)
})

test_that("edge_add_wind_correct_path() keeps linear progress with constant wind", {
  fl_s <- data.frame(
    start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("2020-01-01 02:00:00", tz = "UTC"),
    duration = 2
  )
  t_q <- seq(fl_s$start, fl_s$end, by = 60 * 60)
  lat_s <- 0
  lon_s <- 0
  lat_e <- 0
  lon_e <- 2
  u <- matrix(20 / 3.6, nrow = length(t_q), ncol = 1)
  v <- matrix(0, nrow = length(t_q), ncol = 1)

  linear <- GeoPressureR:::edge_add_wind_interp_path(fl_s, 1, lat_s, lon_s, lat_e, lon_e, 60)
  corrected <- GeoPressureR:::edge_add_wind_correct_path(
    fl_s, 1, t_q, lat_s, lon_s, lat_e, lon_e, u, v
  )

  expect_equal(corrected$lat_int, linear$lat_int, tolerance = 1e-10)
  expect_equal(corrected$lon_int, linear$lon_int, tolerance = 1e-10)
})

test_that("edge_add_wind_correct_path() shifts progress with varying wind", {
  fl_s <- data.frame(
    start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("2020-01-01 02:00:00", tz = "UTC"),
    duration = 2
  )
  t_q <- seq(fl_s$start, fl_s$end, by = 60 * 60)
  lat_s <- 0
  lon_s <- 0
  lat_e <- 0
  lon_e <- 2
  u <- matrix(c(20, 20, 0) / 3.6, ncol = 1)
  v <- matrix(0, nrow = length(t_q), ncol = 1)

  linear <- GeoPressureR:::edge_add_wind_interp_path(fl_s, 1, lat_s, lon_s, lat_e, lon_e, 60)
  corrected <- GeoPressureR:::edge_add_wind_correct_path(
    fl_s, 1, t_q, lat_s, lon_s, lat_e, lon_e, u, v
  )

  expect_gt(corrected$lon_int[1, 2], linear$lon_int[1, 2])
  expect_equal(corrected$lon_int[1, 3], lon_e, tolerance = 1e-10)
})

test_that("edge_add_wind_correct_path() accepts precomputed along-track wind", {
  fl_s <- data.frame(
    start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("2020-01-01 02:00:00", tz = "UTC"),
    duration = 2
  )
  t_q <- seq(fl_s$start, fl_s$end, by = 60 * 60)
  u <- matrix(c(20, 20, 0) / 3.6, ncol = 1)
  v <- matrix(0, nrow = length(t_q), ncol = 1)

  corrected_uv <- GeoPressureR:::edge_add_wind_correct_path(
    fl_s, 1, t_q, 0, 0, 0, 2, u, v
  )
  corrected_along <- GeoPressureR:::edge_add_wind_correct_path(
    fl_s, 1, t_q, 0, 0, 0, 2, wind_along = u * 3.6
  )

  expect_equal(corrected_along, corrected_uv, tolerance = 1e-10)
})

test_that("edge_add_wind_correct_path() keeps zero-distance paths stable", {
  fl_s <- data.frame(
    start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    end = as.POSIXct("2020-01-01 02:00:00", tz = "UTC"),
    duration = 2
  )
  t_q <- seq(fl_s$start, fl_s$end, by = 60 * 60)
  u <- matrix(c(20, 0, -20) / 3.6, ncol = 1)
  v <- matrix(c(0, 20, 0) / 3.6, ncol = 1)

  corrected <- GeoPressureR:::edge_add_wind_correct_path(
    fl_s, 1, t_q, 10, 20, 10, 20, u, v
  )

  expect_equal(corrected$lat_int, matrix(10, nrow = 1, ncol = length(t_q)))
  expect_equal(corrected$lon_int, matrix(20, nrow = 1, ncol = length(t_q)))
})

test_that("edge_add_wind_stap_include() maps graph layers to original stap ids", {
  stap <- data.frame(
    stap_id = 1:5,
    start = as.POSIXct("2020-01-01", tz = "UTC") + 1:5 * 86400,
    end = as.POSIXct("2020-01-01", tz = "UTC") + 1:5 * 86400 + 3600,
    include = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  stap_include <- GeoPressureR:::edge_add_wind_stap_include(stap)
  flight <- stap2flight(stap, format = "list")

  expect_equal(stap_include, c(1, 3, 5))
  expect_equal(as.character(stap_include[2]), names(flight)[2])
})
