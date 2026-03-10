library(testthat)
library(GeoPressureR)

test_that("plot_path() returns ggplot and respects point_fill logic", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  path <- data.frame(
    stap_id = 1:3,
    lon = c(10, 11, 12),
    lat = c(50, 51, 52),
    start = start + c(0, 1, 2) * 86400,
    end = start + c(1, 2, 3) * 86400,
    known_lon = c(10, NA, NA),
    known_lat = c(50, NA, NA),
    zenith = c(NA, 96, NA)
  )

  p <- expect_no_error(plot_path(path, plot_leaflet = FALSE))
  expect_s3_class(p, "ggplot")

  point_layer <- p$layers[[which(vapply(
    p$layers,
    function(x) inherits(x$geom, "GeomPoint"),
    logical(1)
  ))]]
  point_fill <- point_layer$data$point_fill
  expect_equal(point_fill[1], "#D1495B")
  expect_equal(point_fill[2], "#F2C14E")
  expect_equal(point_fill[3], "black")

  lmap <- expect_no_error(plot_path(path, plot_leaflet = TRUE))
  expect_s3_class(lmap, "leaflet")
})

test_that("plot_path() handles missing j and missing coordinates", {
  path <- data.frame(
    stap_id = 1:4,
    lon = c(10, NA, 12, 14),
    lat = c(50, NA, 52, 54)
  )

  p <- expect_no_error(plot_path(path, plot_leaflet = FALSE))
  expect_s3_class(p, "ggplot")

  point_layer <- p$layers[[which(vapply(
    p$layers,
    function(x) inherits(x$geom, "GeomPoint"),
    logical(1)
  ))]]
  expect_equal(nrow(point_layer$data), 3)

  lmap <- expect_no_error(plot_path(path, plot_leaflet = TRUE))
  expect_s3_class(lmap, "leaflet")
})

test_that("plot_path() handles missing stap_id", {
  path <- data.frame(
    lon = c(10, 11, 12),
    lat = c(50, 51, 52),
    zenith = c(NA, 96, NA)
  )

  p <- expect_no_error(plot_path(path, plot_leaflet = FALSE))
  expect_s3_class(p, "ggplot")

  skip("Leaflet path requires stap_id for grouping.")
})
