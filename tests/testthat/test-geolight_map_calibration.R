library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

# Build a tag with or without known locations for calibration.
make_tag_for_calib <- function(with_known) {
  known <- data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)

  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE)

  tag_set_map_args <- list(
    tag = tag,
    extent = c(-16, 23, 0, 50),
    scale = 4
  )
  if (with_known) {
    tag_set_map_args$known <- known
  }

  do.call(tag_set_map, tag_set_map_args) |>
    twilight_create() |>
    twilight_label_read()
}

# Shared assertions for twl_calib structure.
assert_twl_calib <- function(tag) {
  twl_calib <- tag$param$geolight_map$twl_calib
  expect_s3_class(twl_calib, "twl_calib")
  expect_type(twl_calib$x, "double")
  expect_type(twl_calib$y, "double")
  expect_true(is.numeric(twl_calib$hist_breaks))
  expect_true(is.list(twl_calib$hist_counts))
  expect_true(all(vapply(twl_calib$hist_counts, is.numeric, logical(1))))
  expect_true(is.numeric(twl_calib$hist_mids))
  expect_true(is.numeric(twl_calib$binwidth))
  expect_true(all(lengths(twl_calib$hist_counts) == length(twl_calib$hist_mids)))
  expect_true(is.data.frame(twl_calib$calib_stap))
  expect_true(all(
    c(
      "stap_id",
      "known_lat",
      "known_lon",
      "start",
      "end"
    ) %in%
      names(twl_calib$calib_stap)
  ))
  expect_true(!anyNA(twl_calib$calib_stap$known_lat))
  expect_true(!anyNA(twl_calib$calib_stap$known_lon))
}

test_that("geolight_map_calibrate() with known locations", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = Inf))
  assert_twl_calib(tag)
})

test_that("geolight_map_calibrate() with fixed locations", {
  tag <- make_tag_for_calib(with_known = FALSE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = 0))
  assert_twl_calib(tag)
})

test_that("geolight_map_calibrate() with combined known and fixed", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = 0))
  assert_twl_calib(tag)
  expect_true(1 %in% tag$param$geolight_map$twl_calib$calib_stap$stap_id)
})

test_that("geolight_map_calibrate() with no calibration locations", {
  tag <- make_tag_for_calib(with_known = FALSE)
  expect_error(
    geolight_map_calibrate(tag, fitted_location_duration = Inf),
    "There are no calibration locations"
  )
})

test_that("plot_twl_calib() and plot_twl_calib_path() return ggplot", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = Inf))
  path <- tag$stap[, c("stap_id", "start", "end")]
  path$lon <- ifelse(is.na(tag$stap$known_lon), 0, tag$stap$known_lon)
  path$lat <- ifelse(is.na(tag$stap$known_lat), 0, tag$stap$known_lat)
  p1 <- plot_twl_calib(tag, plot_plotly = FALSE)
  p2 <- plot_twl_calib(tag$param$geolight_map$twl_calib, plot_plotly = FALSE)
  p3 <- suppressWarnings(plot_twl_calib(tag, path = path, plot_plotly = FALSE))
  p4 <- suppressWarnings(plot_twl_calib_path(tag, path = path, plot_plotly = FALSE))
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_s3_class(p4, "ggplot")
})
