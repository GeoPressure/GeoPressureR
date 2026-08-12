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
      "calib_type",
      "start",
      "end"
    ) %in%
      names(twl_calib$calib_stap)
  ))
  expect_true(!anyNA(twl_calib$calib_stap$known_lat))
  expect_true(!anyNA(twl_calib$calib_stap$known_lon))
  expect_true(all(twl_calib$calib_stap$calib_type %in% c("known", "fitted")))
}

test_that("geolight_map_calibrate() with known locations", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = Inf, quiet = TRUE))
  assert_twl_calib(tag)
})

test_that("geolight_map_calibrate() with fitted locations", {
  tag <- make_tag_for_calib(with_known = FALSE)
  tag <- expect_no_error(geolight_map_calibrate(
    tag,
    fitted_location_duration = 0,
    refine_fitted_location_max_iter = 0,
    quiet = TRUE
  ))
  assert_twl_calib(tag)
  expect_equal(tag$param$geolight_map$refine_fitted_location_max_iter, 0)
})

test_that("geolight_map_calibrate() stores fitted-location prior parameters", {
  tag <- make_tag_for_calib(with_known = FALSE)
  tag <- geolight_map_calibrate(
    tag,
    fitted_location_duration = 0,
    zenith_prior_mean = 93,
    zenith_prior_sd = 1.3,
    zenith_prior_penalty_weight = 1e-2,
    quiet = TRUE
  )

  expect_equal(tag$param$geolight_map$zenith_prior_mean, 93)
  expect_equal(tag$param$geolight_map$zenith_prior_sd, 1.3)
  expect_equal(tag$param$geolight_map$zenith_prior_penalty_weight, 1e-2)
})

test_that("geolight refinement uses local target-resolution grids", {
  extent <- c(-16, 23, 0, 50)
  extent_local <- geolight_refine_extent(
    lon = 17,
    lat = 49,
    radius_lat_km = 200,
    radius_lon_km = 100,
    extent = extent,
    scale = geolight_refine_scale(extent, 20)
  )

  expect_equal(geolight_refine_scale(extent, 20), 8)
  expect_equal(geolight_refine_scale(extent, 40), 4)
  expect_lt(prod(c(extent_local[4] - extent_local[3], extent_local[2] - extent_local[1])), 39 * 50)
  expect_no_error(map_expand(extent_local, geolight_refine_scale(extent, 20)))

  extent_scale_3 <- c(0, 1 / 3, 0, 1 / 3)
  expect_equal(geolight_refine_scale(extent_scale_3, 20, map_scale = 3), 3)
  expect_no_error(map_expand(extent_scale_3, geolight_refine_scale(extent_scale_3, 20, 3)))
})

test_that("geolight_map_calibrate() refines fitted locations when requested", {
  tag <- make_tag_for_calib(with_known = FALSE)
  tag <- expect_no_error(geolight_map_calibrate(
    tag,
    fitted_location_duration = 0,
    refine_fitted_location_scale_km = 20,
    refine_fitted_location_max_iter = 2,
    quiet = TRUE
  ))

  assert_twl_calib(tag)
  expect_equal(tag$param$geolight_map$refine_fitted_location_scale_km, 20)
  expect_equal(tag$param$geolight_map$refine_fitted_location_max_iter, 2)

  calib_stap <- tag$param$geolight_map$twl_calib$calib_stap
  expect_true(all(is.finite(calib_stap$known_lon)))
  expect_true(all(is.finite(calib_stap$known_lat)))
  expect_true(all(is.na(calib_stap$zenith)))
  expect_true(all(calib_stap$calib_type == "fitted"))
  expect_equal(calib_stap$stap_id, tag$stap$stap_id)
})

test_that("geolight_map_calibrate() keeps known locations fixed during refinement", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(
    tag,
    fitted_location_duration = Inf,
    refine_fitted_location_max_iter = 2,
    quiet = TRUE
  ))

  calib_stap <- tag$param$geolight_map$twl_calib$calib_stap
  expect_equal(calib_stap$known_lon[calib_stap$stap_id == 1], 17.05)
  expect_equal(calib_stap$known_lat[calib_stap$stap_id == 1], 48.9)
  expect_equal(calib_stap$calib_type[calib_stap$stap_id == 1], "known")
  expect_equal(tag$param$geolight_map$refine_fitted_location_max_iter, 2)
})

test_that("geolight_map_calibrate() with combined known and fitted locations", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = 0, quiet = TRUE))
  assert_twl_calib(tag)
  expect_true(1 %in% tag$param$geolight_map$twl_calib$calib_stap$stap_id)
})

test_that("geolight_map_calibrate() with no calibration locations", {
  tag <- make_tag_for_calib(with_known = FALSE)
  expect_error(
    geolight_map_calibrate(tag, fitted_location_duration = Inf, quiet = TRUE),
    "There are no calibration locations"
  )
})

test_that("plot_twl_calib() and plot_twl_calib_path() return ggplot", {
  tag <- make_tag_for_calib(with_known = TRUE)
  tag <- expect_no_error(geolight_map_calibrate(tag, fitted_location_duration = Inf, quiet = TRUE))
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
