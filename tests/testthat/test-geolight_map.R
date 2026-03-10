library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

test_that("geolight_map() errors without map settings", {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create()

  expect_error(geolight_map(tag, quiet = TRUE))
})


test_that("geolight_map() errors without twilight", {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4,
      known = data.frame(
        stap_id = 1,
        known_lon = 17.05,
        known_lat = 48.9
      )
    )

  expect_error(geolight_map(tag, quiet = TRUE))
})


test_that("geolight_map() computes light likelihood map and calibration", {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4,
      known = data.frame(
        stap_id = 1,
        known_lon = 17.05,
        known_lat = 48.9
      )
    ) |>
    twilight_create() |>
    twilight_label_read()

  tag <- expect_no_error(geolight_map(tag, quiet = TRUE))

  expect_true("map_light" %in% names(tag))
  expect_type(tag$map_light, "list")
  expect_equal(length(tag$map_light), nrow(tag$stap))
  expect_equal(length(dim(tag$map_light[[1]])), 2)

  expect_true("geolight_map" %in% names(tag$param))
  expect_true("twl_calib" %in% names(tag$param$geolight_map))
  expect_true("twl_llp" %in% names(tag$param$geolight_map))
  expect_true("compute_known" %in% names(tag$param$geolight_map))
})


test_that("geolight_map() stores compute_known flag and works with compute_known = TRUE", {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4,
      known = data.frame(
        stap_id = 1,
        known_lon = 17.05,
        known_lat = 48.9
      )
    ) |>
    twilight_create() |>
    twilight_label_read()

  tag <- expect_no_error(
    geolight_map(tag, compute_known = TRUE, quiet = TRUE)
  )

  expect_true("geolight_map" %in% names(tag$param))
  expect_true(is.list(tag$param$geolight_map))
  expect_true(is.logical(tag$param$geolight_map$compute_known))
  expect_true(tag$param$geolight_map$compute_known)
})
