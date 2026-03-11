library(testthat)
library(GeoPressureR)

test_with_extdata()

test_that("tag_assert() | invalid input and type handling", {
  expect_false(tag_assert("not_tag", type = "logical"))
  expect_error(tag_assert("not_tag"), "not a")

  tag_min <- structure(list(param = list()), class = "tag")
  expect_error(tag_assert(tag_min, condition = "unknown_condition"), "unknown")

  expect_false(tag_assert(tag_min, condition = "read", type = ""))
})

test_that("tag_assert() | workflow status transitions", {
  tag <- tag_create("18LX", quiet = TRUE)
  expect_true(tag_assert(tag, "tag", "logical"))
  expect_true(tag_assert(tag, "read", "logical"))
  expect_true(tag_assert(tag, "pressure", "logical"))
  expect_false(tag_assert(tag, "label", "logical"))
  expect_warning(tag_assert(tag, "setmap", "warn"), "tag_set_map")

  tag <- tag_label(tag, quiet = TRUE)
  expect_true(tag_assert(tag, "label", "logical"))
  expect_true(tag_assert(tag, "stap", "logical"))

  tag <- tag_set_map(tag, extent = c(-16, 23, 0, 50), scale = 1)
  expect_true(tag_assert(tag, "setmap", "logical"))

  tag <- twilight_create(tag)
  expect_true(tag_assert(tag, "twilight", "logical"))
})

test_that("tag_assert() | map-related messaging types", {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(extent = c(-16, 23, 0, 50), scale = 1)

  expect_false(tag_assert(tag, "map_pressure", "logical"))
  expect_warning(tag_assert(tag, "map_pressure", "warn"), "pressure likelihood map")
  expect_error(tag_assert(tag, "map_pressure", "abort"), "pressure likelihood map")
  expect_no_error(invisible(capture.output(
    tag_assert(tag, "map_pressure", "inform"),
    type = "message"
  )))
})
