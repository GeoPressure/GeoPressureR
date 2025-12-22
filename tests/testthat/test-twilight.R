library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

test_that("twilight_create() runs and outputs expected structure", {
  tag <- tag_create("18LX", quiet = TRUE)
  expect_no_error(plot_tag_twilight(tag))
  tag <- twilight_create(tag)

  expect_true(all(c("twilight", "rise") %in% names(tag$twilight)))
  expect_true(nrow(tag$twilight) > 0)

  tag_off <- expect_no_error(
    twilight_create(tag, twl_offset = 2)
  )
  expect_equal(tag_off$param$twilight_create$twl_offset, 2)
  expect_no_error(plot_tag_twilight(tag_off))

  tag_off <- NULL
  expect_warning(tag_off <- twilight_create(tag, twl_offset = 3.5))
  expect_no_error(plot_tag_twilight(tag_off))
})

test_that("twilight_label_write() and twilight_label_read() work", {
  withr::local_options(list(askYesNo = function(...) TRUE))

  tag <- tag_create("18LX", quiet = TRUE)
  tag <- twilight_create(tag)

  expect_no_error(twilight_label_write(tag, quiet = TRUE))

  # Also work if stap_id exist
  tag <- tag_label(tag, quiet = TRUE)
  expect_no_error(twilight_label_write(tag, quiet = TRUE))

  expect_no_error(
    twilight_label_write(tag, file = "not_folder/not_existing_file")
  )

  tag <- twilight_label_read(tag)
  expect_true("label" %in% names(tag$twilight))
})
