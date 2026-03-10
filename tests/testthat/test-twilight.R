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

  file_missing_dir <- file.path(tempdir(), "gpr-twilight", basename(tempfile()))
  expect_false(dir.exists(dirname(file_missing_dir)))
  expect_no_error(twilight_label_write(tag, file = file_missing_dir))
  expect_true(dir.exists(dirname(file_missing_dir)))

  # Works when twilight has no stap_id and no label
  tag_tmp <- tag
  tag_tmp$twilight$stap_id <- NULL
  tag_tmp$twilight$label <- NULL
  expect_no_error(twilight_label_write(tag_tmp, file = tempfile(), quiet = TRUE))

  # Replace NA labels even when stap_id is missing
  tag_tmp <- tag
  tag_tmp$twilight$stap_id <- NULL
  tag_tmp$twilight$label <- NA_character_
  expect_warning(
    twilight_label_write(tag_tmp, file = tempfile(), quiet = TRUE),
    "Some twilight label contain NA value"
  )

  # Return FALSE when user does not allow directory creation
  withr::local_options(list(askYesNo = function(...) FALSE))
  file_missing_dir <- file.path(tempdir(), "gpr-twilight-no", basename(tempfile()))
  expect_false(dir.exists(dirname(file_missing_dir)))
  expect_identical(twilight_label_write(tag, file = file_missing_dir, quiet = TRUE), FALSE)
  expect_false(dir.exists(dirname(file_missing_dir)))

  tag <- twilight_label_read(tag)
  expect_true("label" %in% names(tag$twilight))
})
