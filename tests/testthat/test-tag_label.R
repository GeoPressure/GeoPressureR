library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

tag <- tag_create(
  id = "18LX",
  crop_start = "2017-06-20",
  crop_end = "2018-05-02",
  quiet = TRUE
)

tag_classified <- tag_label_auto(tag)
test_that("tag_label_auto() | default and edge cases", {
  expect_error(tag_label_auto("not a tag"))
  expect_true(c("label") %in% names(tag_classified$acceleration))
  expect_type(tag_classified$acceleration$label, "character")
  expect_error(tag_label_auto(tag, min_duration = 0))

  tag_zero <- tag
  tag_zero$acceleration$value <- 0
  expect_warning(tag_zero <- tag_label_auto(tag_zero), "All acceleration values are zero")
  expect_true(all(tag_zero$acceleration$label == ""))

  tag_no_acc <- tag
  tag_no_acc$acceleration <- NULL
  tag_no_acc <- tag_label_auto(tag_no_acc)
  expect_true("label" %in% names(tag_no_acc$pressure))
})


test_that("tag_label_write() | default", {
  expect_error(tag_label_write("not a tag", quiet = TRUE))

  # Work under normal condition
  file_labelled <- tag_label_write(
    tag_classified,
    file = "./data/tag-label/18LX.csv",
    quiet = TRUE
  )
  csv <- utils::read.csv(file_labelled)
  expect_true(all(c("series", "timestamp", "value", "label") %in% names(csv)))
  expect_true(all(c("pressure", "acceleration") %in% csv$series))
  expect_true("flight" %in% csv$label)

  # Work even if not auto-classified
  expect_no_error({
    tag_label_write(tag, tempfile(), quiet = TRUE)
  })

  # create new folder
  # expect_no_error(tag_label_write(tag, file.path(tempdir(), "/test/test.csv")))

  # Test without pressure
  tag_tmp <- tag
  tag_tmp$acceleration <- NULL
  expect_no_error({
    tag_label_write(tag_tmp, tempfile(), quiet = TRUE)
  })

  # Test with ref
  tag_tmp <- tag
  tag_tmp$pressure$value_ref <- tag_tmp$pressure$value + 10
  expect_no_error({
    tag_label_write(tag_tmp, tempfile(), quiet = TRUE)
  })
})


tag_labelled <- tag_label_read(tag)
test_that("tag_label_read() | default", {
  # Returned value is correct
  expect_type(tag_labelled, "list")
  expect_true(c("label") %in% names(tag_labelled$pressure))
  expect_type(tag_labelled$pressure$label, "character")
  expect_true(c("label") %in% names(tag_labelled$acceleration))
  expect_type(tag_labelled$acceleration$label, "character")

  # Return error for incorrect input
  expect_error(tag_label_read(tag, file = "not a path"))
  expect_error(tag_label_read("not a tag"))

  # Test with different labelled file size
  expect_warning(
    tag_label_read(tag, file = "./data/tag-label/18LX-labeled-diffsize.csv"),
    "labelization file of pressure is missing"
  )

  # Unknown pressure label
  csv <- utils::read.csv("./data/tag-label/18LX-labeled.csv")
  csv[["label"]][which(csv[["series"]] == "pressure")[1]] <- "unknown_label"
  file_bad <- tempfile(fileext = ".csv")
  utils::write.csv(csv, file_bad, row.names = FALSE)
  expect_error(tag_label_read(tag, file = file_bad), "unknown label")

  # Unknown acceleration label
  csv <- utils::read.csv("./data/tag-label/18LX-labeled.csv")
  csv[["label"]][which(csv[["series"]] == "acceleration")[1]] <- "unknown_label"
  file_bad <- tempfile(fileext = ".csv")
  utils::write.csv(csv, file_bad, row.names = FALSE)
  expect_error(tag_label_read(tag, file = file_bad), "unknown label")
})

tag_labelled <- tag_label_stap(tag_labelled, quiet = TRUE)
test_that("tag_label_stap() | default", {
  # Returned value is correct
  expect_type(tag_labelled, "list")
  expect_true(c("stap") %in% names(tag_labelled))
  expect_type(tag_labelled$pressure$label, "character")
  expect_type(tag_labelled$acceleration$label, "character")
  expect_gt(nrow(tag_labelled$stap), 0)
})


test_that("tag_label_stap() | for elev", {
  tag_elev <- tag_label_read(
    tag,
    file = "./data/tag-label/18LX-labeled-elev.csv"
  )
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
  tag_label_stap <- tag_label_stap(tag_elev, quiet = TRUE)
  expect_true(all(c("elev_1", "elev_2") %in% unique(tag_elev$pressure$label)))
})


test_that("tag_label_read() | no acceleration", {
  expect_no_error({
    tag <- tag_create(
      id = "18LX",
      acceleration_file = NA,
      light_file = NA,
      quiet = TRUE
    )
  })
  expect_no_error({
    tag <- tag_label_read(
      tag,
      file = "./data/tag-label/18LX-labeled-no_acc.csv"
    )
  })
  expect_no_error({
    tag_label_stap(tag, quiet = TRUE)
  })
})


test_that("tag_label_stap() | wrong sensor datetime columns", {
  tag_tmp <- tag_label_read(tag, file = "./data/tag-label/18LX-labeled.csv")
  tag_tmp$magnetic <- data.frame(value = 1)
  expect_error(
    tag_label_stap(tag_tmp, quiet = TRUE),
    "needs to have a column"
  )
})


test_that("tag_label_stap() | no acceleration", {
  expect_warning(
    tag_labelled <- tag_label_read(
      tag,
      file = "./data/tag-label/18LX-labeled-no_acc.csv"
    ),
    "The labelization file does not contains label for acceleration."
  )
  expect_no_error(tag_label_stap(tag_labelled, quiet = TRUE))
})

test_that("tag_label_stap() | pressure longer than acc", {
  # Make a copy of a labeled
  tag_labelled <- tag_label_read(
    tag,
    file = "./data/tag-label/18LX-labeled.csv"
  )
  tag2 <- tag_labelled

  expect_equal(nrow(tag_label_stap(tag2, quiet = TRUE)$stap), 5)

  # Remove some acceleration data
  tag2$acceleration <- tag2$acceleration[seq(1, 3000), ]

  expect_equal(nrow(tag_label_stap(tag2, quiet = TRUE)$stap), 3)

  # Add some flight label to pressure
  tag2$pressure$label[600:650] <- "flight"

  expect_equal(nrow(tag_label_stap(tag2, quiet = TRUE)$stap), 4)

  tag2$pressure$label[50:60] <- "flight"
  expect_no_warning(tag_label_stap(tag2, quiet = TRUE))
  expect_warning(tag_label_stap(tag2, quiet = FALSE))
})


test_that("tag_label() | default", {
  tag_labelled <- expect_no_error(tag_label(tag, quiet = TRUE))
  expect_type(tag_labelled, "list")
})

test_that("tag_label() | missing file and setmap branches", {
  file_missing <- file.path(tempdir(), "gpr-tag-label", "18LX-labeled-missing.csv")
  out <- expect_no_warning(tag_label(tag, file = file_missing, quiet = TRUE))
  expect_identical(out, tag)

  dir_tmp <- tempfile("gpr-tag-label-")
  dir.create(dir_tmp, recursive = TRUE)
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  file_target <- file.path(dir_tmp, "18LX-labeled.csv")
  file_input <- file.path(dir_tmp, "18LX.csv")
  file.create(file_input)
  expect_error(
    tag_label(tag, file = file_target, quiet = TRUE),
    "does not exist but"
  )

  tag_sm <- tag_label(tag, quiet = TRUE)
  tag_sm <- tag_set_map(tag_sm, extent = c(-16, 23, 0, 50))
  out <- tag_label(tag_sm, quiet = TRUE)
  expect_identical(out, tag_sm)
})

test_that("tag_label() | deprecated arguments", {
  expect_warning(
    tag_label(tag, quiet = TRUE, warning_flight_duration = TRUE),
    "deprecated"
  )
  expect_warning(
    tag_label(tag, quiet = TRUE, warning_stap_duration = TRUE),
    "deprecated"
  )
  expect_warning(
    tag_label(tag, quiet = TRUE, foo = "bar"),
    "Additional arguments are ignored"
  )
})

test_that("tag_label_read() and tag_label_stap() | stop after tag_set_map", {
  tag_sm <- tag_label(tag, quiet = TRUE)
  tag_sm <- tag_set_map(tag_sm, extent = c(-16, 23, 0, 50))
  expect_error(tag_label_read(tag_sm), "tag_set_map")
  expect_error(tag_label_stap(tag_sm, quiet = TRUE), "tag_set_map")
})
