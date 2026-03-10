library(testthat)
library(GeoPressureR)

test_that("read_stap() | data.frame and file path", {
  stap_df <- data.frame(
    start = c("2020-01-03 00:00:00", "2020-01-01 00:00:00"),
    end = c("2020-01-04 00:00:00", "2020-01-02 00:00:00"),
    stringsAsFactors = FALSE
  )

  out <- read_stap(stap_df)
  expect_s3_class(out$start, "POSIXct")
  expect_s3_class(out$end, "POSIXct")
  expect_true(all(diff(out$start) >= 0))

  file <- tempfile(fileext = ".csv")
  utils::write.csv(stap_df, file, row.names = FALSE)
  out <- read_stap(file)
  expect_equal(nrow(out), 2)
})

test_that("read_stap() | tag input and overlap error", {
  withr::local_dir(tempdir())
  dir.create("./data/stap-label", recursive = TRUE, showWarnings = FALSE)

  utils::write.csv(
    data.frame(
      start = c("2020-01-01 00:00:00", "2020-01-03 00:00:00"),
      end = c("2020-01-02 00:00:00", "2020-01-04 00:00:00"),
      stringsAsFactors = FALSE
    ),
    "./data/stap-label/test.csv",
    row.names = FALSE
  )
  tag <- structure(list(param = list(id = "test")), class = "tag")
  out <- read_stap(tag)
  expect_equal(nrow(out), 2)

  expect_error(
    read_stap(data.frame(
      start = c("2020-01-01 00:00:00", "2020-01-01 12:00:00"),
      end = c("2020-01-02 00:00:00", "2020-01-02 00:00:00"),
      stringsAsFactors = FALSE
    )),
    "overlap"
  )
})
