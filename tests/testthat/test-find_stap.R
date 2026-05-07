library(testthat)
library(GeoPressureR)

test_that("find_stap() returns row-based indices when stap_id is absent", {
  stap <- data.frame(
    start = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-05 00:00:00", "2020-01-10 00:00:00"),
      tz = "UTC"
    ),
    end = as.POSIXct(
      c("2020-01-03 00:00:00", "2020-01-07 00:00:00", "2020-01-12 00:00:00"),
      tz = "UTC"
    )
  )
  date <- as.POSIXct(
    c(
      "2020-01-02 00:00:00",
      "2020-01-04 00:00:00",
      "2020-01-06 00:00:00",
      "2020-01-08 12:00:00",
      "2020-01-11 00:00:00"
    ),
    tz = "UTC"
  )

  expect_equal(find_stap(stap, date), c(1, 1.5, 2, 2.5, 3))
})

test_that("find_stap() respects regular stap_id values", {
  stap <- data.frame(
    stap_id = c(1, 2, 3),
    start = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-05 00:00:00", "2020-01-10 00:00:00"),
      tz = "UTC"
    ),
    end = as.POSIXct(
      c("2020-01-03 00:00:00", "2020-01-07 00:00:00", "2020-01-12 00:00:00"),
      tz = "UTC"
    )
  )
  date <- as.POSIXct(
    c(
      "2020-01-02 00:00:00",
      "2020-01-04 00:00:00",
      "2020-01-06 00:00:00",
      "2020-01-08 12:00:00",
      "2020-01-11 00:00:00"
    ),
    tz = "UTC"
  )

  expect_equal(find_stap(stap, date), c(1, 1.5, 2, 2.5, 3))
})

test_that("find_stap() respects irregular stap_id values and decimal flight ids", {
  stap <- data.frame(
    stap_id = c(2, 4, 7),
    start = as.POSIXct(
      c("2020-01-01 00:00:00", "2020-01-05 00:00:00", "2020-01-10 00:00:00"),
      tz = "UTC"
    ),
    end = as.POSIXct(
      c("2020-01-03 00:00:00", "2020-01-07 00:00:00", "2020-01-12 00:00:00"),
      tz = "UTC"
    )
  )
  date <- as.POSIXct(
    c(
      "2020-01-02 00:00:00",
      "2020-01-04 00:00:00",
      "2020-01-06 00:00:00",
      "2020-01-08 12:00:00",
      "2020-01-11 00:00:00"
    ),
    tz = "UTC"
  )

  expect_equal(find_stap(stap, date), c(2, 3, 4, 5.5, 7))
})
