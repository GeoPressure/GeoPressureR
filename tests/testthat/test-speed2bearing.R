library(testthat)
library(GeoPressureR)

test_that("speed2bearing()", {
  speed <- c(1, 0, -1, 0) + c(0, 1, 0, -1) * 1i # E, N, W, S
  expect_equal(speed2bearing(speed), c(90, 0, 270, 180))
  expect_equal(speed2bearing(speed, speed_ref = 1), c(0, 270, 180, 90))
})
