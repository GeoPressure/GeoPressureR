library(testthat)
library(GeoPressureR)

test_that("windsupport()", {
  expect_equal(windsupport(1i, 1i), 1)
  expect_equal(windsupport(1i, -1i), -1)
  expect_equal(windsupport(1i, 1 + 1i), sqrt(2) / 2)
})
