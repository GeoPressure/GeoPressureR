library(testthat)
library(GeoPressureR)

test_that("load_interim() | var and var_optional", {
  test_with_extdata()

  # Explicit file path + required/optional selection
  e <- new.env(parent = emptyenv())
  out <- load_interim(
    "./data/interim/18LX.RData",
    var = "tag",
    var_optional = c("marginal", "not_present"),
    envir = e
  )
  expect_type(out, "list")
  expect_true(all(c("tag", "marginal") %in% names(out)))
  expect_false("not_present" %in% names(out))
  expect_true(all(c("tag", "marginal") %in% ls(e)))

  # Optional-only selection returns the object when only one exists
  e <- new.env(parent = emptyenv())
  out <- load_interim(
    "18LX",
    var_optional = c("path_most_likely", "not_present"),
    envir = e
  )
  expect_type(out, "list")
  expect_true("path_most_likely" %in% ls(e))
  expect_false("not_present" %in% ls(e))

  # Missing required object should error
  expect_error(
    load_interim("18LX", var = "not_present", envir = new.env(parent = emptyenv())),
    "does not contain required object"
  )
})
