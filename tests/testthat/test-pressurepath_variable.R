# The variables available to pressurepath_create() depend on both the backend and the ERA5
# product. Requesting one the product does not carry made GeoPressureAPI return 200 with every
# array empty -- blanking surface_pressure and time too -- so the request must be validated
# before it is sent.

test_that("each configuration exposes the right number of variables", {
  # 292 ERA5 bands + altitude; "both" returns ERA5's band namespace so it matches single-levels
  expect_length(pressurepath_variable_available("api", "single-levels"), 293)
  expect_length(pressurepath_variable_available("api", "both"), 293)
  # 69 ERA5-Land bands + geopotential + altitude
  expect_length(pressurepath_variable_available("api", "land"), 71)
  # ARCO maps only surface pressure and 2 m temperature
  expect_equal(
    pressurepath_variable_available("arco", "land"),
    c("altitude", "surface_pressure")
  )
})

test_that("altitude is available everywhere", {
  for (s in c("api", "arco")) {
    for (d in c("single-levels", "land", "both")) {
      expect_true("altitude" %in% pressurepath_variable_available(s, d))
      expect_true("surface_pressure" %in% pressurepath_variable_available(s, d))
    }
  }
})

test_that("the ERA5 and ERA5-Land sets really differ", {
  sl <- pressurepath_variable_available("api", "single-levels")
  land <- pressurepath_variable_available("api", "land")
  # variables that exist only in one product, in both directions
  expect_true("boundary_layer_height" %in% sl && !("boundary_layer_height" %in% land))
  expect_true("snow_cover" %in% land && !("snow_cover" %in% sl))
  expect_length(intersect(sl, land), 45) # 44 shared bands + altitude
})

test_that("valid requests pass silently", {
  expect_silent(pressurepath_variable_check(c("altitude", "surface_pressure"), "arco", "land"))
  expect_silent(pressurepath_variable_check(c("altitude", "snow_cover"), "api", "land"))
  expect_silent(pressurepath_variable_check("boundary_layer_height", "api", "both"))
  expect_silent(pressurepath_variable_check(character(0), "api", "land"))
})

test_that("an unavailable variable names the configuration that would work", {
  expect_snapshot(
    error = TRUE,
    pressurepath_variable_check(c("surface_pressure", "boundary_layer_height"), "api", "land")
  )
  expect_snapshot(
    error = TRUE,
    pressurepath_variable_check("snow_cover", "api", "single-levels")
  )
  expect_snapshot(
    error = TRUE,
    pressurepath_variable_check(c("altitude", "total_precipitation"), "arco", "land")
  )
})

test_that("a typo suggests the intended variable", {
  expect_snapshot(
    error = TRUE,
    pressurepath_variable_check("boundary_layer_heigt", "api", "single-levels")
  )
  expect_snapshot(
    error = TRUE,
    pressurepath_variable_check("banana", "api", "single-levels")
  )
})

test_that("pressurepath_variable_available rejects unknown arguments", {
  expect_error(pressurepath_variable_available("gee", "land"))
  expect_error(pressurepath_variable_available("api", "reanalysis"))
})
