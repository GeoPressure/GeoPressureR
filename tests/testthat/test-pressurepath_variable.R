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
  # ARCO: 20 single-levels variables, 16 ERA5-Land, plus altitude
  expect_length(pressurepath_variable_available("arco", "single-levels"), 21)
  expect_length(pressurepath_variable_available("arco", "land"), 17)
  # "both" reads land over land and single levels over water within one request, so only
  # variables carried by both products are safe
  expect_length(pressurepath_variable_available("arco", "both"), 9)
  expect_true(all(
    pressurepath_variable_available("arco", "both") %in%
      intersect(
        pressurepath_variable_available("arco", "land"),
        pressurepath_variable_available("arco", "single-levels")
      )
  ))
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
    pressurepath_variable_check(c("altitude", "boundary_layer_height"), "arco", "land")
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

test_that("ARCO uses the same variable vocabulary as the API", {
  # every ARCO variable must be a name the API would also accept, so `variable` reads the same
  # whichever backend is used
  for (d in c("single-levels", "land", "both")) {
    expect_true(all(
      pressurepath_variable_available("arco", d) %in%
        pressurepath_variable_available("api", d)
    ))
  }
})

test_that("ARCO store map resolves to the documented zarr arrays", {
  f <- era5_arco_array
  expect_match(
    as.character(f("u_component_of_wind_10m", "land")),
    "cadl-arco-geo-008/arco/reanalysis_era5_land/sfc-wind/geoChunked.zarr/u10$"
  )
  expect_match(
    as.character(f("boundary_layer_height", "single-levels")),
    "cadl-arco-geo-002/arco/reanalysis_era5_single_levels/sfc/geoChunked.zarr/blh$"
  )
  expect_match(
    as.character(f("volumetric_soil_water_layer_3", "land")),
    "sfc-soil-water/geoChunked.zarr/swvl3$"
  )
  # a variable ERA5-Land does not carry must fail loudly rather than build a 404 URL
  expect_error(f("boundary_layer_height", "land"), "not available from ARCO")
})

test_that("the store map has no duplicate or unmapped entries", {
  tbl <- era5_arco_store_table()
  expect_equal(anyDuplicated(tbl[c("variable", "era5_dataset")]), 0L)
  expect_equal(sum(tbl$era5_dataset == "single-levels"), 20)
  expect_equal(sum(tbl$era5_dataset == "land"), 16)
  expect_true(all(nzchar(tbl$short_name)))
})
