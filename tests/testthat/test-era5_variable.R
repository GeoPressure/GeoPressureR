# CDS takes long names but writes the NetCDF under GRIB short names, so tag_download_wind() and
# edge_add_wind() used to speak different vocabularies for the same field with nothing connecting
# them. Both now take the long names and the registry does the translation.

test_that("the registry covers the ERA5 pressure-level set", {
  tbl <- era5_variable_pressure_level()
  expect_equal(nrow(tbl), 16)
  expect_equal(anyDuplicated(tbl$variable), 0L)
  expect_equal(anyDuplicated(tbl$short_name), 0L)
  # verified against a CDS download of all sixteen variables
  expect_equal(era5_variable_short("u_component_of_wind"), "u")
  expect_equal(era5_variable_short("vorticity"), "vo")
  expect_equal(era5_variable_short("vertical_velocity"), "w")
  expect_equal(era5_variable_short("specific_cloud_ice_water_content"), "ciwc")
  expect_equal(
    era5_variable_short(c("temperature", "geopotential")),
    c("t", "z")
  )
})

test_that("long names pass through untouched", {
  v <- c("u_component_of_wind", "v_component_of_wind")
  expect_silent(expect_equal(era5_variable_canonical(v), v))
  expect_silent(expect_equal(era5_variable_canonical(v, allow_short = TRUE), v))
})

test_that("short names are translated where they used to be accepted", {
  expect_snapshot(era5_variable_canonical(c("u", "v"), allow_short = TRUE))
  expect_equal(
    suppressWarnings(era5_variable_canonical(c("u", "v"), allow_short = TRUE)),
    c("u_component_of_wind", "v_component_of_wind")
  )
})

test_that("short names are refused where CDS is called", {
  # tag_download_wind() sends these straight to CDS, which would reject them
  expect_snapshot(error = TRUE, era5_variable_canonical(c("u", "v")))
})

test_that("unknown names are refused, with a suggestion when close", {
  expect_snapshot(error = TRUE, era5_variable_canonical("u_component_of_win"))
  expect_snapshot(error = TRUE, era5_variable_canonical("banana"))
})

test_that("both halves of the wind pipeline default to the same names", {
  expect_equal(
    eval(formals(tag_download_wind)$variable),
    eval(formals(edge_add_wind)$variable)
  )
  expect_equal(
    eval(formals(edge_add_wind)$variable),
    c("u_component_of_wind", "v_component_of_wind")
  )
  # and the internal checker agrees, so graph_add_wind() validates the same way
  expect_equal(
    eval(formals(edge_add_wind_check)$variable),
    eval(formals(edge_add_wind)$variable)
  )
})

test_that("`w` is a short name and a column name without colliding", {
  # edge_add_wind() returns a weight column called `w`, which is also the short name of
  # vertical_velocity; the registry must not be consulted for column names
  expect_equal(era5_variable_short("vertical_velocity"), "w")
  expect_error(era5_variable_canonical("w"), "CDS understands")
})
