library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

tag_twl <- tag_create("18LX", quiet = TRUE) |>
  tag_label(quiet = TRUE) |>
  twilight_create() |>
  twilight_label_read()
tag_twl_daily <- tag_stap_daily(tag_twl, quiet = TRUE)

test_that("geolight_fit_location() does not estimate known staps by default", {
  known <- data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)

  tag <- tag_twl |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4,
      known = known
    )

  expect_warning({
    path <- geolight_fit_location(tag, fitted_location_duration = 0)
  })

  known_row <- path[path$stap_id == 1, ]
  expect_true(is.na(known_row$zenith))
  expect_equal(known_row$lon, known$known_lon)
  expect_equal(known_row$lat, known$known_lat)
})

test_that("geolight_fit_location() works without known columns in stap", {
  path <- expect_no_error(
    geolight_fit_location(tag_twl_daily, fitted_location_duration = 5, quiet = TRUE)
  )

  expect_true(all(c("known_lon", "known_lat", "lon", "lat", "zenith") %in% names(path)))
})
