library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

test_that("geolight_fit_location() does not estimate known staps by default", {
  known <- data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)

  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4,
      known = known
    ) |>
    twilight_create() |>
    twilight_label_read()

  path <- geolight_fit_location(tag, fitted_location_duration = 0)

  known_row <- path[path$stap_id == 1, ]
  expect_true(is.na(known_row$zenith))
  expect_equal(known_row$lon, known$known_lon)
  expect_equal(known_row$lat, known$known_lat)
})
