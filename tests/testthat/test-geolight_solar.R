library(testthat)
library(GeoPressureR)

test_that("geolight_solar returns correct dimensions", {
  date <- as.POSIXct(
    c("2000-06-21 12:00:00", "2000-12-21 12:00:00"),
    tz = "UTC"
  )
  lat <- c(-45, 0, 45)
  lon <- c(-90, 0, 90)

  z <- geolight_solar(date, lat, lon)

  expect_true(is.array(z))
  expect_equal(dim(z), c(length(lat), length(lon), length(date)))
})


test_that("solar zenith at equator and equinox is near zero at noon", {
  date <- as.POSIXct("2000-03-20 12:00:00", tz = "UTC")
  lat <- 0
  lon <- 0

  z <- geolight_solar(date, lat, lon)

  # Allow a small tolerance due to equation of time and refraction
  expect_lt(abs(z), 2)
})


test_that("refraction correction reduces zenith angle near horizon", {
  zenith <- c(89, 88, 87)

  z_ref <- geolight_solar_refracted(zenith)

  # Refraction always reduces apparent zenith (sun appears higher)
  expect_true(all(z_ref < zenith))
})


test_that("wrapper equals composed internal calls", {
  date <- as.POSIXct("2001-01-01 06:00:00", tz = "UTC")
  lat <- c(-10, 10)
  lon <- c(20, 40)

  z1 <- geolight_solar(date, lat, lon)

  sun <- geolight_solar_constants(date)
  z2 <- geolight_solar_refracted(
    geolight_solar_zenith(sun, lat, lon)
  )

  expect_equal(z1, z2)
})


test_that("zenith angles are within physical bounds", {
  date <- as.POSIXct(
    seq(
      as.POSIXct("2000-01-01 00:00:00", tz = "UTC"),
      length.out = 24,
      by = "1 hour"
    )
  )

  lat <- seq(-80, 80, by = 20)
  lon <- seq(-180, 180, by = 60)

  z <- geolight_solar(date, lat, lon)

  expect_true(all(z >= 0))
  expect_true(all(z <= 180))
})
