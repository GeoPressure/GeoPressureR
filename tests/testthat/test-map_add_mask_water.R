library(testthat)
library(GeoPressureR)

test_that("mask_water() | basic functionality", {
  skip_if_not_installed("rnaturalearth")

  # Test with simple extent/scale
  extent <- c(-10, 5, 50, 60) # UK, Ireland, North Sea
  scale <- 2

  mask <- GeoPressureR:::mask_water(extent, scale)

  # Check that output is a logical matrix
  expect_true(is.matrix(mask))
  expect_type(mask, "logical")

  # Check dimensions match expected grid
  g <- map_expand(extent, scale)
  expect_equal(nrow(mask), length(g$lat))
  expect_equal(ncol(mask), length(g$lon))

  # Check that we have both land and water
  expect_true(any(mask)) # At least some water
  expect_true(!all(mask)) # At least some land
})

test_that("mask_water() | geographic accuracy", {
  skip_if_not_installed("rnaturalearth")

  # Test with UK and Ireland (known land/water distribution)
  extent <- c(-10, 5, 50, 60)
  scale <- 2

  mask <- GeoPressureR:::mask_water(extent, scale)

  # Western pixels should include UK/Ireland (land)
  # Most of western portion should be land
  west_cols <- seq_len(ncol(mask) %/% 3)
  west_land_pct <- mean(!mask[, west_cols])
  expect_gt(west_land_pct, 0.1) # At least 10% should be land

  # Eastern North Sea should be mostly water
  east_cols <- seq(2 * ncol(mask) %/% 3, ncol(mask))
  east_water_pct <- mean(mask[, east_cols])
  expect_gt(east_water_pct, 0.5) # At least 50% should be water
})

test_that("mask_water() | island detection", {
  skip_if_not_installed("rnaturalearth")

  # Test with Azores (small islands in Atlantic)
  extent <- c(-30, -24, 37, 40)
  scale <- 4 # Higher resolution to detect islands

  mask <- GeoPressureR:::mask_water(extent, scale)

  # Should have some land pixels (the Azores islands)
  expect_true(!all(mask), info = "Azores islands should be detected")

  # But mostly water in this region
  land_pct <- mean(!mask)
  expect_lt(land_pct, 0.2)
  expect_gt(land_pct, 0)
})

test_that("mask_water() | all water extent", {
  skip_if_not_installed("rnaturalearth")

  # Test with mid-Atlantic (no land)
  extent <- c(-40, -30, 30, 40)
  scale <- 1

  mask <- GeoPressureR:::mask_water(extent, scale)

  # Should be all water (or nearly all)
  water_pct <- mean(mask)
  expect_gt(water_pct, 0.95)
})

test_that("mask_water() | all land extent", {
  skip_if_not_installed("rnaturalearth")

  # Test with central Europe (mostly land)
  extent <- c(5, 15, 45, 55)
  scale <- 2

  mask <- GeoPressureR:::mask_water(extent, scale)

  # Should be mostly land
  land_pct <- mean(!mask)
  expect_gt(land_pct, 0.7)
})

test_that("mask_water() | different scales", {
  skip_if_not_installed("rnaturalearth")

  extent <- c(-10, 5, 50, 60)

  # Test low resolution
  mask_low <- GeoPressureR:::mask_water(extent, scale = 1)
  expect_true(is.matrix(mask_low))

  # Test medium resolution
  mask_med <- GeoPressureR:::mask_water(extent, scale = 2)
  expect_true(is.matrix(mask_med))

  # Test high resolution (if available)
  skip_if_not_installed("rnaturalearthhires")
  mask_high <- GeoPressureR:::mask_water(extent, scale = 4)
  expect_true(is.matrix(mask_high))

  # Higher resolution should have more pixels
  expect_gt(nrow(mask_high) * ncol(mask_high), nrow(mask_low) * ncol(mask_low))
})

test_that("map_add_mask_water() | main function with map", {
  skip_if_not_installed("rnaturalearth")

  extent <- c(-10, 5, 50, 60)
  scale <- 2
  g <- map_expand(extent, scale)
  stap <- data.frame(
    stap_id = 1,
    start = as.POSIXct("2020-01-01", tz = "UTC"),
    end = as.POSIXct("2020-01-02", tz = "UTC"),
    include = TRUE
  )
  map <- map_create(
    data = list(matrix(1, nrow = length(g$lat), ncol = length(g$lon))),
    extent = extent,
    scale = scale,
    stap = stap,
    id = "test",
    type = "light"
  )

  # Run function
  map <- map_add_mask_water(map)

  # Check that mask was added
  expect_true("mask_water" %in% names(map))
  expect_true(is.matrix(map$mask_water))
  expect_type(map$mask_water, "logical")

  # Check dimensions match map grid
  expect_equal(nrow(map$mask_water), length(g$lat))
  expect_equal(ncol(map$mask_water), length(g$lon))
})
