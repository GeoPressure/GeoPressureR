#' Create water mask for geolight map
#'
#' @description
#' This function creates a water mask for maps with `geolight_map` already run.
#' The mask is based on coastline data from Natural Earth and is stored as
#' `map$mask_water` as a boolean matrix where `TRUE` indicates water and
#' `FALSE` indicates land.
#'
#' A pixel is marked as land if any land exists within it (using the conservative
#' `touches=TRUE` approach in rasterization), which is important for detecting small
#' islands; the mask then flags the remaining pixels as water.
#'
#' @param map a GeoPressureR `map` object from `geolight_map`.
#' @param ne_scale Natural Earth resolution passed to
#'   `rnaturalearth::ne_countries(scale = ...)`. Valid values are `"small"`,
#'   `"medium"`, `"large"` or `c(110, 50, 10)`. Default is `"medium"`.
#'
#' @return A `map` object with `mask_water` added as a logical matrix.
#'
#' @details
#' The coastline data is rasterized to match the map's grid (defined by `extent` and
#' `scale` from the map). For `"large"`/`10` resolution, `rnaturalearthhires` is used
#' by `rnaturalearth` when available.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 4
#'     ) |>
#'     twilight_create() |>
#'     twilight_label_read() |>
#'     geolight_map(quiet = TRUE)
#'
#'   # Create water mask
#'   tag$map_light <- map_add_mask_water(tag$map_light)
#'
#'   # View the mask
#'   image(tag$map_light$mask_water)
#' })
#'
#' @family map
#' @export
map_add_mask_water <- function(map, ne_scale = "medium") {
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg rnaturalearth} is required for {.fun map_add_mask_water}.",
      "i" = "Install it with {.run install.packages('rnaturalearth')}."
    ))
  }
  assertthat::assert_that(inherits(map, "map"))

  map$mask_water <- mask_water(
    extent = map$extent,
    scale = map$scale,
    ne_scale = ne_scale
  )

  map
}


#' Internal function to create water mask from extent and scale
#'
#' @param extent Numeric vector of length 4: c(W, E, S, N) in degrees
#' @param scale Numeric scale factor (pixels per degree)
#' @param ne_scale Natural Earth resolution: 10, 50, or 110. Default is 50.
#'
#' @return Logical matrix where TRUE = water, FALSE = land
#'
#' @noRd
mask_water <- function(extent, scale, ne_scale = "medium") {
  # Validate inputs using map_expand (it will check extent and scale)
  g <- map_expand(extent, scale)

  # Load land polygons from Natural Earth
  # Using ne_countries which includes all land areas (countries + territories)
  land <- rnaturalearth::ne_countries(
    scale = ne_scale,
    returnclass = "sf"
  )

  # Convert sf to SpatVector for terra
  land_vect <- terra::vect(land)

  # Create empty raster template matching the map grid
  # Use terra::rast to create SpatRaster
  r_template <- terra::rast(
    nrows = g$dim[1],
    ncols = g$dim[2],
    xmin = extent[1],
    xmax = extent[2],
    ymin = extent[3],
    ymax = extent[4],
    crs = "EPSG:4326"
  )

  # Rasterize land polygons
  # touches=TRUE marks a cell as land if polygon touches it at all
  # This ensures islands are captured even if cell center is in water
  r_land <- terra::rasterize(
    land_vect,
    r_template,
    field = 1,
    touches = TRUE,
    background = 0
  )

  # Convert to matrix and return logical when water
  terra::as.matrix(r_land, wide = TRUE) == 0
}
