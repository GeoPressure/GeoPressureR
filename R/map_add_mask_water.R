#' Create water mask for a `map`
#'
#' @description
#' This function creates a water mask for a `map`, typically the light likelihood map computed by
#' `geolight_map()`. The mask is based on country polygons from Natural Earth and is stored as
#' `map$mask_water` as a logical matrix where `TRUE` indicates water and `FALSE` indicates land.
#'
#' A pixel is marked as water when the fraction of its area covered by land is at or below
#' `land_threshold`. With the default `land_threshold = 0`, a pixel is land if any land exists
#' within it, which is important for detecting small islands.
#'
#' @param map a GeoPressureR `map` object, e.g. `tag$map_light` from `geolight_map()`.
#' @param ne_scale Natural Earth resolution passed to
#'   `rnaturalearth::ne_countries(scale = ...)`. Valid values are `"small"`,
#'   `"medium"`, `"large"` or `c(110, 50, 10)`. Default is `"medium"`.
#' @param land_threshold Numeric threshold used to classify pixels as water from
#'   the rasterized land fraction. Pixels with land fraction at or below
#'   `land_threshold` are marked as water. Use `0` for a conservative mask where
#'   any land in a pixel makes it land.
#'
#' @return A `map` object with `mask_water` added as a logical matrix.
#'
#' @details
#' The land polygons are rasterized to match the map's grid (defined by `extent` and
#' `scale` from the map). `land_threshold` controls how much land can be present
#' in a pixel before it is treated as land in `map$mask_water`. For `"large"`/`10`
#' resolution, `rnaturalearthhires` is used by `rnaturalearth` when available.
#'
#' @examplesIf FALSE
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 4,
#'       known = data.frame(
#'         stap_id = 1,
#'         known_lon = 17.05,
#'         known_lat = 48.9
#'       )
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
map_add_mask_water <- function(map, ne_scale = "medium", land_threshold = 0) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg rnaturalearth} is required for {.fun map_add_mask_water}.",
      "i" = "Install it with {.run install.packages('rnaturalearth')}."
    ))
  }
  if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg rnaturalearthdata} is required for {.fun map_add_mask_water}.",
      "i" = "Install it with {.run install.packages('rnaturalearthdata')}."
    ))
  }
  assertthat::assert_that(inherits(map, "map"))

  map$mask_water <- mask_water(
    extent = map$extent,
    scale = map$scale,
    ne_scale = ne_scale,
    land_threshold = land_threshold
  )

  map
}


#' Internal function to create water mask from extent and scale
#'
#' @param extent Numeric vector of length 4: c(W, E, S, N) in degrees
#' @param scale Numeric scale factor (pixels per degree)
#' @param ne_scale Natural Earth resolution: `"small"`, `"medium"` or `"large"` (or 110, 50, 10).
#' @param land_threshold Land fraction at or below which a cell is water.
#'
#' @return Logical matrix where TRUE = water, FALSE = land
#'
#' @examplesIf FALSE
#' # View the water mask around Caribbean islands
#' extent <- c(-86, -58, 10, 28)
#' scale <- 5
#' mask <- mask_water(extent, scale)
#' g <- map_expand(extent, scale)
#' land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#'
#' image(
#'   g$lon,
#'   rev(g$lat),
#'   t(mask[nrow(mask):1, ]),
#'   col = c("#d8c589", "#2f7fb8"),
#'   xlab = "Longitude",
#'   ylab = "Latitude",
#'   main = glue::glue("scale = ", scale),
#'   asp = 1,
#'   useRaster = TRUE
#' )
#' terra::plot(terra::vect(land), add = TRUE, border = "black", col = NA, lwd = 0.5)
#' @noRd
mask_water <- function(extent, scale, ne_scale = "medium", land_threshold = 0) {
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

  # Rasterize the fraction of each cell covered by land polygons (0 to 1)
  r_land <- terra::rasterize(land_vect, r_template, cover = TRUE, background = 0)

  # Convert to matrix (capped at 1 where polygons overlap) and mark as water when the land fraction
  # is at or below land_threshold
  land_fraction <- pmin(terra::as.matrix(r_land, wide = TRUE), 1)

  land_fraction <= land_threshold
}
