#' Construct a SpatRaster from a `map`
#'
#' This function convert a GeoPressureR `map` object into a [`terra::SpatRaster`] with the data of
#' each stationary periods stored in a different layer.
#'
#' @param x a GeoPressureR `map` object
#' @param names names of the SpatRaster layers created. See [`terra::names`].
#' @inheritParams terra::rast
#' @param ... additional parameters for `terra::rast`
#'
#' @return A [terra::SpatRaster] object.
#'
#'
#' @examples
#' # Load a tag with pressure and light likelihood maps computed with `geopressuretemplate()`
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- load_interim("18LX", var = "tag")
#' })
#'
#' rast.map(tag$map_pressure)
#'
#' @family map
#' @export
rast.map <- function(
  x,
  names = glue::glue("#{map$stap$stap_id}"),
  crs = "epsg:4326",
  ...
) {
  map <- x

  assertthat::assert_that(inherits(map, "map"))

  # Replace stap with NULL value in `map` with a matrix of NA (this should only happen with
  # map_pressure_mse or map_pressure_mask)
  stap_id_null <- which(vapply(map$data, is.null, logical(1)))
  for (i in stap_id_null) {
    map$data[[i]] <- matrix(NA, nrow = dim(map)[1], ncol = dim(map)[2])
  }

  r <- terra::rast(
    simplify2array(map$data),
    extent = map$extent,
    crs = crs,
    ...
  )

  # add starting date as time

  terra::time(r) <- as.POSIXct(map$stap$start, tz = "UTC")

  # Add stap_id as name
  names(r) <- names

  return(r)
}
