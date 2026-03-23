# Create water mask for geolight map

This function creates a water mask for maps with `geolight_map` already
run. The mask is based on coastline data from Natural Earth and is
stored as `map$mask_water` as a boolean matrix where `TRUE` indicates
water and `FALSE` indicates land.

A pixel is marked as land if any land exists within it (using the
conservative `touches=TRUE` approach in rasterization), which is
important for detecting small islands; the mask then flags the remaining
pixels as water.

## Usage

``` r
map_add_mask_water(map, ne_scale = "medium")
```

## Arguments

- map:

  a GeoPressureR `map` object from `geolight_map`.

- ne_scale:

  Natural Earth resolution passed to
  `rnaturalearth::ne_countries(scale = ...)`. Valid values are
  `"small"`, `"medium"`, `"large"` or `c(110, 50, 10)`. Default is
  `"medium"`.

## Value

A `map` object with `mask_water` added as a logical matrix.

## Details

The coastline data is rasterized to match the map's grid (defined by
`extent` and `scale` from the map). For `"large"`/`10` resolution,
`rnaturalearthhires` is used by `rnaturalearth` when available.

## See also

Other map:
[`map_create()`](https://geopressure.org/GeoPressureR/reference/map_create.md),
[`print.map()`](https://geopressure.org/GeoPressureR/reference/print.map.md),
[`rast.map()`](https://geopressure.org/GeoPressureR/reference/rast.map.md)

## Examples

``` r
if (FALSE) {
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      scale = 4,
      known = data.frame(
        stap_id = 1,
        known_lon = 17.05,
        known_lat = 48.9
      )
    ) |>
    twilight_create() |>
    twilight_label_read() |>
    geolight_map(quiet = TRUE)

  # Create water mask
  tag$map_light <- map_add_mask_water(tag$map_light)

  # View the mask
  image(tag$map_light$mask_water)
})
}
```
