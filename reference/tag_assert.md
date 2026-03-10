# Assert the status of a `tag`

This function check the condition of a `tag` object.

## Usage

``` r
tag_assert(tag, condition = "tag", type = "abort")
```

## Arguments

- tag:

  a GeoPressureR `tag` object.

- condition:

  condition to assert `tag` for. One of `"tag"` (default), `"read"`,
  `"pressure"`, `"light"`, `"acceleration"`, `"temperature_internal"`,
  `"temperature_external"`, `"magnetic"`, `"label"`, `"stap"`,
  `"setmap"`, `"map_pressure"`, `"map_pressure_mse"`,
  `"map_pressure_mask"`, `"map_light"`, `"map_light_twl"`,
  `"mask_water"`, `"map_magnetic"`, `"map_magnetic_intensity"`,
  `"map_magnetic_inclination"`, `"twl_calib"`, and `"twilight"`.

- type:

  Message type to display. One of `"abort"` (default), `"warn"`,
  `"inform"`, or `"logical"` (return TRUE/FALSE without messaging).

## Value

logical indicating the `tag` object has the relevant element

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
})

tag_assert(tag)
#> [1] TRUE

tag_assert(tag, "stap")
#> [1] TRUE

tag_assert(tag, "setmap", type = "warn")
#> Warning: ✖ The parameters for the geographical and stationary period have not been yet
#>   been defined in `tag`.
#> → Use `tag_set_map()` to define them.

tag_assert(tag, "map_pressure", type = "inform")
#> ✖ The pressure likelihood map has not yet been computed for `tag`.
#> → Use `geopressure_map()` to compute the maps.

tag_assert(tag, "setmap", type = "logical")
#> [1] FALSE
```
