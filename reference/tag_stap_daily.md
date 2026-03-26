# Create stationary periods from twilight midpoints

Build a `stap` table using midpoints between consecutive twilight pairs
so the period lengths follow the twilight drift. If a `stap0` is
provided (as a data.frame or a CSV path), its intervals are kept as-is
and the remaining gaps are filled using twilight midpoints.

## Usage

``` r
tag_stap_daily(
  tag,
  stap0 = NULL,
  twl_grouping = "night",
  max_twl_gap_hours = 23.5,
  quiet = FALSE
)
```

## Arguments

- tag:

  a GeoPressureR `tag` object with `twilight`.

- stap0:

  optional `stap` data.frame with columns `start` and `end`, a CSV path
  with those columns (POSIXct-compatible strings), or a tag `id`
  resolved by
  [`read_stap()`](https://geopressure.org/GeoPressureR/reference/read_stap.md).

- twl_grouping:

  Which period is assumed to be when the bird is moving: `"night"`
  (default) assumes the bird is moving during the night, so
  sunrise-to-sunset twilights are combined into the same stationary
  period, while `"day"` assumes the bird is moving during the day, so
  sunset-to-sunrise twilights are combined into the same stationary
  period.

- max_twl_gap_hours:

  maximum allowed gap between consecutive twilights (in hours) before
  erroring, indicating missing twilights.

- quiet:

  logical to hide messages.

## Value

Updated `tag` with a twilight-based `stap` and optional `stap0`. The
`stap` includes a logical column `stap0` set to `TRUE` when intervals
come from the provided `stap0` input. If `stap0` includes `known_lat` or
`known_lon`, those columns are kept on matching `stap` rows.

## See also

Other tag:
[`print.tag()`](https://geopressure.org/GeoPressureR/reference/print.tag.md),
[`read_stap()`](https://geopressure.org/GeoPressureR/reference/read_stap.md),
[`tag_create()`](https://geopressure.org/GeoPressureR/reference/tag_create.md),
[`tag_set_map()`](https://geopressure.org/GeoPressureR/reference/tag_set_map.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create() |>
    twilight_label_read()
})
tag <- tag_stap_daily(tag, twl_grouping = "night")
#> Warning: The `tag` object already has a stap defined which will be overwriten.
```
