# Estimate one location per stationary period from twilight times

For each stationary period (`stap_id`) with usable twilight events,
estimate a single longitude, latitude, and effective zenith angle by
minimizing the squared spherical distance to the corresponding
iso-zenith small circles.

Each twilight defines a small circle on the Earth: the set of locations
at a fixed angular distance (zenith angle) from the subsolar point at
that time. The fitted location is the point that is, in a least-squares
sense, closest to all those circles simultaneously.

## Usage

``` r
geolight_fit_location(
  tag,
  fitted_location_duration = Inf,
  extent = NULL,
  zenith_init = 96,
  zenith_bounds = c(60, 120),
  compute_known = FALSE,
  quiet = FALSE
)
```

## Arguments

- tag:

  A GeoPressureR tag object containing `tag$twilight` and `tag$stap`.

- fitted_location_duration:

  Minimum duration (in days) of stationary period(s) eligible to be used
  as a fitted location from twilight times. Default is `Inf` (disabled).

- extent:

  Numeric vector `c(W, E, S, N)` in degrees (longitude west/east,
  latitude south/north).

- zenith_init:

  Initial zenith angle (degrees).

- zenith_bounds:

  Numeric vector of length 2 giving zenith bounds (degrees).

- compute_known:

  Logical; if FALSE, known stationary periods are copied from `tag$stap`
  (rather than being estimated).

- quiet:

  Logical; if TRUE, suppress informative messages.

## Value

A `path` data.frame derived from `tag$stap` with added columns: `lon`,
`lat`, `zenith`. Rows where fitting was not possible remain `NA`.

## See also

Other geolight:
[`geolight_map()`](https://raphaelnussbaumer.com/GeoPressureR/reference/geolight_map.md),
[`geolight_solar()`](https://raphaelnussbaumer.com/GeoPressureR/reference/geolight_solar.md),
[`plot_twl_calib()`](https://raphaelnussbaumer.com/GeoPressureR/reference/plot_twl_calib.md),
[`twilight_create()`](https://raphaelnussbaumer.com/GeoPressureR/reference/twilight_create.md),
[`twilight_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/reference/twilight_label_read.md),
[`twilight_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/reference/twilight_label_write.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create() |>
    twilight_label_read()
})
tag <- tag_stap_daily(tag, quiet = TRUE)
path <- geolight_fit_location(tag, fitted_location_duration = 5, quiet = TRUE)
```
