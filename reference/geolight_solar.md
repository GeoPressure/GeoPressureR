# Solar zenith angle using NOAA / Meeus equations

Compute solar zenith angles from time and location using the formulation
implemented in the NOAA Solar Calculator, itself based on the
astronomical algorithms of Meeus.

## Usage

``` r
geolight_solar(date, lat, lon)
```

## Arguments

- date:

  POSIXct vector of times.

- lat:

  Numeric vector of latitudes (degrees north).

- lon:

  Numeric vector of longitudes (degrees east).

## Value

Solar zenith angle(s) in degrees, corrected for atmospheric refraction.

## Details

The computation is split internally into three steps:

1.  `geolight_solar_constants()`: Time-only solar quantities (solar time
    and declination)

2.  `geolight_solar_zenith()`: Geometric solar zenith angle

3.  `geolight_solar_refracted()`: Approximate atmospheric refraction
    correction

### Conventions

- `lon` is expressed in **degrees east** (positive eastward).

- `lat` is expressed in **degrees north**.

- All angles are in **degrees** unless stated otherwise.

- `date` is provided as POSIXct and internally converted to Julian day.

When `lat` and `lon` are vectors, the result is a 3-D array with
dimensions:

`c(length(lat), length(lon), length(date))`

When `lat` and `lon` are scalars, the result reduces to a vector of
length `length(date)`.

### Solar time and hour angle

Internally, the solar time is computed as NOAA's *true solar time*
expressed as an angle (degrees) at Greenwich, without longitude
adjustment. Longitude enters later through the hour angle:

`hour_angle = solar_time + lon - 180`

Values are intentionally not wrapped to `[0, 360)` because all
downstream trigonometric functions are periodic.

### Atmospheric refraction

Atmospheric refraction is applied using NOAA's standard piecewise
approximation as a function of solar elevation. This correction is
approximate and does not account for local pressure or temperature.

## References

NOAA Global Monitoring Laboratory. Solar Calculation Details.
<https://gml.noaa.gov/grad/solcalc/calcdetails.html>

Meeus, J. (1998). *Astronomical Algorithms*. Willmann-Bell.

## See also

Other geolight:
[`geolight_fit_location()`](https://geopressure.org/GeoPressureR/reference/geolight_fit_location.md),
[`geolight_map()`](https://geopressure.org/GeoPressureR/reference/geolight_map.md),
[`plot_twl_calib()`](https://geopressure.org/GeoPressureR/reference/plot_twl_calib.md),
[`twilight_create()`](https://geopressure.org/GeoPressureR/reference/twilight_create.md),
[`twilight_label_read()`](https://geopressure.org/GeoPressureR/reference/twilight_label_read.md),
[`twilight_label_write()`](https://geopressure.org/GeoPressureR/reference/twilight_label_write.md)

## Examples

``` r
date <- as.POSIXct(
  c("2020-06-21 12:00:00", "2020-12-21 12:00:00"),
  tz = "UTC"
)
z <- geolight_solar(date, lat = 46, lon = 6)
z
#> , , 1
#> 
#>          [,1]
#> [1,] 22.99545
#> 
#> , , 2
#> 
#>          [,1]
#> [1,] 69.63826
#> 
```
