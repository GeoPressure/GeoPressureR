# Calibrate twilight to zenith angle using calibration locations

Calibrate twilight to zenith angle using calibration locations

## Usage

``` r
geolight_calibrate(twl, calib_stap, twl_calib_adjust = 1.4)
```

## Arguments

- twl:

  a twilight data.frame

- calib_stap:

  a stationary period data.frame with calibration locations. Must
  contain the columns `known_lat`, `known_lon`, `start`, and `end`. This
  can include both true known locations and estimated (fitted)
  locations.

- twl_calib_adjust:

  smoothing parameter for the kernel density (see
  [`stats::density()`](https://rdrr.io/r/stats/density.html)).

## Value

a `twl_calib` object with components:

- `x`: the zenith angle sequence

- `y`: the estimated density values

- `hist_breaks`: common histogram breaks used for all calibration staps

- `hist_counts`: list of histogram counts per calibration stationary
  period

- `calib_stap`: calibration stationary periods used for calibration
