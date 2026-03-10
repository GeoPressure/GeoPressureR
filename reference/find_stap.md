# Find the stationary period corresponding to a date

Find the stationary period corresponding to a date

## Usage

``` r
find_stap(stap, date)
```

## Arguments

- stap:

  a data.frame with columns `start` and `end` defining stationary
  periods.

- date:

  a POSIXct vector of datetimes to map to stationary periods.

## Value

Numeric vector of `stap_id` indices (fractional values for in-flight
gaps).
