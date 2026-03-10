# Convert datetimes to a plotting time-of-day scale

Convert datetimes to a plotting time-of-day scale

## Usage

``` r
time2plottime(x, ref = x[1])
```

## Arguments

- x:

  a POSIXct vector of datetimes.

- ref:

  reference datetime used to keep ordering around midnight.

## Value

POSIXct vector on an arbitrary day encoding plotted time-of-day.
