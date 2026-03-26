# Read or validate stap data

Reads a CSV file defining coarse stationary periods (`stap0`) or
validates a data.frame, including date parsing, ordering, and overlap
checks.

## Usage

``` r
read_stap(x, required_cols = c("start", "end"))
```

## Arguments

- x:

  a GeoPressureR `tag` object, a `stap` data.frame, or a CSV path. If a
  `tag` is provided, the default file path is
  `./data/stap-label/{tag$param$id}.csv`.

- required_cols:

  character vector of required columns (default: `c("start", "end")`).

## Value

A data.frame containing at least the required columns, sorted by
`start`.

## See also

Other tag:
[`print.tag()`](https://geopressure.org/GeoPressureR/reference/print.tag.md),
[`tag_create()`](https://geopressure.org/GeoPressureR/reference/tag_create.md),
[`tag_set_map()`](https://geopressure.org/GeoPressureR/reference/tag_set_map.md),
[`tag_stap_daily()`](https://geopressure.org/GeoPressureR/reference/tag_stap_daily.md)

## Examples

``` r
# From a data.frame
stap_df <- data.frame(
  start = as.POSIXct(c("2020-01-01", "2020-01-03"), tz = "UTC"),
  end = as.POSIXct(c("2020-01-02", "2020-01-04"), tz = "UTC")
)
read_stap(stap_df)
#>        start        end
#> 1 2020-01-01 2020-01-02
#> 2 2020-01-03 2020-01-04

# From a CSV path
# read_stap("./data/stap-label/18LX.csv")
```
