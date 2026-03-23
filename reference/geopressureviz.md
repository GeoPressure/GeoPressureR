# Start the GeoPressureViz shiny app

GeoPressureViz is an interactive app to inspect and manually edit
migration paths together with the underlying map likelihoods and
pressure time series by stationary period. It is useful to compare
pressure-, light- and distance-based information before finalizing a
path.

The app can start from a `tag` object already in memory, or from an
interim `.RData`/`.rda` file that contains at least `tag` (and
optionally `marginal`, `path_most_likely`, `pressurepath`,
`pressurepath_most_likely`).

You can retrieve the edited path from the return value of this function
(when `run_bg = FALSE`) or with
`shiny::getShinyOption("path_geopressureviz")` after the app completes.

Learn more about GeoPressureViz in the
[GeoPressureManual](https://geopressure.org/GeoPressureManual/geopressureviz.html)
.

## Usage

``` r
geopressureviz(
  x,
  path = NULL,
  marginal = NULL,
  launch_browser = TRUE,
  run_bg = TRUE
)
```

## Arguments

- x:

  One of:

  - a GeoPressureR `tag` object;

  - a path to an existing `.RData`/`.rda` file;

  - a tag id (character scalar), interpreted as
    `"./data/interim/{id}.RData"`.

- path:

  Optional GeoPressureR `path` or `pressurepath` data.frame. If `NULL`,
  a path is resolved from available inputs in this order:

  - for file/id input (`x` character): `path_most_likely` (if present),
    then `pressurepath` (if present);

  - otherwise fallback to `tag2path(tag, interp = 1)`.

- marginal:

  map of the marginal probability computed with
  [`graph_marginal()`](https://geopressure.org/GeoPressureR/reference/graph_marginal.md).
  Overwrite the `path` or `pressurepath` contained in the `.Rdata` file.

- launch_browser:

  If true (by default), the app runs in your browser, otherwise it runs
  on Rstudio.

- run_bg:

  If true, the app runs in a background R session using the `callr`
  package. This allows you to continue using your R session while the
  app is running.

## Value

When `run_bg = FALSE`: The updated path visualized in the app. Can also
be retrieved with `shiny::getShinyOption("path_geopressureviz")` after
the app completes. When `run_bg = TRUE`: Returns the background process
object.

## See also

[GeoPressureManual](https://geopressure.org/GeoPressureManual/geopressureviz.html)

## Examples

``` r
if (FALSE) {
  geopressureviz("18LX")
}
```
