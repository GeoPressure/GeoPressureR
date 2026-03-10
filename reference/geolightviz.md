# Start the GeoLightViz shiny app

Start the GeoLightViz shiny app

## Usage

``` r
geolightviz(
  x,
  stapath = NULL,
  launch_browser = TRUE,
  run_bg = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  a GeoPressureR `tag` object, a `.Rdata` file or the unique identifier
  `id` with a `.Rdata` file located in `"./data/interim/{id}.RData"`.

- stapath:

  optional stationary path data.frame (defaults to `tag$stap` when
  available).

- launch_browser:

  If true (by default), the app runs in your browser, otherwise it runs
  on Rstudio.

- run_bg:

  If true (by default), the app runs in a background R process using
  [`callr::r_bg()`](https://callr.r-lib.org/reference/r_bg.html),
  allowing you to continue using the R console. If false, the app blocks
  the console until closed.

- quiet:

  logical, currently unused.

- ...:

  currently unused.

## Value

When `run_bg = TRUE`, an invisible `callr` `r_process` running the app.
When `run_bg = FALSE`, the return value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Examples

``` r
if (FALSE) {
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create() |>
    twilight_label_read()
})
geolightviz(tag, run_bg = FALSE, launch_browser = FALSE)
}
```
