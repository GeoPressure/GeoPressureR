# Start the GeoPressure Trainset shiny app

GeoPressure Trainset is a shiny app designed to help you manually label
pressure and acceleration data for training machine learning models.
This interactive app allows you to visualize time series data, select
data points or regions, and assign behavioral labels (e.g., "flight",
"discard", or custom elevation labels) to create training datasets.

## Usage

``` r
trainset(x, launch_browser = TRUE, run_bg = TRUE, debug = FALSE)
```

## Arguments

- x:

  One of:

  - a GeoPressureR `tag` object,

  - a path to an interim `.RData` file containing an object called
    `tag`,

  - a path to a TRAINSET-compatible `.csv` label file,

  - a single-character `id`. In that case the function will first look
    for `"./data/interim/{id}.RData"`, and if not found will look for
    `"./data/tag-label/{id}-labeled.csv"` and then
    `"./data/tag-label/{id}.csv"`.

- launch_browser:

  If true (by default), the app runs in your browser, otherwise it runs
  on Rstudio.

- run_bg:

  If true, the app runs in a background R session using the `callr`
  package. This allows you to continue using your R session while the
  app is running.

- debug:

  If `TRUE`, prints debug messages about plot refreshes (x-range changes
  and point counts) to the R console. Defaults to `FALSE`.

## Value

A GeoPressureR `tag` object (with pressure and optionally acceleration
data and an `id` in `tag$param$id`). If `run_bg = TRUE`, a background
process object is returned invisibly, with the `tag` attached as an
attribute `attr(p, "tag")`. The labeled data can be exported directly
from the app interface.

## Details

The app features:

- Interactive plotly visualization of pressure and acceleration time
  series

- Point and region selection for efficient labeling

- Support for stationary periods (STAP) navigation

- Custom label creation (elevation labels)

- Export functionality to save labeled data as CSV files

Learn more about data labeling workflows in the
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/) or
explore the [GeoPressureR
documentation](https://raphaelnussbaumer.com/GeoPressureR/).

## See also

[`tag_label_read()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_read.md),
[`tag_label_write()`](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_write.md),
[GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/)

## Examples

``` r
if (FALSE) {
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
})
trainset(tag, run_bg = FALSE, launch_browser = FALSE)
}
```
