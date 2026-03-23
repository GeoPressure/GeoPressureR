# Plot movement model of a `graph`

This function display a plot of pressure time series recorded by a tag

## Usage

``` r
plot_graph_movement(graph, speed = seq(0, 120), plot_plotly = FALSE)
```

## Arguments

- graph:

  a GeoPressureR `graph` object or a movement list (from
  `graph_set_movement`).

- speed:

  Vector of speed value (km/h) used on the x-axis.

- plot_plotly:

  logical to use `plotly`

## Value

a plot or ggplotly object.

## See also

Other movement:
[`bird_create()`](https://geopressure.org/GeoPressureR/reference/bird_create.md),
[`graph_set_movement()`](https://geopressure.org/GeoPressureR/reference/graph_set_movement.md),
[`speed2prob()`](https://geopressure.org/GeoPressureR/reference/speed2prob.md),
[`tag_download_wind()`](https://geopressure.org/GeoPressureR/reference/tag_download_wind.md)

## Examples

``` r
movement_gamma <- list(
  type = "gs",
  method = "gamma",
  shape = 7,
  scale = 7,
  low_speed_fix = 15,
  zero_speed_ratio = 1
)
plot_graph_movement(movement_gamma)


movement_logis <- list(
  type = "gs",
  method = "logis",
  scale = 7,
  location = 40,
  low_speed_fix = 15,
  zero_speed_ratio = 1
)
plot_graph_movement(movement_logis)


bird <- bird_create("Example bird", mass = 0.1, wing_span = 0.4, wing_aspect = 7)
movement_power <- list(
  type = "as",
  method = "power",
  bird = bird,
  power2prob = \(power) (1 / power)^3,
  low_speed_fix = 15,
  zero_speed_ratio = 1
)
plot_graph_movement(movement_power)
```
