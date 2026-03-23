# Plot a `path`

This function plots a `path` data.frame. This function is used in
[`plot.map()`](https://geopressure.org/GeoPressureR/reference/plot.map.md).

## Usage

``` r
plot_path(
  path,
  plot_leaflet = TRUE,
  map = NULL,
  provider = "Esri.WorldTopoMap",
  provider_options = leaflet::providerTileOptions(),
  pad = 3,
  polyline = NULL,
  circle = NULL
)
```

## Arguments

- path:

  a GeoPressureR `path` data.frame.

- plot_leaflet:

  logical defining if the plot is an interactive `leaflet` map or a
  static basic plot.

- map:

  optional `map` object to plot the path on top of.

- provider:

  tile provider name (see
  [`leaflet::providers`](https://rstudio.github.io/leaflet/reference/providers.html)).

- provider_options:

  tile options. See leaflet::addProviderTiles() and
  leaflet::providerTileOptions()

- pad:

  padding of the map in degree lat-lon (only for
  `plot_leaflet = FALSE`).

- polyline:

  list of parameters passed to
  [`leaflet::addPolylines()`](https://rstudio.github.io/leaflet/reference/map-layers.html)

- circle:

  list of parameters passed to
  [`leaflet::addCircleMarkers()`](https://rstudio.github.io/leaflet/reference/map-layers.html)

## Value

A `leaflet` map when `plot_leaflet = TRUE`, otherwise a `ggplot2`
object.

## See also

[`plot.map()`](https://geopressure.org/GeoPressureR/reference/plot.map.md)

Other path:
[`path2edge()`](https://geopressure.org/GeoPressureR/reference/path2edge.md),
[`path2elevation()`](https://geopressure.org/GeoPressureR/reference/path2elevation.md),
[`path2twilight()`](https://geopressure.org/GeoPressureR/reference/path2twilight.md),
[`tag2path()`](https://geopressure.org/GeoPressureR/reference/tag2path.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE)
})

path <- data.frame(
  stap_id = 1:4,
  lon = c(17.05, 16.2, NA, 15.4),
  lat = c(48.9, 47.8, NA, 46.5),
  known_lon = c(17.05, NA, NA, NA),
  known_lat = c(48.9, NA, NA, NA),
  start = as.POSIXct(
    c("2018-01-01", "2018-01-03", "2018-01-05", "2018-01-07"),
    tz = "UTC"
  ),
  end = as.POSIXct(
    c("2018-01-02", "2018-01-04", "2018-01-06", "2018-01-08"),
    tz = "UTC"
  )
)

plot_path(path)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addPolylines","args":[[[[{"lng":[17.05,16.2,15.4],"lat":[48.9,47.8,46.5]}]]],null,[1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"grey","weight":5,"opacity":0.35,"fill":false,"fillColor":"grey","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addPolylines","args":[[[[{"lng":[17.05,16.2],"lat":[48.9,47.8]}]],[[{"lng":[15.4],"lat":[46.5]}]]],null,[1,1,1,1],{"interactive":true,"className":"","stroke":true,"color":"black","weight":5,"opacity":0.7,"fill":false,"fillColor":"black","fillOpacity":0.2,"smoothFactor":1,"noClip":false},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[[48.9,47.8,46.5],[17.05,16.2,15.4],[6,6,6,6],null,[1,1,1],{"interactive":true,"className":"","stroke":true,"color":"white","weight":2,"opacity":1,"fill":[true,true,true,true],"fillColor":["#D1495B","black","black","black"],"fillOpacity":0.8},null,null,null,null,["#1, 1 days","#2, 1 days","#3, 1 days","#4, 1 days"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[46.5,48.9],"lng":[15.4,17.05]}},"evals":[],"jsHooks":[]}
plot_path(path, plot_leaflet = FALSE)

```
