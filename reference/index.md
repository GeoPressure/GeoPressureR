# Package index

## Tag

Core functions related to a `tag` object.

- [`tag_create()`](https://geopressure.org/GeoPressureR/reference/tag_create.md)
  :

  Create a `tag` object

- [`tag_set_map()`](https://geopressure.org/GeoPressureR/reference/tag_set_map.md)
  :

  Configure the spatial and temporal parameters of the `map` of a `tag`
  object

### Visualize tag

Print and plot a `tag` object.

- [`print(`*`<tag>`*`)`](https://geopressure.org/GeoPressureR/reference/print.tag.md)
  :

  Print a `tag` object

- [`plot(`*`<tag>`*`)`](https://geopressure.org/GeoPressureR/reference/plot.tag.md)
  :

  Plot a `tag` object

- [`plot_tag_pressure()`](https://geopressure.org/GeoPressureR/reference/plot_tag_pressure.md)
  :

  Plot pressure data of a `tag`

- [`plot_tag_light()`](https://geopressure.org/GeoPressureR/reference/plot_tag_light.md)
  :

  Plot light data of a `tag`

- [`plot_tag_twilight()`](https://geopressure.org/GeoPressureR/reference/plot_tag_twilight.md)
  :

  Plot twilight data of a `tag`

- [`plot_tag_acceleration()`](https://geopressure.org/GeoPressureR/reference/plot_tag_acceleration.md)
  :

  Plot acceleration data of a `tag`

- [`plot_tag_temperature()`](https://geopressure.org/GeoPressureR/reference/plot_tag_temperature.md)
  :

  Plot temperature data of a `tag`

- [`plot_tag_actogram()`](https://geopressure.org/GeoPressureR/reference/plot_tag_actogram.md)
  :

  Plot Actogram data of a `tag`

### Label tag

Core functions related to labelling a `tag` object.

- [`tag_label()`](https://geopressure.org/GeoPressureR/reference/tag_label.md)
  :

  Label a `tag` object

- [`tag_label_auto()`](https://geopressure.org/GeoPressureR/reference/tag_label_auto.md)
  :

  Automatic labelling acceleration data of a `tag`

- [`tag_label_write()`](https://geopressure.org/GeoPressureR/reference/tag_label_write.md)
  : Write a tag label file

- [`tag_label_read()`](https://geopressure.org/GeoPressureR/reference/tag_label_read.md)
  : Read a tag label file

- [`tag_label_stap()`](https://geopressure.org/GeoPressureR/reference/tag_label_stap.md)
  : Create stationary periods from a tag label

### Tag utilities

Various utility functions related to a `tag` object.

- [`tag_assert()`](https://geopressure.org/GeoPressureR/reference/tag_assert.md)
  :

  Assert the status of a `tag`

- [`read_stap()`](https://geopressure.org/GeoPressureR/reference/read_stap.md)
  : Read or validate stap data

- [`tag_stap_daily()`](https://geopressure.org/GeoPressureR/reference/tag_stap_daily.md)
  : Create stationary periods from twilight midpoints

- [`tag2path()`](https://geopressure.org/GeoPressureR/reference/tag2path.md)
  :

  Build a `path` from the likelihood maps of a `tag`

## GeoPressure

Core functions of the
[GeoPressureAPI](https://github.com/GeoPressure/GeoPressureAPI).

### GeoPressure map

Geopositioning based on pressure data.

- [`geopressure_map()`](https://geopressure.org/GeoPressureR/reference/geopressure_map.md)
  [`geopressure_map_likelihood()`](https://geopressure.org/GeoPressureR/reference/geopressure_map.md)
  [`geopressure_map_mismatch()`](https://geopressure.org/GeoPressureR/reference/geopressure_map.md)
  : Compute likelihood map from pressure data

- [`geopressure_map_preprocess()`](https://geopressure.org/GeoPressureR/reference/geopressure_map_preprocess.md)
  :

  Prepare pressure data for
  [`geopressure_map()`](https://geopressure.org/GeoPressureR/reference/geopressure_map.md)

### GeoPressure time series

Retrieving time series pressure at a given location.

- [`geopressure_timeseries()`](https://geopressure.org/GeoPressureR/reference/geopressure_timeseries.md)
  : Request and download pressure time series at a given location

## GeoLight

Geopositioning based on light data.

- [`twilight_create()`](https://geopressure.org/GeoPressureR/reference/twilight_create.md)
  : Estimate twilights from light data
- [`twilight_label_write()`](https://geopressure.org/GeoPressureR/reference/twilight_label_write.md)
  : Write a twilight label file
- [`twilight_label_read()`](https://geopressure.org/GeoPressureR/reference/twilight_label_read.md)
  : Read a twilight label file
- [`geolight_solar()`](https://geopressure.org/GeoPressureR/reference/geolight_solar.md)
  : Solar zenith angle using NOAA / Meeus equations
- [`geolight_map()`](https://geopressure.org/GeoPressureR/reference/geolight_map.md)
  [`geolight_map_aggregate()`](https://geopressure.org/GeoPressureR/reference/geolight_map.md)
  [`geolight_map_calibrate()`](https://geopressure.org/GeoPressureR/reference/geolight_map.md)
  [`geolight_map_likelihood()`](https://geopressure.org/GeoPressureR/reference/geolight_map.md)
  : Compute likelihood map from twilight
- [`geolight_fit_location()`](https://geopressure.org/GeoPressureR/reference/geolight_fit_location.md)
  : Estimate one location per stationary period from twilight times
- [`plot_twl_calib()`](https://geopressure.org/GeoPressureR/reference/plot_twl_calib.md)
  [`plot_twl_calib_path()`](https://geopressure.org/GeoPressureR/reference/plot_twl_calib.md)
  : Plot twilight calibration diagnostics
- [`ts2mat()`](https://geopressure.org/GeoPressureR/reference/ts2mat.md)
  : Format time series data frame into a matrix

## Graph

Constructing the Hidden-Markov model with a `graph` and computing
trajectory products.

- [`graph_create()`](https://geopressure.org/GeoPressureR/reference/graph_create.md)
  :

  Create a `graph` object

### Graph movement

Defining the movement model, optionally using wind data and bird
morphology.

- [`graph_set_movement()`](https://geopressure.org/GeoPressureR/reference/graph_set_movement.md)
  :

  Define the movement model of a `graph`

- [`tag_download_wind()`](https://geopressure.org/GeoPressureR/reference/tag_download_wind.md)
  : Download flight data

- [`graph_add_wind()`](https://geopressure.org/GeoPressureR/reference/graph_add_wind.md)
  :

  Compute windspeed and airspeed on a `graph`

- [`bird_create()`](https://geopressure.org/GeoPressureR/reference/bird_create.md)
  : Create bird flight traits

- [`print(`*`<bird>`*`)`](https://geopressure.org/GeoPressureR/reference/print.bird.md)
  :

  Print a `bird` object

- [`speed2prob()`](https://geopressure.org/GeoPressureR/reference/speed2prob.md)
  : Compute probability of a bird speed

- [`edge_add_wind()`](https://geopressure.org/GeoPressureR/reference/edge_add_wind.md)
  : Retrieve ERA5 variable along edge

- [`plot_graph_movement()`](https://geopressure.org/GeoPressureR/reference/plot_graph_movement.md)
  :

  Plot movement model of a `graph`

### Graph products

Compute the three main products of the Hidden-Markov model.

- [`graph_most_likely()`](https://geopressure.org/GeoPressureR/reference/graph_most_likely.md)
  : Compute the most likely trajectory
- [`graph_marginal()`](https://geopressure.org/GeoPressureR/reference/graph_marginal.md)
  : Compute marginal probability map
- [`graph_simulation()`](https://geopressure.org/GeoPressureR/reference/graph_simulation.md)
  : Simulate randomly multiple trajectories

### Graph utilities

Utility functions for a `graph` object

- [`graph_assert()`](https://geopressure.org/GeoPressureR/reference/graph_assert.md)
  :

  Assert the status of a `graph`

- [`print(`*`<graph>`*`)`](https://geopressure.org/GeoPressureR/reference/print.graph.md)
  :

  Print a `graph` object

## Map

Container for spatio-temporal (stationary periods) data.

- [`map_create()`](https://geopressure.org/GeoPressureR/reference/map_create.md)
  :

  Create a `map` object

- [`print(`*`<map>`*`)`](https://geopressure.org/GeoPressureR/reference/print.map.md)
  :

  Print a `map` object

- [`plot(`*`<map>`*`)`](https://geopressure.org/GeoPressureR/reference/plot.map.md)
  :

  Plot a `map` object

- [`rast.map()`](https://geopressure.org/GeoPressureR/reference/rast.map.md)
  :

  Construct a SpatRaster from a `map`

- [`map_expand()`](https://geopressure.org/GeoPressureR/reference/map_expand.md)
  :

  Construct grid from `extent` and `scale`

- [`map_add_mask_water()`](https://geopressure.org/GeoPressureR/reference/map_add_mask_water.md)
  : Create water mask for geolight map

## Path

Data.frame of positions defining a bird trajectory.

- [`path2edge()`](https://geopressure.org/GeoPressureR/reference/path2edge.md)
  :

  Extract the edges of a `path` from a `graph`

- [`plot_path()`](https://geopressure.org/GeoPressureR/reference/plot_path.md)
  :

  Plot a `path`

## Sampling path

Gibbs sampling from movement model.

- [`tag2path()`](https://geopressure.org/GeoPressureR/reference/tag2path.md)
  :

  Build a `path` from the likelihood maps of a `tag`

## Pressurepath

Data.frame of pressure time series along a path.

- [`pressurepath_create()`](https://geopressure.org/GeoPressureR/reference/pressurepath_create.md)
  :

  Create a `pressurepath`

- [`plot_pressurepath()`](https://geopressure.org/GeoPressureR/reference/plot_pressurepath.md)
  :

  Plot a `pressurepath`

- [`path2elevation()`](https://geopressure.org/GeoPressureR/reference/path2elevation.md)
  : Download ground elevation along a path

## GeoPressureTemplate

Wrapper functions for the full GeoPressureR worflow

- [`geopressuretemplate()`](https://geopressure.org/GeoPressureR/reference/geopressuretemplate.md)
  [`geopressuretemplate_config()`](https://geopressure.org/GeoPressureR/reference/geopressuretemplate.md)
  [`geopressuretemplate_graph()`](https://geopressure.org/GeoPressureR/reference/geopressuretemplate.md)
  [`geopressuretemplate_pressurepath()`](https://geopressure.org/GeoPressureR/reference/geopressuretemplate.md)
  [`geopressuretemplate_tag()`](https://geopressure.org/GeoPressureR/reference/geopressuretemplate.md)
  : Workflow for GeoPressureR
- [`load_interim()`](https://geopressure.org/GeoPressureR/reference/load_interim.md)
  : Load interim objects from an RData file

## Shiny Apps

Web applications for labeling and visualizing data.

- [`geopressureviz()`](https://geopressure.org/GeoPressureR/reference/geopressureviz.md)
  : Start the GeoPressureViz shiny app
- [`geolightviz()`](https://geopressure.org/GeoPressureR/reference/geolightviz.md)
  : Start the GeoLightViz shiny app
- [`trainset()`](https://geopressure.org/GeoPressureR/reference/trainset.md)
  : Start the GeoPressure Trainset shiny app

## Param

List of parameters used in `tag` and `graph`.

- [`param_create()`](https://geopressure.org/GeoPressureR/reference/param_create.md)
  :

  Create a `param` list

- [`print(`*`<param>`*`)`](https://geopressure.org/GeoPressureR/reference/print.param.md)
  :

  Print a `param` list

## Utilities

General utility functions of the package.

- [`stap2flight()`](https://geopressure.org/GeoPressureR/reference/stap2flight.md)
  : Compute flights from stationary periods

- [`stap2duration()`](https://geopressure.org/GeoPressureR/reference/stap2duration.md)
  : Compute duration of stationary periods

- [`speed2bearing()`](https://geopressure.org/GeoPressureR/reference/speed2bearing.md)
  : Compute the bearing of a speed vector

- [`windsupport()`](https://geopressure.org/GeoPressureR/reference/windsupport.md)
  : Compute wind support and drift from a wind and ground speed vectors

- [`path2twilight()`](https://geopressure.org/GeoPressureR/reference/path2twilight.md)
  :

  Compute exact astronomical twilights from a `path` (positions and
  dates)

- [`GeoPressureR`](https://geopressure.org/GeoPressureR/reference/GeoPressureR.md)
  : GeoPressureR: Global positioning by atmospheric pressure.
