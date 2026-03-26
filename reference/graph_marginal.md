# Compute marginal probability map

Compute the marginal probability map from a graph. The computation uses
the [forward backward
algorithm](https://en.wikipedia.org/wiki/Forward%E2%80%93backward_algorithm).
For more details, see [section 2.3.2 of Nussbaumer et al.
(2023b)](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0012-title)
and the
[GeoPressureManual](https://geopressure.org/GeoPressureManual/trajectory.html#product-2-marginal-probability-map).

## Usage

``` r
graph_marginal(graph, quiet = FALSE)
```

## Arguments

- graph:

  a GeoPressureR `graph` with defined movement model
  [`graph_set_movement()`](https://geopressure.org/GeoPressureR/reference/graph_set_movement.md).

- quiet:

  logical to hide messages about the progress.

## Value

A list of the marginal maps for each stationary period (even those not
modelled). Best to include within `tag`.

## References

Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and
Daniel Sheldon. 2023. Reconstructing bird trajectories from pressure and
wind data using a highly optimized hidden Markov model. *Methods in
Ecology and Evolution*, 14, 1118–1129
[doi:10.1111/2041-210X.14082](https://doi.org/10.1111/2041-210X.14082) .

## See also

[GeoPressureManual](https://geopressure.org/GeoPressureManual/trajectory.html#product-2-marginal-probability-map)

Other graph:
[`graph_add_wind()`](https://geopressure.org/GeoPressureR/reference/graph_add_wind.md),
[`graph_create()`](https://geopressure.org/GeoPressureR/reference/graph_create.md),
[`graph_most_likely()`](https://geopressure.org/GeoPressureR/reference/graph_most_likely.md),
[`graph_set_movement()`](https://geopressure.org/GeoPressureR/reference/graph_set_movement.md),
[`graph_simulation()`](https://geopressure.org/GeoPressureR/reference/graph_simulation.md),
[`print.graph()`](https://geopressure.org/GeoPressureR/reference/print.graph.md)

## Examples

``` r
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE) |>
    tag_label(quiet = TRUE) |>
    twilight_create() |>
    twilight_label_read() |>
    tag_set_map(
      extent = c(-16, 23, 0, 50),
      known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
    ) |>
    geopressure_map(quiet = TRUE) |>
    geolight_map(quiet = TRUE)
})
#> Warning: UNRELIABLE VALUE: One of the ‘future.apply’ iterations (‘future_lapply-2’) unexpectedly generated random numbers without declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be invalid. To fix this, specify 'future.seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced via a parallel RNG method. To disable this check, use 'future.seed = NULL', or set option 'future.rng.onMisuse' to "ignore". [future ‘future_lapply-2’ (3c41cc4a119dff972f50a41e8ef03f88-10); on 3c41cc4a119dff972f50a41e8ef03f88@runnervm46oaq<8505>]
#> Warning: Caught httr2_http_503. Canceling all iterations ...
#> Warning: UNRELIABLE VALUE: Future (‘future_lapply-2’) unexpectedly generated random numbers without specifying argument 'seed'. There is a risk that those random numbers are not statistically sound and the overall results might be invalid. To fix this, specify 'seed=TRUE'. This ensures that proper, parallel-safe random numbers are produced. To disable this check, use 'seed=NULL', or set option 'future.rng.onMisuse' to "ignore". [future ‘future_lapply-2’ (3c41cc4a119dff972f50a41e8ef03f88-10); on 3c41cc4a119dff972f50a41e8ef03f88@runnervm46oaq<8505>]
#> Error in httr2::req_perform(req_i, path = file): HTTP 503 Service Unavailable.

# Create graph
graph <- graph_create(tag, quiet = TRUE)
#> Error: object 'tag' not found

# Define movement model
graph <- graph_set_movement(graph)
#> Error: object 'graph' not found

# Compute marginal
marginal <- graph_marginal(graph)
#> Error: object 'graph' not found

plot(marginal)
#> Error: object 'marginal' not found
```
