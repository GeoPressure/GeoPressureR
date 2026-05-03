# Memory Optimization For `graph_add_wind()`

`graph_add_wind()` can be run on very large graphs, sometimes with tens of millions of edges for a
single stationary-period transition. In that situation, peak memory is the main limitation.

The previous implementation reused the general `edge_add_wind()` extraction path. That was
convenient, but it meant that graph-scale wind computation could temporarily allocate large
intermediate objects before reducing them to the final average wind vector.

For a transition with:

```r
number_of_edges_in_stap * number_of_query_times
```

the old implementation could allocate time-by-edge objects. This becomes very expensive when a
single source stationary period has millions of possible edges, and it becomes worse for long
flights because the number of query times increases.

For example, with 30 million edges and 10 hourly query times, a single numeric matrix is already
around:

```r
30e6 * 10 * 8 bytes = 2.4 GB
```

Several such objects may be needed during interpolation and averaging. This can make a final graph
that is only a few GB require much more peak memory during `graph_add_wind()`.

## Main Optimization

The optimized graph path avoids storing values for all query times at once.

Instead of building objects shaped like:

```r
number_of_edges_in_stap x number_of_query_times
```

it processes one query time at a time while still keeping vectorized operations over all edges in
the current source stationary-period group.

The graph-scale loop is now conceptually:

```r
for each source stationary-period edge group:
  for each flight segment:
    for each query time:
      compute positions for all edges in this group
      read/interpolate ERA5 u/v for these positions
      accumulate weighted mean wind
```

So the working vectors are mostly proportional to:

```r
number_of_edges_in_stap
```

rather than:

```r
number_of_edges_in_stap * number_of_query_times
```

This is especially helpful for long flights, where the old implementation had to hold multiple
hourly query times in memory at once.

## What Is Still Vectorized

The optimized implementation does **not** process one edge at a time.

For a source stationary period with millions of edges, each query time is still processed as vectors
of length `length(st_id)`, where `st_id` contains all edges starting from that stationary period.

This keeps the expensive numerical operations vectorized while avoiding the largest temporary
time-by-edge matrices.

## Other Memory Improvements

The graph path now avoids several additional temporary objects:

- no detailed wind `data.frame` is created;
- no temporary averaged `u/v` matrix is created before `graph$ws`;
- no full `lat_int` / `lon_int` matrices are created across all query times;
- graph edge indices are decoded directly instead of using the public edge preparation helper with
  full validation;
- decoded edge indices are stored as integer matrices;
- when `thr_as = Inf`, graph pruning is skipped entirely, avoiding copies of `graph$s`, `graph$t`,
  `graph$gs`, and `graph$ws`.

The implementation also preserves matrix dimensions after NetCDF time and pressure interpolation.
This avoids accidental dimension dropping when a NetCDF read covers only one latitude or longitude
cell.

## Expected Impact

For large graphs, the main expected benefit is lower peak memory and less garbage collection. This
should make `graph_add_wind()` more robust on large datasets and reduce the chance of hitting R's
vector memory limit.

Runtime may be mixed:

- small graphs may not be faster because the previous implementation used larger vectorized
  matrices;
- large graphs should benefit from reduced allocation and lower garbage-collection pressure;
- the main goal is to make graph-scale wind computation feasible when memory is the limiting
  factor.

## Code Changes

`graph_add_wind()` keeps the same public signature:

```r
graph_add_wind(graph, thr_as = Inf, variable = c("u", "v"), ...)
```

It now uses a private graph-scale helper:

```r
add_wind_graph_edge()
```

This helper only supports ERA5 wind components `u` and `v`, and returns wind as a complex vector in
km/h:

```r
(u + 1i * v) * 3.6
```

The public graph output is unchanged: `graph$ws` remains a complex wind vector aligned with
`graph$s`, `graph$t`, and `graph$gs`.

`edge_add_wind()` remains the public function for detailed edge-level extraction. It now always
returns detailed edge/time/variable values as a `data.frame`. The old `return_averaged_variable`
argument is deprecated with `lifecycle`; graph-scale averaged wind is now handled by
`graph_add_wind()`.
