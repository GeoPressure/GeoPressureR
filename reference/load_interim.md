# Load interim objects from an RData file

Loads one or more objects from an interim `.RData` file created during
the GeoPressure analysis. This is a convenience function to quickly
restore saved objects (e.g., `tag`, `graph`, `path_most_likely`) for a
given tag `id` or directly from a file path.

## Usage

``` r
load_interim(
  x,
  var = NULL,
  var_optional = NULL,
  envir = parent.frame(),
  verbose = FALSE
)
```

## Arguments

- x:

  A character string of length 1. If `x` is the path to an existing
  `.RData`/`.rda` file, that file is used. Otherwise, `x` is interpreted
  as a tag identifier and the function looks for
  `./data/interim/{x}.RData`.

- var:

  Optional name (or names) of objects that must exist in the file. If
  `NULL` and `var_optional` is also `NULL`, all objects from the file
  are loaded into `envir` (the behaviour of
  [`base::load()`](https://rdrr.io/r/base/load.html)).

- var_optional:

  Character vector of object names that may or may not exist in the
  file. Any of these that are present will be loaded; missing ones are
  silently ignored.

- envir:

  the environment where the data should be loaded.

- verbose:

  should item names be printed during loading?

## Value

If no selection is requested (`var` and `var_optional` both `NULL`),
invisibly returns the character vector of object names loaded (as in
[`base::load()`](https://rdrr.io/r/base/load.html)). Otherwise, if a
single object name is requested, invisibly returns that object; if
multiple names are requested, invisibly returns a named list of those
objects.

## Examples

``` r
if (FALSE) {
  load_interim("18LX", var = c("tag", "graph"))
}
```
