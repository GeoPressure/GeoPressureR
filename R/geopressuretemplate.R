#' Workflow for GeoPressureR
#'
#' @description
#' The `geopressuretemplate()` function manages the complete workflow for modelling bird
#' trajectories using geolocator data. It includes the creation `tag` object, the construction of
#' the likelihood maps (pressure and/or light), the creation of the graph model construction, and,
#' finally, the estimation of the trajectory products and the pressure path computation. Read the
#' [Workflow chapter in the GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/geopressuretemplate-workflow.html) for more
#' information.
#'
#' @details
#' The `geopressuretemplate` function is a high-level entry point that coordinates multiple
#' steps for processing the geolocator data and produce trajectories. It relies on underlying child
#' functions for each step:
#'
#' * **Tag Creation [`geopressuretemplate_tag()`]**: Initializes and labels the `tag` object. It
#' also generates light and pressure likelihood maps:
#'
#' 1. `tag_create()`: Initializes the tag object.
#' 2. `tag_label()`: Adds labels.
#' 3. `tag_set_map()`: Sets the spatial and temporal parameters.
#' 4. If `"map_pressure"` is in the `config$geopressuretemplate$likelihood`:
#'     - `geopressure_map()`: Computes the pressure likelihood.
#' 5. If `"map_light"` is in the `config$geopressuretemplate$likelihood`:
#'     - `twilight_create() |> twilight_read() |> geolight_map()`: Computes the light likelihood.
#'
#' * **Graph Creation [`geopressuretemplate_graph()`]**: Builds a movement model graph
#' based `tag` data, and can include wind effects if specified. Outputs such as
#' marginal distributions, most likely paths, and simulation paths can be computed.

#' 1. `graph_create()`: Creates the graph based on tag.
#' 2. If `config$graph_set_movement$type == "gs"` (i.e., no wind):
#'     - `graph_set_movement()`: Sets the movement model without wind.
#' 3. If `config$graph_set_movement$type == "as"` (i.e., with wind):
#'     - `graph_add_wind()`: Adds wind data to the graph.
#'     - `graph_set_movement()`: Sets the movement model with wind.
#' 4. If `"marginal"` is in `config$geopressuretemplate$outputs`:
#'     - `graph_marginal()`: Computes the marginal distribution map.
#' 5. If `"most_likely"` is in `config$geopressuretemplate$outputs`:
#'     - `graph_most_likely()`: Computes the most likely path based on the movement model.
#' 6. If `"simulation"` is in `config$geopressuretemplate$outputs`:
#'     - `graph_simulation()`: Runs simulations to model multiple possible paths.
#' 7. `save()`: Saves the computed graph and associated objects in `data/interim/{id}.Rdata`
#'
#' * **Pressure Path Processing [`geopressuretemplate_pressurepath()`]**: Computes pressurepaths
#' (`pressurepath_create`) using the content of the `Rdata` file and appending the pressurepath
#' data.frame to the same file.
#'
#' 1. If `"most_likely"` is in `config$geopressuretemplate$pressurepath`, computes the pressure path
#' for `path_most_likely`.
#' 2. If `"geopressureviz"` is in `config$geopressuretemplate$pressurepath` computes the pressure
#' path for `path_geopressureviz`
#'
#' Each of these child functions can be called individually or automatically as part of
#' the `geopressuretemplate` workflow.
#'
#' @param id unique identifier of a tag. For the workflow functions, this can also be a character
#' vector of tag identifiers. For `geopressuretemplate_status()`, if `NULL`, all tag identifiers
#' found in `config.yml` and `data/interim` are reported.
#' @param config configuration object specifying workflow parameters. If omitted, each tag uses
#' `config::get(config = id_i)`. For vector `id`, supply a list of configuration objects with the
#' same length as `id`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages during execution. The default
#' value is `FALSE`.
#' @param file A file path to save the intermediate results (e.g., tag, graph, and pressure paths).
#' If omitted, each tag uses `./data/interim/{id}.RData`. For vector `id`, supply a character
#' vector of file paths with the same length as `id`.
#' @param assert_tag Logical. If `TRUE`, check that the config is compatible for the creation of
#' a tag. The default value is `TRUE`.
#' @param assert_graph Logical. If `TRUE`, check that the config is compatible for the creation of
#' a graph. The default value is `TRUE`. Set to `FALSE` only if you don't want to create a graph
#' model
#' @param config_file Path to the `config.yml` file used by `geopressuretemplate_status()`.
#' @param ... Additional parameters to overwrite default or config values. Always prefer to modify
#' `config.yml` if possible.
#'
#'
#' @return
#' `geopressuretemplate()`, `geopressuretemplate_graph()` and
#' `geopressuretemplate_pressurepath()` return the saved `file` path invisibly for scalar `id`, or
#' a named character vector of saved file paths for vector `id`.
#'
#' `geopressuretemplate_tag()` returns the created `tag` invisibly for scalar `id`, or a named list
#' of `tag` objects for vector `id`.
#'
#' `geopressuretemplate_config()` returns a single configuration object.
#'
#' `geopressuretemplate_status()` returns a `data.frame` with one row per tag and columns
#' describing the requested workflow, available interim artifacts, missing artifacts, and next
#' suggested workflow step.
#'
#' @examplesIf FALSE
#' # Run the complete geopressuretemplate workflow
#' geopressuretemplate("18LX")
#'
#' # Overwrite parameters passed through ...
#' geopressuretemplate("18LX",
#'   tag_set_map = list(scale = 2),
#'   geopressure_map = list(max_sample = 100, margin = 20)
#' )
#'
#' # Or run step-by-step
#' # you can check that all the parameters are correctly set in the config file
#' geopressuretemplate_config(id)
#' # 1. creation of the tag
#' tag <- geopressuretemplate_tag("18LX")
#' # 2. creation of the graph
#' geopressuretemplate_graph("18LX")
#' # 3. Computation of the pressurepath
#' geopressuretemplate_pressurepath("18LX")
#'
#' @family geopressuretemplate
#' @seealso [GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/geopressuretemplate-workflow.html)
#' @export
geopressuretemplate <- function(
  id,
  config = NULL,
  quiet = FALSE,
  file = NULL,
  ...
) {
  inputs <- geopressuretemplate_normalize_inputs(
    id = id,
    config = config,
    config_missing = missing(config),
    file = file,
    file_missing = missing(file)
  )

  if (inputs$scalar) {
    return(invisible(geopressuretemplate_scalar(
      id = inputs$id[[1]],
      config = inputs$config[[1]],
      quiet = quiet,
      file = inputs$file[[1]],
      show_heading = TRUE,
      ...
    )))
  }

  if (!quiet) {
    cli::cli_h1("Running geopressuretemplate for {.val {length(inputs$id)}} tags")
  }

  out <- unlist(
    lapply(seq_along(inputs$id), function(i) {
      if (!quiet) {
        cli::cli_h2("Run geopressuretemplate for {.field {inputs$id[[i]]}}")
      }
      geopressuretemplate_scalar(
        id = inputs$id[[i]],
        config = inputs$config[[i]],
        quiet = quiet,
        file = inputs$file[[i]],
        show_heading = FALSE,
        ...
      )
    }),
    use.names = FALSE
  )
  names(out) <- inputs$id

  invisible(out)
}

#' @noRd
geopressuretemplate_scalar <- function(id, config, quiet, file, show_heading = TRUE, ...) {
  if (!quiet && show_heading) {
    cli::cli_h1("Running geopressuretemplate for {id}")
  }

  geopressuretemplate_tag_scalar(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  geopressuretemplate_graph_scalar(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  geopressuretemplate_pressurepath_scalar(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  file
}

#' @noRd
geopressuretemplate_normalize_inputs <- function(
  id,
  config,
  config_missing,
  file,
  file_missing
) {
  if (!is.character(id) || length(id) == 0 || any(is.na(id))) {
    cli::cli_abort("{.arg id} must be a non-missing character vector.")
  }

  scalar <- length(id) == 1

  config_out <- if (config_missing) {
    lapply(id, function(id_i) config::get(config = id_i))
  } else if (scalar) {
    list(config)
  } else {
    if (!is.list(config) || length(config) != length(id)) {
      cli::cli_abort(
        "For vector {.arg id}, {.arg config} must be a list with the same length as {.arg id}."
      )
    }
    config
  }

  file_out <- if (file_missing) {
    glue::glue("./data/interim/{id}.RData")
  } else {
    if (!is.character(file) || length(file) != length(id)) {
      cli::cli_abort(
        "For vector {.arg id}, {.arg file} must be a character vector with the same length as {.arg id}."
      )
    }
    file
  }

  list(
    id = id,
    config = unname(config_out),
    file = unname(file_out),
    scalar = scalar
  )
}
