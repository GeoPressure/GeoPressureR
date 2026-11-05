#' Workflow for GeoPressureR
#'
#' @description
#' The `geopressuretemplate()` function manages the complete workflow for modelling bird
#' trajectories using geolocator data. It includes the creation of the `tag` object, the construction of
#' the likelihood maps (pressure and/or light), the construction of the graph model, and,
#' finally, the estimation of the trajectory products and the pressure path computation. Read the
#' [Workflow chapter in the GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/geopressuretemplate-workflow.html) for more
#' information.
#'
#' @details
#' The `geopressuretemplate` function is a high-level entry point that coordinates multiple
#' steps for processing the geolocator data and producing trajectories. It relies on underlying child
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
#'     - `twilight_create() |> twilight_label_read() |> geolight_map()`: Computes the light likelihood.
#' 6. `save()`: Saves `tag` and `param` in `data/interim/{id}.RData`, replacing the content of the
#' file.
#'
#' * **Graph Creation [`geopressuretemplate_graph()`]**: Builds a movement model graph
#' based on `tag` data, and can include wind effects if specified. Outputs such as
#' marginal distributions, most likely paths, and simulation paths can be computed.
#'
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
#' 7. `save()`: Saves the `tag`, `param` and computed outputs in `data/interim/{id}.RData`. Other
#' objects already in the file and not derived from the graph (e.g. `path_geopressureviz`) are
#' kept; outputs derived from a previous graph (paths, edges, marginal and their pressurepaths)
#' are replaced or removed.
#'
#' * **Pressure Path Processing [`geopressuretemplate_pressurepath()`]**: Computes pressurepaths
#' (`pressurepath_create()`) using the content of the `.RData` file and appending the pressurepath
#' data.frames to the same file.
#'
#' For each of `"most_likely"`, `"simulation"`, `"geopressureviz"` and `"tag"` listed in
#' `config$geopressuretemplate$pressurepath`, computes `pressurepath_{type}` from `path_{type}`
#' and saves it in the same file. `path_geopressureviz` is read from the file or, if absent, from
#' the CSV exported by GeoPressureViz (`{id}-path-geopressureviz.csv` in the directory of `file`,
#' i.e. `data/interim/` by default). `path_tag` is
#' a path you saved yourself (e.g. from `tag2path()`). A warning is given when a path is missing.
#'
#' Each of these child functions can be called individually or automatically as part of
#' the `geopressuretemplate` workflow.
#'
#' @param id unique identifier of a tag.
#' @param config configuration object specifying workflow parameters, which is loaded by default
#' using `config::get(config = id)`.
#' @param quiet Logical. If `TRUE`, suppresses informational messages during execution. The default
#' value is `FALSE`.
#' @param file A file path to save the intermediate results (e.g., tag, graph, and pressure paths).
#' Default is `./data/interim/{id}.RData`.
#' @param assert_tag Logical. If `TRUE`, check that the config is compatible for the creation of
#' a tag. The default value is `TRUE`.
#' @param assert_graph Logical. If `TRUE`, check that the config is compatible for the creation of
#' a graph. Only used by `geopressuretemplate_config()`, where the default value is `FALSE`
#' (`geopressuretemplate_graph()` always sets it to `TRUE`).
#' @param ... Additional parameters to overwrite default or config values. Always prefer to modify
#' `config.yml` if possible.
#'
#'
#' @return
#' `geopressuretemplate()`, `geopressuretemplate_graph()` and `geopressuretemplate_pressurepath()`
#' return `file` invisibly and save the processed outputs (tag, paths, pressure paths, etc.) to the
#' specified `file`. If the movement model or the outputs fail, `geopressuretemplate_graph()`
#' returns the `graph` computed so far instead, without saving. `geopressuretemplate_tag()` returns
#' the `tag` invisibly and `geopressuretemplate_config()` returns the merged configuration list.
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
#' geopressuretemplate_config("18LX")
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
  config = config::get(config = id),
  quiet = FALSE,
  file = glue::glue("./data/interim/{id}.RData"),
  ...
) {
  if (!quiet) {
    cli::cli_h1("Running geopressuretemplate for {id}")
  }

  # Create the tag
  geopressuretemplate_tag(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  # Create and process the graph using the computed or loaded tag
  geopressuretemplate_graph(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  geopressuretemplate_pressurepath(
    id = id,
    config = config,
    quiet = quiet,
    file = file,
    ...
  )

  invisible(file)
}
