#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_graph <- function(
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
    return(invisible(geopressuretemplate_graph_scalar(
      id = inputs$id[[1]],
      config = inputs$config[[1]],
      quiet = quiet,
      file = inputs$file[[1]],
      ...
    )))
  }

  if (!quiet) {
    cli::cli_h1("Running geopressuretemplate_graph for {.val {length(inputs$id)}} tags")
  }

  out <- unlist(
    lapply(seq_along(inputs$id), function(i) {
      geopressuretemplate_graph_scalar(
        id = inputs$id[[i]],
        config = inputs$config[[i]],
        quiet = quiet,
        file = inputs$file[[i]],
        ...
      )
    }),
    use.names = FALSE
  )
  names(out) <- inputs$id

  invisible(out)
}

#' @noRd
geopressuretemplate_graph_scalar <- function(id, config, quiet, file, ...) {
  file_env <- new.env(parent = emptyenv())
  load(file, envir = file_env)
  tag <- get("tag", envir = file_env, inherits = FALSE)
  rm(file_env)
  gc()

  if (tag$param$id != id) {
    cli::cli_abort(c(
      x = "{.var id}={id} is different from {.var tag$param$id}={tag$param$id}."
    ))
  }

  tag_assert(tag)

  config <- geopressuretemplate_config(
    id,
    config = config,
    assert_graph = TRUE,
    ...
  )

  if (!all(config$geopressuretemplate$likelihood %in% names(tag))) {
    cli::cli_abort(c(
      x = "{.var geopressuretemplate$likelihood}={.val {config$geopressuretemplate$likelihood}}
      {?is/are} not present{?s} in {.var tag}."
    ))
  }

  # Create the geospatial graph using the provided or default parameters
  if (!quiet) {
    cli::cli_h2("Create Graph {.field {id}}")
  }
  graph <- do.call(
    graph_create,
    c(
      list(
        tag = tag,
        quiet = quiet
      ),
      config$graph_create
    )
  )
  gc()

  # Set the movement model based on the configuration
  tryCatch(
    {
      if (eval(config$graph_set_movement$type) == "gs") {
        # Without wind speed
        graph <- do.call(
          graph_set_movement,
          c(
            list(graph = graph),
            config$graph_set_movement
          )
        )
      } else {
        if (!quiet) {
          cli::cli_h2("Add wind to graph")
        }
        # With wind speed
        graph <- do.call(
          graph_add_wind,
          c(
            list(
              graph = graph,
              pressure = tag$pressure,
              quiet = quiet
            ),
            config$graph_add_wind
          )
        )

        bird <- do.call(bird_create, config$bird_create)

        graph <- do.call(
          graph_set_movement,
          c(
            list(
              graph = graph,
              bird = bird
            ),
            config$graph_set_movement
          )
        )
      }
    },
    error = function(e) {
      cli::cli_bullets(c(
        "x" = "{e$message}",
        "i" = "Error while defining the movement model. {.var graph} is return.",
        ">" = "Debug line by line by opening {.code edit(geopressuretemplate_graph)}"
      ))
      # Need to have return otherwise this is not returning to main function
      return(graph)
    }
  )

  # Store the graph parameters
  param <- graph$param

  # Initialize a list to keep track of outputs to be saved
  save_list <- c("tag", "param")

  tryCatch(
    {
      # Compute the marginal distribution if requested
      if ("marginal" %in% config$geopressuretemplate$outputs) {
        if (!quiet) {
          cli::cli_h2("Compute marginal map")
        }
        marginal <- graph_marginal(graph, quiet = quiet)
        save_list <- c(save_list, "marginal")
      }

      # Compute the most likely path if requested
      if ("most_likely" %in% config$geopressuretemplate$outputs) {
        if (!quiet) {
          cli::cli_h2("Compute most likely path")
        }
        path_most_likely <- graph_most_likely(graph, quiet = quiet)
        edge_most_likely <- path2edge(path_most_likely, graph)
        save_list <- c(save_list, "path_most_likely", "edge_most_likely")
      }

      # Compute simulations if requested
      if ("simulation" %in% config$geopressuretemplate$outputs) {
        if (!quiet) {
          cli::cli_h2("Compute simulation paths")
        }
        path_simulation <- graph_simulation(
          graph,
          nj = config$graph_simulation$nj,
          quiet = quiet
        )
        edge_simulation <- path2edge(path_simulation, graph)
        save_list <- c(save_list, "path_simulation", "edge_simulation")
      }
    },
    error = function(e) {
      cli::cli_bullets(c(
        "x" = "{e$message}",
        "x" = "Error while computing the outputs. {.var graph} is returned.",
        ">" = "Debug line by line by opening {.code edit(geopressuretemplate_graph)}"
      ))
      # Need to have return otherwise this is not returning to main function
      return(graph)
    }
  )

  # Save the outputs to the specified file
  save(
    list = save_list,
    file = file
  )

  # Return the file path invisibly
  file
}
