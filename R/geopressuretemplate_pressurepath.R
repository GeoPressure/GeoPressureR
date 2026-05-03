#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_pressurepath <- function(
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
    return(invisible(geopressuretemplate_pressurepath_scalar(
      id = inputs$id[[1]],
      config = inputs$config[[1]],
      quiet = quiet,
      file = inputs$file[[1]],
      ...
    )))
  }

  if (!quiet) {
    cli::cli_h1("Running geopressuretemplate_pressurepath for {.val {length(inputs$id)}} tags")
  }

  out <- unlist(
    lapply(seq_along(inputs$id), function(i) {
      geopressuretemplate_pressurepath_scalar(
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
geopressuretemplate_pressurepath_scalar <- function(id, config, quiet, file, ...) {
  config <- geopressuretemplate_config(id, config = config, ...)

  if ("pressurepath" %in% names(config$geopressuretemplate)) {
    if (!quiet) {
      cli::cli_h2("Compute pressurepath {.field {id}}")
    }

    save_list <- load(file)

    tag <- get("tag")

    tag_assert(tag)

    if (tag$param$id != id) {
      cli::cli_abort(c(
        x = "{.var id}={id} is different from {.var tag$param$id}={tag$param$id}."
      ))
    }

    if (
      "most_likely" %in%
        config$geopressuretemplate$pressurepath &&
        "path_most_likely" %in% save_list
    ) {
      path_most_likely <- get("path_most_likely")
      pressurepath_most_likely <- do.call(
        pressurepath_create,
        c(
          list(tag = tag, path = path_most_likely, quiet = quiet),
          config$pressurepath_create
        )
      )
      save_list <- c(save_list, "pressurepath_most_likely")
    }

    if ("geopressureviz" %in% config$geopressuretemplate$pressurepath) {
      if ("path_geopressureviz" %in% save_list) {
        path_geopressureviz <- get("path_geopressureviz")
      } else {
        file_geopressureviz <- geopressuretemplate_geopressureviz_file(id, file)
        if (file.exists(file_geopressureviz)) {
          path_geopressureviz <- utils::read.csv(file_geopressureviz)
          save_list <- c(save_list, "path_geopressureviz")
        }
      }
    }

    if (
      "geopressureviz" %in% config$geopressuretemplate$pressurepath &&
        exists("path_geopressureviz", inherits = FALSE)
    ) {
      pressurepath_geopressureviz <- do.call(
        pressurepath_create,
        c(
          list(tag = tag, path = path_geopressureviz, quiet = quiet),
          config$pressurepath_create
        )
      )
      save_list <- c(save_list, "pressurepath_geopressureviz")
    }

    if (
      "tag" %in%
        config$geopressuretemplate$pressurepath &&
        "path_tag" %in% save_list
    ) {
      path_tag <- get("path_tag")
      pressurepath_tag <- do.call(
        pressurepath_create,
        c(
          list(tag = tag, path = path_tag, quiet = quiet),
          config$pressurepath_create
        )
      )
      save_list <- c(save_list, "pressurepath_tag")
    }

    if (
      "simulation" %in%
        config$geopressuretemplate$pressurepath &&
        "path_simulation" %in% save_list
    ) {
      path_simulation <- get("path_simulation")
      pressurepath_simulation <- do.call(
        pressurepath_create,
        c(
          list(tag = tag, path = path_simulation, quiet = quiet),
          config$pressurepath_create
        )
      )
      save_list <- c(save_list, "pressurepath_simulation")
    }

    # Save the outputs to the specified file
    save(
      list = unique(save_list),
      file = file
    )
  }

  # Return the file path invisibly
  file
}
