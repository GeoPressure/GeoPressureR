#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_tag <- function(
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
    return(invisible(geopressuretemplate_tag_scalar(
      id = inputs$id[[1]],
      config = inputs$config[[1]],
      quiet = quiet,
      file = inputs$file[[1]],
      ...
    )))
  }

  if (!quiet) {
    cli::cli_h1("Running geopressuretemplate_tag for {.val {length(inputs$id)}} tags")
  }

  out <- lapply(seq_along(inputs$id), function(i) {
    geopressuretemplate_tag_scalar(
      id = inputs$id[[i]],
      config = inputs$config[[i]],
      quiet = quiet,
      file = inputs$file[[i]],
      ...
    )
  })
  names(out) <- inputs$id

  invisible(out)
}

#' @noRd
geopressuretemplate_tag_scalar <- function(id, config, quiet, file, ...) {
  # Create the config file
  config <- geopressuretemplate_config(id, config = config, ...)

  # Check if folder exist
  dir_file <- dirname(file)
  if (!dir.exists(dir_file)) {
    cli::cli_bullets(c(
      "!" = "The directory {.file {dir_file}} does not exists."
    ))
    # Use interactive prompt only in interactive sessions.
    res <- if (interactive()) {
      utils::askYesNo("Do you want to create it?")
    } else {
      TRUE
    }
    if (res) {
      dir.create(dir_file, recursive = TRUE)
    } else {
      cli::cli_abort("Please create the directory and run the function again.")
    }
  }

  if (!quiet) {
    cli::cli_h2("Prepare tag {.field {id}}")
  }
  # Create a tag object and initialize it with sensor data and other parameters
  tag <- do.call(
    tag_create,
    c(
      list(id = id, quiet = quiet),
      config$tag_create
    )
  )

  # Label the tag with additional metadata and annotations
  tag <- do.call(
    tag_label,
    c(
      list(tag = tag, quiet = quiet),
      config$tag_label
    )
  )

  # Set the geospatial map for the tag using provided parameters
  tag <- do.call(
    tag_set_map,
    c(
      list(tag = tag),
      config$tag_set_map
    )
  )

  # If light mapping is required, process it
  if ("map_light" %in% config$geopressuretemplate$likelihood) {
    if (!quiet) {
      cli::cli_h2("Build light likelihood map")
    }
    # Compute the twilight (light-based) map
    tag <- do.call(
      twilight_create,
      c(
        list(tag = tag),
        config$twilight_create
      )
    )

    tag <- do.call(
      twilight_label_read,
      c(
        list(tag = tag),
        config$twilight_label_read
      )
    )

    tag <- do.call(
      geolight_map,
      c(
        list(tag = tag, quiet = quiet),
        config$geolight_map
      )
    )
  }

  # If pressure mapping is required, process it
  if ("map_pressure" %in% config$geopressuretemplate$likelihood) {
    if (!quiet) {
      cli::cli_h2("Build pressure likelihood map")
    }
    tag <- do.call(
      geopressure_map,
      c(
        list(tag = tag, quiet = quiet),
        config$geopressure_map
      )
    )
  }

  param <- tag$param

  # Save the processed tag object to a file if requested
  save(
    tag,
    param,
    file = file
  )

  # Return the processed tag object invisibly
  tag
}
