#' @rdname geopressuretemplate
#' @family geopressuretemplate
#' @export
geopressuretemplate_pressurepath <- function(
  id,
  config = config::get(config = id),
  quiet = FALSE,
  file = glue::glue("./data/interim/{id}.RData"),
  ...
) {
  config <- geopressuretemplate_config(id, config = config, ...)

  if ("pressurepath" %in% names(config$geopressuretemplate)) {
    if (!quiet) {
      cli::cli_h2("Compute pressurepath {.field {id}}")
    }

    # Load into a separate environment so that saved objects cannot overwrite local variables
    interim <- new.env()
    load(file, envir = interim)
    tag <- interim$tag

    tag_assert(tag)

    if (tag$param$id != id) {
      cli::cli_abort(c(
        x = "{.var id}={id} is different from {.var tag$param$id}={tag$param$id}."
      ))
    }

    path_types <- config$geopressuretemplate$pressurepath
    unknown <- setdiff(path_types, c("most_likely", "simulation", "geopressureviz", "tag"))
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        x = "Unknown {.var geopressuretemplate$pressurepath}: {.val {unknown}}.",
        i = "Use {.val most_likely}, {.val simulation}, {.val geopressureviz} or {.val tag}."
      ))
    }

    # A path edited in GeoPressureViz is exported as CSV next to the interim file
    csv <- file.path(dirname(file), glue::glue("{id}-path-geopressureviz.csv"))
    if (
      "geopressureviz" %in% path_types && is.null(interim$path_geopressureviz) && file.exists(csv)
    ) {
      interim$path_geopressureviz <- read_stap(csv)
    }

    for (path_type in path_types) {
      path <- interim[[glue::glue("path_{path_type}")]]
      if (is.null(path)) {
        cli::cli_warn(
          "{.var path_{path_type}} is not available in {.file {file}}: {.var pressurepath_{path_type}} is not computed."
        )
        next
      }
      interim[[glue::glue("pressurepath_{path_type}")]] <- do.call(
        pressurepath_create,
        c(list(tag = tag, path = path, quiet = quiet), config$pressurepath_create)
      )
    }

    save(list = ls(interim), envir = interim, file = file)
  }

  # Return the file path invisibly
  invisible(file)
}
