#' Load interim objects from an RData file
#'
#' @description
#' Loads one or more objects from an interim `.RData` file created during the GeoPressure analysis.
#' This is a convenience function to quickly restore saved objects (e.g., `tag`, `graph`,
#' `path_most_likely`) for a given tag `id` or directly from a file path.
#'
#' @param x A character string of length 1. If `x` is the path to an existing `.RData`/`.rda`
#'   file, that file is used. Otherwise, `x` is interpreted as a tag identifier and the function
#'   looks for `./data/interim/{x}.RData`.
#' @param var Optional name (or names) of objects that must exist in the file. If `NULL` and
#'   `var_optional` is also `NULL`, all objects from the file are loaded into `envir` (the
#'   behaviour of [base::load()]).
#' @param var_optional Character vector of object names that may or may not exist in the file. Any
#'   of these that are present will be loaded; missing ones are silently ignored.
#' @inheritParams base::load
#'
#' @return If no selection is requested (`var` and `var_optional` both `NULL`), invisibly returns
#'   the character vector of object names loaded (as in [base::load()]). Otherwise, if a single
#'   object name is requested, invisibly returns that object; if multiple names are requested,
#'   invisibly returns a named list of those objects.
#' @examplesIf FALSE
#'   load_interim("18LX", var = c("tag", "graph"))
#' @export
load_interim <- function(
  x,
  var = NULL,
  var_optional = NULL,
  envir = parent.frame(),
  verbose = FALSE
) {
  assertthat::assert_that(is.character(x), length(x) == 1)

  ext <- tolower(tools::file_ext(x))
  if (nzchar(ext) && ext %in% c("rdata", "rda") && file.exists(x)) {
    # Explicit RData/RDA file path
    file <- x
  } else {
    # Treat as id and look in ./data/interim/
    file <- glue::glue("./data/interim/{x}.RData")
  }

  if (!file.exists(file)) {
    cli::cli_abort(c(
      "x" = "File {.file {file}} does not exist.",
      "i" = "If {.val {x}} is an id, ensure you have run the previous steps of the workflow or check the spelling.",
      "i" = "If {.val {x}} is a file path, ensure the file exists."
    ))
  }

  # No selection: behave like base::load()
  if (is.null(var) && is.null(var_optional)) {
    return(invisible(base::load(file, envir = envir, verbose = verbose)))
  }

  if (is.null(var)) {
    var <- character()
  } else {
    var <- as.character(var)
  }

  if (is.null(var_optional)) {
    var_optional <- character()
  } else {
    var_optional <- as.character(var_optional)
  }

  tmp <- new.env(parent = emptyenv())
  base::load(file, envir = tmp, verbose = verbose)

  missing_required <- setdiff(var, ls(tmp))
  if (length(missing_required) > 0) {
    cli::cli_abort(
      "File {.file {file}} does not contain required object{?s} called {.var {missing_required}}."
    )
  }

  # Assign required + any present optional objects
  present_optional <- intersect(var_optional, ls(tmp))
  vars_to_assign <- c(var, present_optional)

  for (nm in vars_to_assign) {
    assign(nm, get(nm, envir = tmp), envir = envir)
  }

  if (length(vars_to_assign) == 1L) {
    invisible(get(vars_to_assign, envir = envir, inherits = FALSE))
  } else {
    invisible(mget(vars_to_assign, envir = envir, inherits = FALSE))
  }
}
