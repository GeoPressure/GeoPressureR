#' Start the GeoPressureViz shiny app
#'
#' @description
#'
#' GeoPressureViz is an interactive app to inspect and manually edit migration paths together with
#' the underlying map likelihoods and pressure time series by stationary period. It is useful to
#' compare pressure-, light- and distance-based information before finalizing a path.
#'
#' The app can start from a `tag` object already in memory, or from an interim `.RData`/`.rda`
#' file that contains at least `tag` (and optionally `marginal`, `path_most_likely`,
#' `pressurepath`, `pressurepath_most_likely`).
#'
#' You can retrieve the edited path from the return value of this function (when `run_bg = FALSE`)
#' or with `shiny::getShinyOption("path_geopressureviz")` after the app completes.
#'
#' Learn more about GeoPressureViz in the [GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/geopressureviz.html).
#'
#' @param x One of:
#' * a GeoPressureR `tag` object;
#' * a path to an existing `.RData`/`.rda` file;
#' * a tag id (character scalar), interpreted as `"./data/interim/{id}.RData"`.
#' @param path Optional GeoPressureR `path` or `pressurepath` data.frame.
#' If `NULL`, a path is resolved from available inputs in this order:
#' * for file/id input (`x` character): `path_most_likely` (if present), then `pressurepath`
#' (if present);
#' * otherwise fallback to `tag2path(tag, interp = 1)`.
#' @param marginal map of the marginal probability computed with `graph_marginal()`. Overwrite the
#' `path` or `pressurepath` contained in the `.Rdata` file.
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @param run_bg If true, the app runs in a background R session using the `callr` package. This
#' allows you to continue using your R session while the app is running.
#' @return When `run_bg = FALSE`: The updated path visualized in the app. Can also be retrieved with
#' `shiny::getShinyOption("path_geopressureviz")` after the app completes.
#' When `run_bg = TRUE`: Returns the background process object.
#' @examplesIf FALSE
#'   geopressureviz("18LX")
#' @seealso [GeoPressureManual
#' ](https://geopressure.org/GeoPressureManual/geopressureviz.html)
#' @export
geopressureviz <- function(
  x,
  path = NULL,
  marginal = NULL,
  launch_browser = TRUE,
  run_bg = TRUE
) {
  if (inherits(x, "tag")) {
    tag <- x
  } else if (is.character(x) && length(x) == 1) {
    # Decide which extra objects to load from the file
    var_optional <- character()
    if (is.null(path)) {
      var_optional <- c(
        var_optional,
        "path_most_likely",
        "pressurepath",
        "pressurepath_most_likely"
      )
    }
    if (is.null(marginal)) {
      var_optional <- c(var_optional, "marginal")
      marginal <- NULL
    }

    load_interim(
      x,
      var = "tag",
      var_optional = if (length(var_optional)) var_optional else NULL,
      envir = environment()
    )

    # Only derive path from loaded objects if user did not supply a path
    if (is.null(path)) {
      # Accept path_most_likely instead of path, if available
      if (exists("path_most_likely", inherits = FALSE)) {
        path <- get("path_most_likely", inherits = FALSE)
      }
      # Use pressurepath if available over path_most_likely
      if (exists("pressurepath_most_likely", inherits = FALSE)) {
        pressurepath <- get("pressurepath_most_likely", inherits = FALSE)
      }
      if (exists("pressurepath", inherits = FALSE)) {
        pressurepath <- get("pressurepath", inherits = FALSE)
        if ("pressure_era5" %in% names(pressurepath)) {
          cli::cli_warn(c(
            "!" = "{.var pressurepath} has been create with an old version of \\
      {.pkg GeoPressureR} (<v3.2.0)",
            ">" = "For optimal performance, we suggest to re-run \\
      {.fun pressurepath_create}"
          ))
          pressurepath$surface_pressure <- pressurepath$pressure_era5
          pressurepath$surface_pressure_norm <- pressurepath$pressure_era5_norm
        }
        path <- pressurepath
      }
    }
  } else {
    cli::cli_abort(
      "The first argument {.var x} needs to be a {.cls tag} or a single character string (file path or id)."
    )
  }

  tag_assert(tag, "setmap")

  if (all(c("map_pressure", "map_light") %in% names(tag))) {
    tag$map_preslight <- tag$map_pressure * tag$map_light
  }

  if (!is.null(marginal)) {
    tag$map_marginal <- marginal
  }

  # Add possible map to display
  map_types <- map_type()
  map_entries <- map_types[setdiff(names(map_types), c("unknown", "mask_water"))]
  map_display <- vapply(map_entries, function(x) x$display, character(1))
  map_needed <- lapply(map_entries, function(x) x$name)
  available <- vapply(map_needed, function(x) all(x %in% names(tag)), logical(1))

  map_display <- map_display[available]
  map_needed <- map_needed[available]
  map_type_key <- stats::setNames(names(map_entries)[available], map_display)
  maps <- stats::setNames(
    lapply(map_needed, function(likelihood) tag2map(tag, likelihood = likelihood)),
    map_display
  )

  # Set colour of each stationary period
  col <- rep(
    RColorBrewer::brewer.pal(8, "Dark2"),
    times = ceiling(nrow(tag$stap) / 8)
  )
  tag$stap$col <- col[tag$stap$stap_id]
  tag$stap$duration <- stap2duration(tag$stap)

  # Get the pressure time series
  if (is.null(path)) {
    # path is not defined
    pressurepath <- data.frame()
    path <- tag2path(tag, interp = 1)
  } else if ("pressure_tag" %in% names(path)) {
    # If path is a pressurepath
    pressurepath <- path
    path <- merge(
      tag$stap,
      unique(pressurepath[
        pressurepath$stap_id == round(pressurepath$stap_id),
        c("stap_id", "lat", "lon")
      ]),
      all = TRUE
    )
    pressurepath$linetype <- as.factor(1)
    pressurepath$col <- NULL # Reset col if entered before
    pressurepath <- merge(
      pressurepath,
      tag$stap[, names(tag$stap) %in% c("stap_id", "col")],
      by = "stap_id"
    )
  } else {
    # path is a path
    pressurepath <- data.frame()
  }

  # Assign the type of path
  attr(path, "type") <- "geopressureviz"

  # out <- tryCatch(edge_add_wind_check(tag), error = function(e) e)
  # if(any(class(out) == "error")){
  #   file_wind <- NULL
  # } else {
  #   geopressure_wd <- getwd()
  #   file_wind <- \(stap_id) glue::glue("{geopressure_wd}{file(stap_id)}")
  # }
  file_wind <- NULL

  deps <- c("shiny", "shinyjs", "shinyWidgets")
  deps_missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
  if (length(deps_missing)) {
    cli::cli_alert_info(
      "Installing missing GeoPressureViz package{?s}: {.pkg {deps_missing}}"
    )
    utils::install.packages(deps_missing)
  }

  # nocov start
  if (run_bg) {
    return(shiny_run_app_bg(
      system.file("geopressureviz", package = "GeoPressureR"),
      shiny_opts = list(
        tag = tag,
        maps = maps,
        map_type_key = map_type_key,
        pressurepath = pressurepath,
        path = path,
        file_wind = file_wind,
        stop_on_session_end = FALSE
      ),
      launch_browser = launch_browser,
      proc_option = "GeoPressureR.geopressureviz_processes",
      proc_id = tag$param$id,
      app_label = "GeoPressureViz"
    ))
  }

  shiny_run_app_fg(
    system.file("geopressureviz", package = "GeoPressureR"),
    shiny_opts = list(
      tag = tag,
      maps = maps,
      map_type_key = map_type_key,
      pressurepath = pressurepath,
      path = path,
      file_wind = file_wind,
      stop_on_session_end = TRUE
    ),
    launch_browser = launch_browser
  )

  # Return the updated path from shiny options
  return(invisible(shiny::getShinyOption("path_geopressureviz")))
  # nocov end
}
