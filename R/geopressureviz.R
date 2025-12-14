#' Start the GeoPressureViz shiny app
#'
#' GeoPressureViz is a shiny app designed to help you visualize the overall trajectory of the bird
#' as well as each step-by-step move. This app is particularly useful to check the correspondence
#' between pressure map, light map and flight distance. You can edit the path and query pressure
#' time series for individual stationary period to test manual what seems the optimal path.
#'
#' GeoPressureViz can be started based on a `.Rdata` file containing at least `tag`, but also
#' optionally `marginal` and/or `path` (`path_most_likely` is also accepted).
#'
#' You can retrieve the edited path from the return value of this function (when `run_bg = FALSE`)
#' or with `shiny::getShinyOption("path_geopressureviz")` after the app completes.
#'
#' Learn more about GeoPressureViz in the [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html) or with
#' this [demo of the Great Reed Warbler (18LX)](https://rafnuss.shinyapps.io/GeoPressureViz/).
#'
#'
#' @param x a GeoPressureR `tag` object, a `.Rdata` file or the
#' unique identifier `id` with a `.Rdata` file located in `"./data/interim/{id}.RData"`.
#' @param path a GeoPressureR `path` or `pressurepath` data.frame.
#' @param marginal map of the marginal probability computed with `graph_marginal()`. Overwrite the
#' `path` or `pressurepath` contained in the `.Rdata` file.
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @param run_bg If true, the app runs in a background R session using the `callr` package. This
#' allows you to continue using your R session while the app is running.
#' @return When `run_bg = FALSE`: The updated path visualized in the app. Can also be retrieved with
#' `shiny::getShinyOption("path_geopressureviz")` after the app completes.
#' When `run_bg = TRUE`: Returns the background process object.
#'
#' @seealso [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/geopressureviz.html)
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
  maps_choices <- list(
    "Pres. MSE" = "map_pressure_mse",
    "Pres. mask" = "map_pressure_mask",
    "Pressure" = "map_pressure",
    "Light" = "map_light",
    "Mag. incl." = "map_magnetic_inclination",
    "Mag. int." = "map_magnetic_intensity",
    "Magnetic" = "map_magnetic",
    "Pres.&Light" = c("map_pressure", "map_light"),
    "Marginal" = "map_marginal"
  )
  maps_is_available <- sapply(maps_choices, \(x) all(x %in% names(tag)))

  maps <- lapply(maps_choices[maps_is_available], \(likelihood) {
    tag2map(tag, likelihood = likelihood)
  })

  names(maps) <- names(maps_choices[maps_is_available])

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

  if (run_bg) {
    p <- callr::r_bg(
      func = function(tag, maps, pressurepath, path, file_wind) {
        library(GeoPressureR)

        # Set shiny options instead of global variables
        shiny::shinyOptions(
          tag = tag,
          maps = maps,
          pressurepath = pressurepath,
          path = path,
          file_wind = file_wind
        )

        shiny::runApp(system.file("geopressureviz", package = "GeoPressureR"))
      },
      args = list(
        tag = tag,
        maps = maps,
        pressurepath = pressurepath,
        path = path,
        file_wind = file_wind
      )
    )

    port <- NA
    while (p$is_alive()) {
      p$poll_io(1000) # wait up to 1s for new output
      err <- p$read_error()
      out <- p$read_output()
      txt <- paste(err, out, sep = "\n")

      if (grepl("Listening on http://127\\.0\\.0\\.1:[0-9]+", txt)) {
        port <- sub(".*127\\.0\\.0\\.1:([0-9]+).*", "\\1", txt)
        url <- glue::glue("http://127.0.0.1:{port}")
        cli::cli_alert_success("Opening GeoPressureViz app at {.url {url}}")
        utils::browseURL(url)
        break
      }
    }
    return(invisible(p))
  } else {
    # Set shiny options instead of global variables
    shiny::shinyOptions(
      tag = tag,
      maps = maps,
      pressurepath = pressurepath,
      path = path,
      file_wind = file_wind
    )

    if (launch_browser) {
      launch_browser <- getOption("browser")
    } else {
      launch_browser <- getOption("shiny.launch.browser", interactive())
    }

    # Start the app
    shiny::runApp(
      system.file("geopressureviz", package = "GeoPressureR"),
      launch.browser = launch_browser
    )
    # Return the updated path from shiny options
    return(invisible(shiny::getShinyOption("path_geopressureviz")))
  }
}
