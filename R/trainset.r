#' Start the GeoPressure Trainset shiny app
#'
#' `r lifecycle::badge("experimental")`
#'
#' GeoPressure Trainset is a shiny app designed to help you manually label pressure and
#' acceleration data for training machine learning models. This interactive app allows you to
#' visualize time series data, select data points or regions, and assign behavioral labels
#' (e.g., "flight", "discard", or custom elevation labels) to create training datasets.
#'
#' The app features:
#' - Interactive plotly visualization of pressure and acceleration time series
#' - Point and region selection for efficient labeling
#' - Support for stationary periods (STAP) navigation
#' - Custom label creation (elevation labels)
#' - Export functionality to save labeled data as CSV files
#'
#' Learn more about data labeling workflows in the [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/) or explore the
#' [GeoPressureR documentation](https://raphaelnussbaumer.com/GeoPressureR/).
#'
#' @param x One of:
#' * a GeoPressureR `tag` object,
#' * a path to an interim `.RData` file containing an object called `tag`,
#' * a path to a TRAINSET-compatible `.csv` label file,
#' * a single-character `id`. In that case the function will first look for
#'   `"./data/interim/{id}.RData"`, and if not found will look for
#'   `"./data/tag-label/{id}-labeled.csv"` and then `"./data/tag-label/{id}.csv"`.
#' @param launch_browser If true (by default), the app runs in your browser, otherwise it runs on
#' Rstudio.
#' @param run_bg If true, the app runs in a background R session using the `callr` package. This
#' allows you to continue using your R session while the app is running.
#' @return A GeoPressureR `tag` object (with pressure and optionally acceleration data
#' and an `id` in `tag$param$id`). If `run_bg = TRUE`, a background process object
#' is returned invisibly, with the `tag` attached as an attribute `attr(p, "tag")`.
#' The labeled data can be exported directly from the app interface.
#'
#' @seealso [tag_label_read()], [tag_label_write()], [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/)
#' @export
trainset <- function(x, launch_browser = TRUE, run_bg = TRUE) {
  label_dir <- getwd()

  if (inherits(x, "tag")) {
    tag <- x
  } else if (is.character(x) && length(x) == 1) {
    if (file.exists(x)) {
      # x is a path to a file that exists: decide based on extension
      ext <- tolower(tools::file_ext(x))

      if (ext %in% c("rdata", "rda")) {
        # Interim file: load and extract tag without polluting caller environment
        tag <- load_interim(x, var = "tag", envir = new.env())
        # Infer label_dir from interim file structure: .../data/interim/{id}.RData
        interim_dir <- dirname(x)
        if (basename(interim_dir) == "interim") {
          data_dir <- dirname(interim_dir)
          candidate <- file.path(data_dir, "tag-label")
          if (dir.exists(candidate)) {
            label_dir <- candidate
          }
        }
      } else if (ext %in% c("csv")) {
        # TRAINSET-compatible CSV label file
        tag <- csv2tag(x)
        label_dir <- dirname(x)
      }
    } else {
      # x is a character (id), try finding interim or label files
      id <- x
      labeled_file <- glue::glue("./data/tag-label/{id}-labeled.csv")
      unlabeled_file <- glue::glue("./data/tag-label/{id}.csv")

      if (file.exists(labeled_file)) {
        tag <- csv2tag(labeled_file, id = id)
        label_dir <- dirname(labeled_file)
      } else if (file.exists(unlabeled_file)) {
        tag <- csv2tag(unlabeled_file, id = id)
        label_dir <- dirname(unlabeled_file)
      } else {
        cli::cli_abort(c(
          "Cannot find data files for id {.val {id}}",
          "i" = "Looked for:",
          "*" = "{.file {file.path(getwd(), labeled_file)}}",
          "*" = "{.file {file.path(getwd(), unlabeled_file)}}",
          "i" = "Please ensure the files exist or provide a valid file path"
        ))
      }
    }
  } else {
    cli::cli_abort("{.arg x} must be a {.cls tag} or a single character string (file path or id)")
  }

  label_dir <- normalizePath(label_dir, mustWork = FALSE)

  if (run_bg) {
    p <- callr::r_bg(
      func = function(tag, label_dir) {
        library(GeoPressureR)
        shiny::shinyOptions(tag = tag, label_dir = label_dir)
        shiny::runApp(system.file("trainset", package = "GeoPressureR"))
      },
      args = list(
        tag = tag,
        label_dir = label_dir
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
        cli::cli_alert_success("Opening Trainset app at {.url {url}}")
        utils::browseURL(url)
        break
      }
    }
    return(invisible(p))
  } else {
    if (launch_browser) {
      launch_browser <- getOption("browser")
    } else {
      launch_browser <- getOption("shiny.launch.browser", interactive())
    }
    shiny::shinyOptions(tag = tag, label_dir = label_dir)

    # Start the app
    shiny::runApp(
      system.file("trainset", package = "GeoPressureR"),
      launch.browser = launch_browser
    )

    return(invisible(tag))
  }
}

# Convert a TRAINSET CSV file to a GeoPressureR tag
csv2tag <- function(file, id = NULL) {
  assertthat::assert_that(is.character(file), length(file) == 1)
  assertthat::assert_that(file.exists(file))

  csv <- trainset_read_raw(file)

  if (is.null(id)) {
    id <- sub("-labeled$", "", sub("\\..*$", "", basename(file)))
  }

  tag <- tag_create_dataframe(
    id,
    pressure_file = csv[csv$series == "pressure", ],
    acceleration_file = csv[csv$series == "acceleration", ],
    quiet = TRUE
  )

  tag <- tag_label_stap(tag, quiet = TRUE)
  tag
}
