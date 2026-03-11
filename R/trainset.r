#' Start the GeoPressure Trainset shiny app
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
#' @param debug If `TRUE`, prints debug messages about plot refreshes (x-range changes and point
#' counts) to the R console. Defaults to `FALSE`.
#' @return A GeoPressureR `tag` object (with pressure and optionally acceleration data
#' and an `id` in `tag$param$id`). If `run_bg = TRUE`, a background process object
#' is returned invisibly, with the `tag` attached as an attribute `attr(p, "tag")`.
#' The labeled data can be exported directly from the app interface.
#'
#' @examplesIf FALSE
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#' trainset(tag, run_bg = FALSE, launch_browser = FALSE)
#'
#' @seealso [tag_label_read()], [tag_label_write()], [GeoPressureManual
#' ](https://raphaelnussbaumer.com/GeoPressureManual/)
#' @export
trainset <- function(
  x,
  launch_browser = TRUE,
  run_bg = TRUE,
  debug = FALSE
) {
  if (inherits(x, "tag")) {
    tag <- x
    if (!tag_assert(tag, "label", "logical")) {
      tag <- tag_label_auto(tag)
    }
    label_dir <- "./data/tag-label"
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

  # nocov start
  if (run_bg) {
    return(shiny_run_app_bg(
      system.file("trainset", package = "GeoPressureR"),
      shiny_opts = list(
        tag = tag,
        label_dir = label_dir,
        trainset_debug = debug,
        stop_on_session_end = FALSE
      ),
      launch_browser = launch_browser,
      proc_option = "GeoPressureR.trainset_processes",
      proc_id = tag$param$id,
      app_label = "Trainset"
    ))
  }

  shiny_run_app_fg(
    system.file("trainset", package = "GeoPressureR"),
    shiny_opts = list(
      tag = tag,
      label_dir = label_dir,
      trainset_debug = debug,
      stop_on_session_end = TRUE
    ),
    launch_browser = launch_browser
  )

  return(invisible(tag))
  # nocov end
}

# Convert a TRAINSET CSV file to a GeoPressureR tag
csv2tag <- function(file, id = NULL) {
  assertthat::assert_that(is.character(file), length(file) == 1)
  assertthat::assert_that(file.exists(file))

  csv <- trainset_read_raw(file)

  if (is.null(id)) {
    id <- sub("-labeled$", "", sub("\\..*$", "", basename(file)))
  }

  pressure <- csv[csv$series == "pressure", ]
  acceleration <- csv[csv$series == "acceleration", ]

  tag <- tag_create_tabular(
    id,
    pressure_file = if (nrow(pressure)) pressure else NULL,
    acceleration_file = if (nrow(acceleration)) acceleration else NULL,
    quiet = TRUE
  )

  if (tag_assert(tag, "label", "logical")) {
    tag <- tag_label_stap(tag, quiet = TRUE)
  }

  tag
}
