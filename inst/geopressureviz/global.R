list_of_packages <- c("shiny", "shinyjs", "shinyWidgets", "callr")
new_packages <- list_of_packages[
  !(list_of_packages %in% installed.packages()[, "Package"])
]
if (length(new_packages) > 0) {
  install.packages(new_packages)
}

suppressMessages({
  library(GeoPressureR)
  library(callr)
  library(shinyjs)
  library(shiny)
  library(shinyWidgets)
})

# Get data from shiny options instead of global variables
tag <- shiny::getShinyOption("tag")
maps <- shiny::getShinyOption("maps")
map_type_key <- shiny::getShinyOption("map_type_key")
pressurepath <- shiny::getShinyOption("pressurepath")
path <- shiny::getShinyOption("path")
file_wind <- shiny::getShinyOption("file_wind")

if (is.null(tag) || is.null(maps) || is.null(map_type_key) || is.null(pressurepath) || is.null(path)) {
  cli::cli_abort(
    "Required data not found in shiny options. Please restart the app with correct options."
  )
}
