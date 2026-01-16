suppressMessages({
  library(GeoPressureR)
  library(shiny)
  library(plotly)
  library(bslib)
})

# Source utility functions
source("utils.R", local = TRUE)

tag <- shiny::getShinyOption("tag")
if (is.null(tag)) {
  cli::cli_abort(
    "No tag data found in shiny options. Please provide a valid tag object with {.fun trainset} or {.code shiny::shinyOptions(tag = tag)}."
  )
}
