suppressMessages({
  library(GeoPressureR)
  library(shinyjs)
  library(shiny)
})

# Source module files
source("modules/modal_calibration.R", local = TRUE)

# Source server files
source("server/init.R", local = TRUE)
source("server/plotly_output.R", local = TRUE)
source("server/map_output.R", local = TRUE)
source("server/navigation_observers.R", local = TRUE)
source("server/drawing_observers.R", local = TRUE)
source("server/labeling_observers.R", local = TRUE)
source("server/position_observers.R", local = TRUE)
source("server/export_handlers.R", local = TRUE)

# Get data from shiny options instead of global variables
.tag <- shiny::getShinyOption("tag")
.stapath <- shiny::getShinyOption("stapath")
.twl <- shiny::getShinyOption("twl")
.light_trace <- shiny::getShinyOption("light_trace")

if (is.null(.tag) || is.null(.twl)) {
  cli::cli_abort(
    "Required data not found in shiny options. Please restart the app with correct options."
  )
}
