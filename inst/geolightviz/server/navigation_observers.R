# Navigation observers for previous/next position and stapath updates

# Helper function to update stapath shiny::selectInput
update_stapath_helper <- function(stapath, session, selected = NULL) {
  choices <- as.list(stapath$stap_id)
  names(choices) <-
    glue::glue("#{stapath$stap_id} ({round(stapath$duration, 1)} d.)")
  shiny::updateSelectInput(
    session,
    "stap_id",
    choices = choices,
    selected = selected
  )
}

# Setup navigation observers
setup_navigation_observers <- function(
  input,
  output,
  session,
  stapath,
  show_calibration
) {
  # Initialize stapath selector
  shiny::observe({
    isolate({
      update_stapath_helper(stapath(), session)
    })
  })
  # Render tag ID
  output$tag_id <- shiny::renderUI({
    return(shiny::HTML(glue::glue("<h3 style='margin:0;'>", .tag$param$id, "</h3>")))
  })

  # Toggle visibility of navigation controls
  shiny::observe({
    shinyjs::toggle(
      id = "stapath_nav_container",
      condition = nrow(stapath()) > 0
    )
  })

  # Previous position button
  shiny::observeEvent(input$previous_position, {
    idx_new <- min(max(as.numeric(input$stap_id) - 1, 1), nrow(stapath()))
    shiny::updateSelectInput(session, "stap_id", selected = idx_new)
  })

  # Next position button
  shiny::observeEvent(input$next_position, {
    idx_new <- min(max(as.numeric(input$stap_id) + 1, 1), nrow(stapath()))
    shiny::updateSelectInput(session, "stap_id", selected = idx_new)
  })

  # Show calibration histogram button
  shiny::observeEvent(input$show_twilight_histogram, {
    idx <- as.numeric(input$stap_id)
    show_calibration(idx)
  })

  # Return update function for use by other modules
  return(list(
    update_stapath = function(stapath_, selected = NULL) {
      stapath(stapath_)
      update_stapath_helper(stapath_, session, selected)
    }
  ))
}
