# Position editing observers (ML position finder and manual editing)

# Setup position editing observers
setup_position_observers <- function(
  input,
  stapath,
  is_edit,
  map_likelihood,
  map_grid = NULL,
  session
) {
  # Find ML (Maximum Likelihood) position button
  shiny::observeEvent(input$ml_position, {
    lk <- map_likelihood()
    if (!any(is.finite(lk))) {
      return()
    }

    stapath_ <- stapath()
    if (nrow(stapath_) < 1) {
      return()
    }

    idx <- as.numeric(input$stap_id)
    if (is.na(idx) || idx < 1 || idx > nrow(stapath_)) {
      return()
    }

    max_idx <- which(lk == max(lk, na.rm = TRUE), arr.ind = TRUE)
    if (nrow(max_idx) < 1) {
      return()
    }

    stapath_$lat[idx] <- map_grid$lat[max_idx[1, 1]]
    stapath_$lon[idx] <- map_grid$lon[max_idx[1, 2]]
    stapath(stapath_)
  })

  # Toggle manual position editing mode
  shiny::observeEvent(input$edit_position, {
    if (is_edit()) {
      is_edit(FALSE)
      shiny::updateActionButton(
        session,
        "edit_position",
        label = "Edit Position"
      )
      shinyjs::removeClass("edit_position", "primary")
    } else {
      is_edit(TRUE)
      shiny::updateActionButton(
        session,
        "edit_position",
        label = "Stop editing"
      )
      shinyjs::addClass("edit_position", "primary")
    }
  })

  # Handle map clicks for manual position editing
  shiny::observeEvent(input$map_click, {
    click <- input$map_click
    shiny::req(is_edit())
    shiny::req(!is.null(click))

    new_stapath <- stapath()
    idx <- as.numeric(input$stap_id)

    new_stapath$lat[idx] <- click$lat
    new_stapath$lon[idx] <- click$lng
    stapath(new_stapath)
  })
}
