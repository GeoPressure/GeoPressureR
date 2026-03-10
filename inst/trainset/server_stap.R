# STAP navigation + recompute. Sourced inside `server()`.

shiny::observeEvent(input$compute_stap_btn, {
  refresh_stap_state()

  shiny::showNotification("STAP recomputed.", type = "message", duration = 2)
})

#----- STAP -----
shiny::observe({
  d <- state$stap_data
  if (nrow(d) == 0) {
    return()
  }

  stap_choices <- c(
    "None" = "",
    stats::setNames(
      d$stap_id,
      glue::glue("#{d$stap_id} ({round(d$duration, 1)}d)")
    )
  )

  current <- isolate(input$stap_id)
  if (is.null(current) || !(current %in% d$stap_id)) {
    current <- ""
  }

  shiny::updateSelectInput(
    session,
    "stap_id",
    choices = stap_choices,
    selected = current
  )
})

shiny::observeEvent(input$stap_id_prev, {
  d <- state$stap_data
  if (nrow(d) == 0) {
    return()
  }

  current_stap <- input$stap_id

  if (current_stap == "" || is.null(current_stap)) {
    new_stap <- d$stap_id[1]
  } else {
    current_index <- which(d$stap_id == current_stap)
    if (length(current_index) > 0 && current_index > 1) {
      new_stap <- d$stap_id[current_index - 1]
    } else {
      new_stap <- ""
    }
  }
  shiny::updateSelectInput(session, "stap_id", selected = new_stap)
})

shiny::observeEvent(input$stap_id_next, {
  d <- state$stap_data
  if (nrow(d) == 0) {
    return()
  }

  current_stap <- input$stap_id

  if (current_stap == "" || is.null(current_stap)) {
    new_stap <- d$stap_id[1]
  } else {
    current_index <- which(d$stap_id == current_stap)
    if (length(current_index) > 0 && current_index < nrow(d)) {
      new_stap <- d$stap_id[current_index + 1]
    } else {
      new_stap <- current_stap
    }
  }
  shiny::updateSelectInput(session, "stap_id", selected = new_stap)
})

shiny::observeEvent(input$stap_id, {
  if (!is.null(input$stap_id) && input$stap_id != "") {
    d <- state$stap_data
    if (nrow(d) == 0) {
      return()
    }
    selected_stap <- d[d$stap_id == input$stap_id, ]

    if (nrow(selected_stap) > 0) {
      zoom_to_window(selected_stap$start[[1]], selected_stap$end[[1]])
    }
  }
})
