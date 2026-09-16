# Twilight labeling observers

# Setup labeling-related observers
setup_labeling_observers <- function(
  input,
  is_modifying,
  twl,
  zoom_state,
  session
) {
  label_undo <- shiny::reactiveVal(list())
  label_redo <- shiny::reactiveVal(list())

  update_label_history_buttons <- function() {
    if (length(label_undo())) {
      shinyjs::enable("undo_twilight_label")
    } else {
      shinyjs::disable("undo_twilight_label")
    }
    if (length(label_redo())) {
      shinyjs::enable("redo_twilight_label")
    } else {
      shinyjs::disable("redo_twilight_label")
    }
  }

  record_label_change <- function(idx, before, after) {
    label_undo(append(label_undo(), list(list(idx = idx, before = before, after = after))))
    label_redo(list())
    update_label_history_buttons()
  }

  # Toggle labeling mode button
  shiny::observeEvent(input$label_twilight, {
    is_modifying(!is_modifying())
    if (is_modifying()) {
      shinyjs::disable("change_range")
      shinyjs::disable("add_stap")
      shinyjs::disable("remove_stap")
      shiny::updateActionButton(
        session,
        "label_twilight",
        label = shiny::tags$span("Stop", class = "btn-label"),
        icon = shiny::icon("stop")
      )
      shinyjs::removeClass("label_twilight", "primary")
    } else {
      shinyjs::enable("change_range")
      shinyjs::enable("add_stap")
      shinyjs::enable("remove_stap")
      shiny::updateActionButton(
        session,
        "label_twilight",
        label = shiny::tags$span("Start", class = "btn-label"),
        icon = shiny::icon("pen")
      )
      shinyjs::addClass("label_twilight", "primary")
    }
  })

  # Click on plotly to toggle individual points
  shiny::observeEvent(plotly::event_data("plotly_click"), {
    shiny::req(is_modifying())
    click_data <- plotly::event_data("plotly_click")
    if (is.null(click_data$x) || is.null(click_data$y)) {
      return()
    }

    twl_ <- twl()
    clicked_x <- as.POSIXct(click_data$x, tz = "UTC")
    clicked_y <- as.POSIXct(click_data$y, tz = "UTC")

    # Find nearby points
    nearby_idx <- which(
      abs(as.numeric(difftime(twl_$twilight, clicked_x, units = "days"))) <= 0.5 &
        abs(as.numeric(difftime(
          twl_$plottime,
          clicked_y,
          units = "mins"
        ))) <=
          15
    )

    # Toggle labels for nearby points
    if (length(nearby_idx) > 0) {
      before <- twl_$label[nearby_idx]
      after <- ifelse(
        before == "",
        "discard",
        ""
      )
      twl_$label[nearby_idx] <- after
      twl(twl_)
      record_label_change(nearby_idx, before, after)
    }
  })

  # Select multiple points on plotly
  shiny::observeEvent(plotly::event_data("plotly_selected"), {
    shiny::req(is_modifying())
    selected <- plotly::event_data("plotly_selected")

    if (length(selected) > 0) {
      twl_ <- twl()
      idx <- selected$pointNumber + 1
      if (length(idx) > 0) {
        before <- twl_$label[idx]
        after <- ifelse(before == "", "discard", "")
        twl_$label[idx] <- after
        twl(twl_)
        record_label_change(idx, before, after)
      }
    }
    plotly::plotlyProxyInvoke(
      plotly::plotlyProxy("plotly_div", session),
      "restyle",
      list(selectedpoints = NULL)
    )
  })

  shiny::observeEvent(input$undo_twilight_label, {
    undo <- label_undo()
    if (!length(undo)) {
      return()
    }
    transaction <- undo[[length(undo)]]
    twl_ <- twl()
    twl_$label[transaction$idx] <- transaction$before
    twl(twl_)
    label_undo(undo[-length(undo)])
    label_redo(append(label_redo(), list(transaction)))
    update_label_history_buttons()
  })

  shiny::observeEvent(input$redo_twilight_label, {
    redo <- label_redo()
    if (!length(redo)) {
      return()
    }
    transaction <- redo[[length(redo)]]
    twl_ <- twl()
    twl_$label[transaction$idx] <- transaction$after
    twl(twl_)
    label_redo(redo[-length(redo)])
    label_undo(append(label_undo(), list(transaction)))
    update_label_history_buttons()
  })

  # Capture zoom state when user zooms/pans
  shiny::observeEvent(plotly::event_data("plotly_relayout"), {
    relayout_data <- plotly::event_data("plotly_relayout")

    # Clear zoom state if any axis is auto-ranged
    if (
      !is.null(relayout_data$`xaxis.autorange`) ||
        !is.null(relayout_data$`yaxis.autorange`)
    ) {
      if (!is_modifying()) {
        zoom_state(NULL)
      }
      return()
    }

    # Only update zoom state for zoom/pan events, not drawing events
    if (
      !is.null(relayout_data$`xaxis.range[0]`) &&
        !is.null(relayout_data$`xaxis.range[1]`) &&
        !is.null(relayout_data$`yaxis.range[0]`) &&
        !is.null(relayout_data$`yaxis.range[1]`)
    ) {
      zoom_state(list(
        xaxis.range = c(
          relayout_data$`xaxis.range[0]`,
          relayout_data$`xaxis.range[1]`
        ),
        yaxis.range = c(
          relayout_data$`yaxis.range[0]`,
          relayout_data$`yaxis.range[1]`
        )
      ))
    }
  })
}
