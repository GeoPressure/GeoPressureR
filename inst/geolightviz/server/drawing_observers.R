# Drawing and range editing observers

# Helper function to toggle drawing mode
create_draw_range_function <- function(drawing, session) {
  function(type) {
    # Toggle drawing state
    if (is.null(drawing())) {
      drawing(type)
    } else {
      drawing(NULL)
    }

    if (is.null(drawing())) {
      # Enable all buttons
      shinyjs::enable("label_twilight")
      shinyjs::enable("change_range")
      shinyjs::enable("add_stap")
      shinyjs::enable("remove_stap")
      shiny::updateActionButton(session, "change_range", icon = shiny::icon("pen"))
      shiny::updateActionButton(session, "add_stap", icon = shiny::icon("square-plus"))
    } else {
      # Disable certain buttons based on drawing type
      shinyjs::disable(c("label_twilight", "remove_stap"))
      if (type == "change_range") {
        shiny::updateActionButton(session, "change_range", icon = shiny::icon("ban"))
        shinyjs::disable("add_stap")
        shinyjs::disable("remove_stap")
      } else if (type == "add_stap") {
        shiny::updateActionButton(session, "add_stap", icon = shiny::icon("ban"))
        shinyjs::disable("change_range")
        shinyjs::disable("remove_stap")
      }
    }
  }
}

# Setup drawing-related observers
setup_drawing_observers <- function(
  input,
  drawing,
  stapath,
  map_light_aggregate,
  map_grid = NULL,
  update_stapath,
  session
) {
  # Create draw_range function
  draw_range <- create_draw_range_function(drawing, session)

  # Add stap button
  shiny::observeEvent(input$add_stap, {
    draw_range("add_stap")
  })

  # Remove stap button
  shiny::observeEvent(input$remove_stap, {
    stapath_ <- stapath()
    if (nrow(stapath_) == 1) {
      shinyjs::alert("Only one stap left. You cannot remove it")
    } else {
      idx <- as.numeric(input$stap_id)
      stapath_ <- stapath_[-idx, ]
      stapath_$stap_id <- seq_len(nrow(stapath_))
      update_stapath(stapath_, selected = max(1, idx - 1))
    }
  })

  # Change range button
  shiny::observeEvent(input$change_range, {
    draw_range("change_range")
  })

  # Plotly relayout event (for drawing rectangles)
  shiny::observeEvent(plotly::event_data("plotly_relayout"), {
    drawing_ <- drawing()
    if (is.null(drawing_)) {
      return()
    }

    relayout <- plotly::event_data("plotly_relayout")
    s <- relayout$shape
    if (is.null(s)) {
      return()
    }

    r <- c(
      as.POSIXct(utils::tail(s$x0, 1), tz = "UTC"),
      as.POSIXct(utils::tail(s$x1, 1), tz = "UTC")
    )
    if (anyNA(r)) {
      shinyjs::alert("Stap start/end could not be parsed.")
      draw_range("")
      return()
    }

    new_row <- data.frame(start = min(r), end = max(r))
    new_row$duration <- GeoPressureR::stap2duration(new_row)
    if (!is.finite(new_row$duration) || new_row$duration <= 0) {
      shinyjs::alert("Stap duration must be positive.")
      draw_range("")
      return()
    }

    new_stapath <- stapath()
    trim_ref <- new_stapath
    if (drawing_ == "change_range") {
      trim_ref <- trim_ref[-as.numeric(input$stap_id), , drop = FALSE]
    }
    if (nrow(trim_ref) > 0) {
      ord <- order(trim_ref$start)
      pos <- sum(trim_ref$start < new_row$start) + 1
      if (pos > 1) {
        new_row$start <- max(new_row$start, trim_ref$end[ord[pos - 1]])
      }
      if (pos <= length(ord)) {
        new_row$end <- min(new_row$end, trim_ref$start[ord[pos]])
      }
      new_row$duration <- GeoPressureR::stap2duration(new_row)
      if (!is.finite(new_row$duration) || new_row$duration <= 0) {
        shinyjs::alert("Stap overlaps existing range.")
        draw_range("")
        return()
      }
    }

    # Add missing columns in new_row to match stapath
    new_row[, setdiff(names(new_stapath), names(new_row))] <- NA
    new_row <- new_row[, names(new_stapath)]
    if ("include" %in% names(new_row)) {
      new_row$include <- TRUE
    }

    idx <- as.numeric(input$stap_id)

    if (drawing_ == "change_range") {
      new_stapath[idx, c("start", "end", "duration")] <- new_row[
        1,
        c("start", "end", "duration")
      ]
      new_stapath <- new_stapath[order(new_stapath$start), ]
      new_stapath$stap_id <- seq_len(nrow(new_stapath))
      idx <- which(
        new_stapath$start == new_row$start & new_stapath$end == new_row$end
      )[1]
    } else if (drawing_ == "add_stap") {
      # Add new row to stapath and re-order stap_id
      new_stapath <- rbind(new_stapath, new_row)
      new_stapath <- new_stapath[order(new_stapath$start), ]
      if (anyNA(new_stapath$start) || anyNA(new_stapath$end)) {
        shinyjs::alert("Stap start/end must not be NA.")
        draw_range("")
        return()
      }
      new_stapath$stap_id <- seq_len(nrow(new_stapath))
      # Find index of new row
      idx <- which(new_stapath$start == new_row$start)

      # If map present, start stap with most likely position
      if (!is.null(map_grid)) {
        map_light <- map_light_aggregate(new_stapath)
        if (
          !is.null(map_light) &&
            length(map_light$data) >= idx &&
            !is.null(map_light$data[[idx]])
        ) {
          lk <- map_light$data[[idx]]
          if (!is.null(lk) && any(is.finite(lk))) {
            max_idx <- which(lk == max(lk, na.rm = TRUE), arr.ind = TRUE)
            if (nrow(max_idx) > 0) {
              new_stapath$lat[idx] <- map_grid$lat[max_idx[1, 1]]
              new_stapath$lon[idx] <- map_grid$lon[max_idx[1, 2]]
            }
          }
        }
      }
    }

    selected <- NULL
    update_stapath(new_stapath, selected = idx)
    draw_range("") # Deactivate drawing mode
  })
}
