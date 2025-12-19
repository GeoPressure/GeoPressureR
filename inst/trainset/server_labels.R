# Label UI + Plotly event handling. Sourced inside `server()`.

#----- LABELS -----
# Initialize stap_elev_count from highest elev_xxx in pressure_data$label
elev_labels <- grep("^elev_\\d+$", pressure_data$label, value = TRUE)
elev_nums <- as.integer(sub("elev_", "", elev_labels))
initial_stap_elev_count <- if (length(elev_nums) > 0 && !all(is.na(elev_nums))) {
  max(elev_nums, na.rm = TRUE)
} else {
  1
}
stap_elev_count <- reactiveVal(initial_stap_elev_count)

# Handle add label button click
observeEvent(input$add_label_btn, {
  stap_elev_count(stap_elev_count() + 1)
})

update_label_select <- function(series = "pressure", elev_count = 1) {
  if (series == "acceleration") {
    label_choices <- "flight"
  } else {
    label_choices <- c("discard")
    if (!has_acceleration) {
      label_choices <- c(label_choices, "flight")
    }
    label_choices <- c(label_choices, paste0("elev_", seq(1, elev_count)))
  }

  shiny::updateSelectizeInput(
    session,
    "label_select",
    choices = label_choices,
    selected = label_choices[[length(label_choices)]]
  )

  if ("flight" %in% label_choices) {
    shinyjs::show("compute_stap_btn")
  } else {
    shinyjs::hide("compute_stap_btn")
  }
}

# Ensure the selectize input is populated right after UI is flushed
session$onFlushed(function() {
  # Avoid reading reactive inputs here; initialize with the default series.
  update_label_select("pressure", elev_count = initial_stap_elev_count)
}, once = TRUE)

# Update label choices when elev_count changes
observeEvent(stap_elev_count(), {
  series <- active_series_or_default()
  update_label_select(series, elev_count = stap_elev_count())
})

# Re-style on series changes (labels are re-sliced from the current view indices)
if (isTRUE(has_acceleration)) {
  observeEvent(input$active_series, {
    update_label_select(input$active_series, elev_count = stap_elev_count())

    labels <- view_labels()
    apply_current_styling(
      plotly::plotlyProxy("ts_plot", session),
      labels$pressure,
      labels$acceleration,
      active_series = input$active_series
    )
  })
}

# Shared labeling function for both selection and click events
apply_labels_to_points <- function(point_data, ctrl_pressed = FALSE) {
  active_series <- active_series_or_default()
  selected_label <- if (isTRUE(ctrl_pressed)) "" else input$label_select

  if (is.null(point_data) || nrow(point_data) == 0 || length(point_data) == 0) {
    return()
  }

  # Treat pressure overview/detail line clicks as pressure markers for labeling
  point_data$curveNumber[
    point_data$curveNumber %in% c(curve_overview_pressure, curve_pressure_detail_line)
  ] <- curve_pressure_markers

  target_curve <- if (active_series == "pressure") {
    curve_pressure_markers
  } else if (active_series == "acceleration" && has_acceleration) {
    curve_acceleration
  } else {
    NULL
  }

  if (!is.null(target_curve)) {
    point_data <- point_data[point_data$curveNumber == target_curve, , drop = FALSE]
  }

  if (nrow(point_data) == 0) {
    showNotification(
      glue::glue(
        "No {active_series} points in selection. Switch active series or select different points."
      ),
      duration = 3,
      type = "warning"
    )
    return()
  }

  point_indices <- NULL
  if (!is.null(point_data$rowIndex) && any(!is.na(point_data$rowIndex))) {
    point_indices <- as.integer(point_data$rowIndex)
    point_indices <- point_indices[!is.na(point_indices)]
  } else {
    display_idx <- point_data$pointNumber + 1
    if (active_series == "pressure" && length(state$pressure_detail_idx) > 0) {
      point_indices <- state$pressure_detail_idx[display_idx]
    } else if (active_series == "acceleration" && length(state$acceleration_detail_idx) > 0) {
      point_indices <- state$acceleration_detail_idx[display_idx]
    } else {
      point_indices <- integer(0)
    }
  }

  point_indices <- unique(point_indices)
  point_indices <- point_indices[point_indices > 0]

  if (active_series == "pressure") {
    current_labels <- reactive_label_pres()
    current_labels[point_indices] <- selected_label
    reactive_label_pres(current_labels)
  } else if (active_series == "acceleration") {
    current_labels <- reactive_label_acc()
    current_labels[point_indices] <- selected_label
    reactive_label_acc(current_labels)
  }

  state$labels_dirty <- TRUE

  label_action <- if (isTRUE(ctrl_pressed)) {
    "Cleared labels from"
  } else {
    glue::glue("Applied label '{selected_label}' to")
  }
  showNotification(
    glue::glue("{label_action} {length(point_indices)} {active_series} points"),
    duration = 2,
    type = "message"
  )
}

process_plotly_event <- function(event_info) {
  if (!is.null(event_info$points) && length(event_info$points) > 0) {
    point_data <- points_to_df(event_info$points)
    apply_labels_to_points(point_data, event_info$ctrlPressed)
  }

  labels <- view_labels()
  apply_current_styling(
    plotly::plotlyProxy("ts_plot", session),
    labels$pressure,
    labels$acceleration,
    active_series = active_series_or_default()
  )
}

observeEvent(input$plotly_selected_with_keys, {
  process_plotly_event(input$plotly_selected_with_keys)
})

observeEvent(input$plotly_click_with_keys, {
  process_plotly_event(input$plotly_click_with_keys)
})
