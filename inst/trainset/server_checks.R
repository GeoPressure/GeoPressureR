# Check modal and warning navigation. Sourced inside `server()`.

pretty_duration <- function(tim) {
  seconds <- as.numeric(as.difftime(tim), units = "secs")
  days <- floor(seconds / (24 * 60 * 60))
  seconds <- seconds %% (24 * 60 * 60)
  hrs <- floor(seconds / (60 * 60))
  seconds <- seconds %% (60 * 60)
  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)

  out <- character(0)
  if (days > 0) {
    out <- c(out, glue::glue("{days}d"))
  }
  if (hrs > 0) {
    out <- c(out, glue::glue("{hrs}h"))
  }
  if (mins > 0) {
    out <- c(out, glue::glue("{mins}m"))
  }
  if (secs > 0 || length(out) == 0) {
    out <- c(out, glue::glue("{secs}s"))
  }

  glue::glue_collapse(out, sep = " ")
}

empty_flight_warning <- function() {
  data.frame(
    stap_s = numeric(0),
    stap_t = numeric(0),
    start = as.POSIXct(character(0), tz = time_tz),
    end = as.POSIXct(character(0), tz = time_tz),
    duration = as.difftime(numeric(0), units = "hours"),
    duration_text = character(0)
  )
}

empty_pressure_warning <- function() {
  data.frame(
    date = as.POSIXct(character(0), tz = time_tz),
    stap_id = numeric(0),
    pressure_diff = numeric(0)
  )
}

# Same checks as `tag_label_check()`, formatted for the modal.
compute_check_rows <- function(
  tag,
  warning_stap_duration,
  warning_flight_duration,
  warning_pressure_diff
) {
  if (is.null(tag$stap) || nrow(tag$stap) == 0) {
    return(list(
      single_stap = FALSE,
      stap_warning = data.frame(
        stap_id = numeric(0),
        start = tag$pressure$date[0],
        end = tag$pressure$date[0]
      ),
      flight_warning = empty_flight_warning(),
      pressure_warning = empty_pressure_warning(),
      pressure_error = "No stationary period.",
      n_stap = 0,
      n_flight = 0
    ))
  }

  checks <- tag_label_check(
    tag,
    warning_stap_duration = warning_stap_duration,
    warning_flight_duration = warning_flight_duration,
    warning_pressure_diff = warning_pressure_diff
  )
  hours_text <- function(x) {
    vapply(x, \(h) pretty_duration(as.difftime(h, units = "hours")), character(1))
  }

  stap_warning <- checks$stap
  stap_warning$duration_text <- hours_text(stap_warning$duration)
  flight_warning <- if (is.null(checks$flight)) empty_flight_warning() else checks$flight
  flight_warning$duration_text <- hours_text(flight_warning$duration)

  list(
    single_stap = nrow(tag$stap) == 1,
    stap_warning = stap_warning,
    flight_warning = flight_warning,
    pressure_warning = if (is.null(checks$pressure)) empty_pressure_warning() else checks$pressure,
    pressure_error = checks$error,
    n_stap = nrow(tag$stap),
    n_flight = nrow(tag$stap) - 1
  )
}

check_stap_button <- function(stap_id) {
  shiny::tags$button(
    type = "button",
    class = "btn btn-link btn-sm p-0",
    onclick = glue::glue(
      "Shiny.setInputValue('check_zoom_stap', '{stap_id}', {{priority: 'event'}});"
    ),
    "Go"
  )
}

check_flight_button <- function(start, end) {
  shiny::tags$button(
    type = "button",
    class = "btn btn-link btn-sm p-0",
    onclick = glue::glue(
      "Shiny.setInputValue('check_zoom_flight', {{start_ms: {round(as.numeric(start) * 1000)}, end_ms: {round(as.numeric(end) * 1000)}, nonce: Date.now()}}, {{priority: 'event'}});"
    ),
    "Go"
  )
}

check_pressure_button <- function(date) {
  shiny::tags$button(
    type = "button",
    class = "btn btn-link btn-sm p-0",
    onclick = glue::glue(
      "Shiny.setInputValue('check_zoom_pressure_diff', {{date_ms: {round(as.numeric(date) * 1000)}, nonce: Date.now()}}, {{priority: 'event'}});"
    ),
    "Go"
  )
}

check_table_stap <- function(stap_warning) {
  shiny::tags$table(
    class = "table table-sm",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Stationary period"),
        shiny::tags$th("Time range"),
        shiny::tags$th("Duration"),
        shiny::tags$th("")
      )
    ),
    shiny::tags$tbody(
      lapply(seq_len(nrow(stap_warning)), function(i) {
        s <- stap_warning[i, ]
        shiny::tags$tr(
          shiny::tags$td(glue::glue("Stap {s$stap_id}")),
          shiny::tags$td(glue::glue(
            "{format(s$start, format = '%Y-%m-%d %H:%M')} - {format(s$end, format = '%Y-%m-%d %H:%M')}"
          )),
          shiny::tags$td(s$duration_text),
          shiny::tags$td(check_stap_button(s$stap_id))
        )
      })
    )
  )
}

check_table_flight <- function(flight_warning) {
  shiny::tags$table(
    class = "table table-sm",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Flight"),
        shiny::tags$th("Time range"),
        shiny::tags$th("Duration"),
        shiny::tags$th("")
      )
    ),
    shiny::tags$tbody(
      lapply(seq_len(nrow(flight_warning)), function(i) {
        f <- flight_warning[i, ]
        shiny::tags$tr(
          shiny::tags$td(glue::glue("{f$stap_s} -> {f$stap_t}")),
          shiny::tags$td(glue::glue(
            "{format(f$start, format = '%Y-%m-%d %H:%M')} - {format(f$end, format = '%Y-%m-%d %H:%M')}"
          )),
          shiny::tags$td(f$duration_text),
          shiny::tags$td(check_flight_button(f$start, f$end))
        )
      })
    )
  )
}

check_table_pressure <- function(pressure_warning) {
  shiny::tags$table(
    class = "table table-sm",
    shiny::tags$thead(
      shiny::tags$tr(
        shiny::tags$th("Stationary period"),
        shiny::tags$th("Time"),
        shiny::tags$th("Pressure diff (hPa)"),
        shiny::tags$th("")
      )
    ),
    shiny::tags$tbody(
      lapply(seq_len(nrow(pressure_warning)), function(i) {
        p <- pressure_warning[i, ]
        shiny::tags$tr(
          shiny::tags$td(glue::glue("Stap {p$stap_id}")),
          shiny::tags$td(format(p$date, format = "%Y-%m-%d %H:%M")),
          shiny::tags$td(round(p$pressure_diff, 2)),
          shiny::tags$td(check_pressure_button(p$date))
        )
      })
    )
  )
}

check_threshold_input <- function(id, value, step = 0.5) {
  shiny::tags$input(
    id = id,
    type = "number",
    min = 0,
    step = step,
    value = value,
    class = "form-control",
    style = "width:90px;"
  )
}

limit_check_items <- function(df, max_items) {
  n <- nrow(df)
  n_show <- min(n, max_items)
  list(
    data = if (n_show > 0) df[seq_len(n_show), , drop = FALSE] else df,
    total = n,
    shown = n_show,
    truncated = n > max_items
  )
}

check_section_header <- function(title, input_id, value, step = 0.5, unit_label = NULL) {
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center border rounded px-3 py-2 mt-3 mb-2 bg-light",
    shiny::tags$strong(title),
    shiny::tags$div(
      class = "d-flex align-items-center gap-2",
      if (!is.null(unit_label)) shiny::tags$span(unit_label),
      check_threshold_input(input_id, value, step = step)
    )
  )
}

check_results_ui <- function(
  checks,
  warning_stap_duration,
  warning_flight_duration,
  warning_pressure_diff
) {
  if (checks$n_stap == 0) {
    return(list(
      stap = shiny::tags$p(
        "No stationary periods available. Add labels and recompute STAP first."
      )
    ))
  }

  stap_suffix <- if (checks$n_stap == 1) "" else "s"
  flight_suffix <- if (checks$n_flight == 1) "" else "s"
  hour_suffix_stap <- if (warning_stap_duration == 1) "" else "s"
  hour_suffix_flight <- if (warning_flight_duration == 1) "" else "s"

  stap_warning <- checks$stap_warning
  flight_warning <- checks$flight_warning
  pressure_warning <- checks$pressure_warning
  stap_limited <- limit_check_items(stap_warning, state$max_check_items)
  flight_limited <- limit_check_items(flight_warning, state$max_check_items)
  pressure_limited <- limit_check_items(pressure_warning, state$max_check_items)

  stap_section <- if (stap_limited$total > 0) {
    shiny::tagList(
      if (stap_limited$truncated) {
        shiny::tags$p(
          class = "text-warning mb-2",
          glue::glue(
            "Showing first {stap_limited$shown} of {stap_limited$total} stationary period warnings."
          )
        )
      },
      check_table_stap(stap_limited$data)
    )
  } else {
    shiny::tags$p(
      class = "text-success mb-3",
      glue::glue(
        "All {checks$n_stap} stationary period{stap_suffix} are above {warning_stap_duration} hour{hour_suffix_stap}."
      )
    )
  }

  flight_section <- if (flight_limited$total > 0) {
    shiny::tagList(
      if (flight_limited$truncated) {
        shiny::tags$p(
          class = "text-warning mb-2",
          glue::glue(
            "Showing first {flight_limited$shown} of {flight_limited$total} flight warnings."
          )
        )
      },
      check_table_flight(flight_limited$data)
    )
  } else {
    shiny::tags$p(
      class = "text-success mb-0",
      glue::glue(
        "All {checks$n_flight} flight{flight_suffix} are above {warning_flight_duration} hour{hour_suffix_flight}."
      )
    )
  }

  pressure_section <- if (!is.null(checks$pressure_error)) {
    shiny::tags$p(
      class = "text-warning mb-0",
      glue::glue("Pressure diff check unavailable: {checks$pressure_error}")
    )
  } else if (pressure_limited$total > 0) {
    shiny::tagList(
      if (pressure_limited$truncated) {
        shiny::tags$p(
          class = "text-warning mb-2",
          glue::glue(
            "Showing first {pressure_limited$shown} of {pressure_limited$total} pressure jump warnings."
          )
        )
      },
      check_table_pressure(pressure_limited$data)
    )
  } else {
    shiny::tags$p(
      class = "text-success mb-0",
      glue::glue(
        "No hourly pressure jump above {warning_pressure_diff} hPa."
      )
    )
  }

  single_stap_alert <- if (isTRUE(checks$single_stap)) {
    shiny::tags$div(
      class = "alert alert-warning d-flex justify-content-between align-items-center py-2",
      shiny::tags$span(
        "There is only one stationary period. Check flight labels and selected series."
      ),
      check_stap_button(state$stap_data$stap_id[[1]])
    )
  } else {
    NULL
  }

  list(
    stap = shiny::tagList(single_stap_alert, stap_section),
    flight = flight_section,
    pressure = pressure_section
  )
}

show_check_modal <- function() {
  modal <- shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-center justify-content-between w-100",
      shiny::span("Checks"),
      shiny::tags$button(
        type = "button",
        class = "btn-close",
        title = "Close",
        onclick = "Shiny.setInputValue('check_close', Date.now(), {priority: 'event'});"
      )
    ),
    easyClose = TRUE,
    size = "l",
    # Threshold inputs are rendered once so typing keeps focus
    check_section_header(
      "Short Stationary Periods",
      "check_warning_stap_duration",
      state$warning_stap_duration,
      step = 0.5,
      unit_label = "Threshold"
    ),
    shiny::uiOutput("check_modal_stap"),
    check_section_header(
      "Short Flights",
      "check_warning_flight_duration",
      state$warning_flight_duration,
      step = 0.5,
      unit_label = "Threshold"
    ),
    shiny::uiOutput("check_modal_flight"),
    check_section_header(
      "Large Hourly Pressure Jumps",
      "check_warning_pressure_diff",
      state$warning_pressure_diff,
      step = 0.1,
      unit_label = "Threshold"
    ),
    shiny::uiOutput("check_modal_pressure"),
    footer = NULL
  )
  modal$attribs$class <- paste(modal$attribs$class, "check-modal")
  shiny::showModal(modal)
}

check_sections <- shiny::reactive({
  checks <- compute_check_rows(
    state$tag,
    state$warning_stap_duration,
    state$warning_flight_duration,
    state$warning_pressure_diff
  )
  check_results_ui(
    checks,
    state$warning_stap_duration,
    state$warning_flight_duration,
    state$warning_pressure_diff
  )
})

output$check_modal_stap <- shiny::renderUI(check_sections()$stap)
output$check_modal_flight <- shiny::renderUI(check_sections()$flight)
output$check_modal_pressure <- shiny::renderUI(check_sections()$pressure)

shiny::observeEvent(input$check_btn, {
  refresh_stap_state()
  show_check_modal()
})

shiny::observeEvent(input$check_close, {
  shiny::removeModal()
})

set_check_threshold <- function(input_value, state_name) {
  if (!is.null(input_value) && is.finite(input_value)) {
    state[[state_name]] <- input_value
  }
}

shiny::observeEvent(
  input$check_warning_stap_duration,
  {
    set_check_threshold(input$check_warning_stap_duration, "warning_stap_duration")
  },
  ignoreInit = TRUE
)

shiny::observeEvent(
  input$check_warning_flight_duration,
  {
    set_check_threshold(input$check_warning_flight_duration, "warning_flight_duration")
  },
  ignoreInit = TRUE
)

shiny::observeEvent(
  input$check_warning_pressure_diff,
  {
    set_check_threshold(input$check_warning_pressure_diff, "warning_pressure_diff")
  },
  ignoreInit = TRUE
)

shiny::observeEvent(input$check_zoom_stap, {
  if (is.null(input$check_zoom_stap) || identical(input$check_zoom_stap, "")) {
    return()
  }
  if (!(as.character(input$check_zoom_stap) %in% as.character(state$stap_data$stap_id))) {
    return()
  }

  shiny::updateSelectInput(
    session,
    "stap_id",
    selected = as.character(input$check_zoom_stap)
  )
  # Zoom explicitly since re-selecting the current STAP does not fire an input change
  stap <- state$stap_data[state$stap_data$stap_id == input$check_zoom_stap, ]
  zoom_to_window(stap$start[[1]], stap$end[[1]])
  shiny::removeModal()
})

shiny::observeEvent(input$check_zoom_flight, {
  ev <- input$check_zoom_flight
  if (is.null(ev$start_ms) || is.null(ev$end_ms)) {
    return()
  }

  shiny::updateSelectInput(session, "stap_id", selected = "")
  start <- as.POSIXct(ev$start_ms / 1000, origin = "1970-01-01", tz = time_tz)
  end <- as.POSIXct(ev$end_ms / 1000, origin = "1970-01-01", tz = time_tz)
  zoom_to_window(start, end, lag_x_hours = 6)
  shiny::removeModal()
})

shiny::observeEvent(input$check_zoom_pressure_diff, {
  ev <- input$check_zoom_pressure_diff
  if (is.null(ev$date_ms)) {
    return()
  }

  shiny::updateSelectInput(session, "stap_id", selected = "")
  date <- as.POSIXct(ev$date_ms / 1000, origin = "1970-01-01", tz = time_tz)
  zoom_to_window(date, date, lag_x_hours = 6)
  shiny::removeModal()
})
