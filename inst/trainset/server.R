# Preprocess tag data before server starts
pressure_data <- tag$pressure
if (!"label" %in% names(pressure_data)) {
  pressure_data$label <- ""
}

has_acceleration <- !is.null(tag$acceleration) && nrow(tag$acceleration) > 0

acceleration_data <- if (has_acceleration) {
  tag$acceleration
} else {
  data.frame(date = as.POSIXct(character(0)), value = numeric(0), label = character(0))
}
if (!"label" %in% names(acceleration_data)) {
  acceleration_data$label <- ""
}

stap_data <- if (!is.null(tag$stap) && nrow(tag$stap) > 0) {
  tag$stap
} else {
  data.frame(
    start = as.POSIXct(character(0)),
    end = as.POSIXct(character(0)),
    stap_id = character(0)
  )
}

# Precompute numeric time vectors for fast windowing
pressure_time_num <- as.numeric(pressure_data$date)
acceleration_time_num <- as.numeric(acceleration_data$date)
time_tz <- attr(pressure_data$date, "tzone")
if (is.null(time_tz) || identical(time_tz, "")) {
  time_tz <- "UTC"
}

# Stable sorting (some tags are not strictly sorted by time)
pressure_ok <- is.finite(pressure_time_num) & !is.na(pressure_time_num)
pressure_sort_idx <- which(pressure_ok)
pressure_sort_idx <- pressure_sort_idx[order(pressure_time_num[pressure_sort_idx])]
pressure_time_sorted <- pressure_time_num[pressure_sort_idx]
pressure_date_sorted <- pressure_data$date[pressure_sort_idx]
pressure_value_sorted <- pressure_data$value[pressure_sort_idx]

acceleration_sort_idx <- integer(0)
acceleration_time_sorted <- numeric(0)
acceleration_date_sorted <- as.POSIXct(character(0), tz = time_tz)
acceleration_value_sorted <- numeric(0)
if (has_acceleration) {
  acceleration_ok <- is.finite(acceleration_time_num) & !is.na(acceleration_time_num)
  acceleration_sort_idx <- which(acceleration_ok)
  acceleration_sort_idx <- acceleration_sort_idx[order(acceleration_time_num[acceleration_sort_idx])]
  acceleration_time_sorted <- acceleration_time_num[acceleration_sort_idx]
  acceleration_date_sorted <- acceleration_data$date[acceleration_sort_idx]
  acceleration_value_sorted <- acceleration_data$value[acceleration_sort_idx]
}

# Avoid c() on huge vectors
time_min <- min(pressure_data$date, na.rm = TRUE)
time_max <- max(pressure_data$date, na.rm = TRUE)
if (has_acceleration && nrow(acceleration_data) > 0) {
  time_min <- min(time_min, min(acceleration_data$date, na.rm = TRUE), na.rm = TRUE)
  time_max <- max(time_max, max(acceleration_data$date, na.rm = TRUE), na.rm = TRUE)
}

server <- function(input, output, session) {
  label_dir <- shiny::getShinyOption("label_dir")
  auto_save_enabled <- !is.null(label_dir) && dir.exists(label_dir)

  # Fixed performance parameters (not user-configurable via `trainset()`).
  max_points_detail <- 50000L
  max_points_overview <- 5000L
  min_window_seconds <- 86400 # 1 day

  curve_overview_pressure <- 0
  curve_pressure_detail_line <- 1
  curve_pressure_markers <- 2
  curve_acceleration <- if (has_acceleration) 3 else NA_integer_

  state <- reactiveValues(
    tag = tag,
    stap_data = {
      d <- stap_data
      d$duration <- GeoPressureR::stap2duration(d)
      d
    },
    ui_ready = FALSE,
    labels_dirty = FALSE,
    view_xmin = time_min,
    view_xmax = time_max,
    pressure_detail_idx = integer(0),
    acceleration_detail_idx = integer(0),
    acc_has_lines = TRUE
  )

  reactive_label_pres <- shiny::reactiveVal(pressure_data$label)
  reactive_label_acc <- shiny::reactiveVal(acceleration_data$label)

  trainset_debug <- isTRUE(shiny::getShinyOption("trainset_debug"))

  update_state_tag_labels <- function() {
    state$tag$pressure$label <- reactive_label_pres()
    if (has_acceleration) {
      state$tag$acceleration$label <- reactive_label_acc()
    }
  }

  write_labels_csv <- function(path) {
    update_state_tag_labels()
    GeoPressureR::tag_label_write(state$tag, file = path, quiet = TRUE)
    state$labels_dirty <- FALSE
  }

  session$onFlushed(
    function() {
      state$ui_ready <- TRUE
      if (isTRUE(auto_save_enabled)) {
        shinyjs::show("save_btn")
        shinyjs::hide("export_btn")
      } else {
        shinyjs::hide("save_btn")
        shinyjs::show("export_btn")
        folder_msg <- if (!is.null(label_dir)) label_dir else "data/tag-label"
        shiny::showNotification(
          glue::glue(
            "Automatic save is disabled: folder {folder_msg} does not exist. Use the download dialog to save labels."
          ),
          duration = 10,
          type = "error"
        )
      }
    },
    once = TRUE
  )

  shiny::observe({
    if (!is.null(input$active_series) && input$active_series == "acceleration") {
      shinyjs::disable("add_label_btn")
    } else {
      shinyjs::enable("add_label_btn")
    }
  })

  session$sendCustomMessage("updateTitle", glue::glue("Trainset - {tag$param$id}"))

  output$header_title <- shiny::renderUI({
    shiny::tagList(
      span("GeoPressure Trainset"),
      span(
        glue::glue("  {tag$param$id}"),
        style = "font-size: 0.8em; font-weight: 600; color: #e0e0e0; margin-left: 8px;"
      )
    )
  })

  if (isTRUE(shiny::getShinyOption("stop_on_session_end"))) {
    session$onSessionEnded(stopApp)
  }

  output$acceleration_data_available <- shiny::reactive({
    has_acceleration
  })
  outputOptions(output, "acceleration_data_available", suspendWhenHidden = FALSE)

  output$stap_data_available <- shiny::reactive({
    nrow(state$stap_data) > 0
  })
  outputOptions(output, "stap_data_available", suspendWhenHidden = FALSE)

  # Split logic into smaller files (sourced into this session).
  source("server_helpers.R", local = TRUE)
  source("server_plot.R", local = TRUE)
  source("server_labels.R", local = TRUE)
  source("server_stap.R", local = TRUE)
  source("server_save.R", local = TRUE)
  source("server_shortcuts.R", local = TRUE)
}
