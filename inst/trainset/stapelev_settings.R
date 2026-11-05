# Stapelev workspace state, input readers, level settings and drafts. Sourced inside `server()`.

state$stapelev_revision <- 0L
stapelev_classification_settings <- shiny::reactiveVal(NULL)
stapelev_preview_settings <- shiny::reactiveVal(NULL)
stapelev_preview_query <- shiny::reactiveVal(NULL)
stapelev_preview_ready <- shiny::reactive({
  !is.null(state$stapelev_proposal) &&
    identical(stapelev_input_settings(), stapelev_preview_settings()) &&
    identical(state$stapelev_query_id, stapelev_preview_query())
})
shiny::observe({
  shinyjs::toggleState("stapelev_apply", condition = stapelev_preview_ready())
})
stapelev_draft_file <- if (auto_save_enabled) {
  file.path(label_dir, glue::glue("{tag$param$id}-stapelev-drafts.rds"))
} else {
  NULL
}
if (!is.null(stapelev_draft_file) && file.exists(stapelev_draft_file)) {
  saved <- readRDS(stapelev_draft_file)
  if (identical(saved$version, 1L)) state$stapelev_drafts <- saved$drafts
}

stapelev_current_offsets <- function() {
  stats::setNames(
    vapply(
      seq_along(state$stapelev_offsets),
      function(i) {
        value <- input[[paste0(
          "stapelev_offset_",
          state$stapelev_revision,
          "_",
          if (i == 1L) "baseline" else names(state$stapelev_offsets)[i]
        )]]
        round(if (is.null(value)) state$stapelev_offsets[i] else value, 1)
      },
      numeric(1)
    ),
    names(state$stapelev_offsets)
  )
}

stapelev_card_value <- function(field) {
  vapply(
    seq_along(state$stapelev_offsets),
    function(i) {
      value <- input[[paste0("stapelev_card_", state$stapelev_revision, "_", i, "_", field)]]
      if (is.null(value) || is.na(value)) state$stapelev_cards[[field]][i] else value
    },
    if (field == "discard_solar") character(1) else numeric(1)
  )
}

stapelev_current_fit <- function() {
  vapply(
    seq_along(state$stapelev_offsets),
    function(i) {
      value <- input[[paste0("stapelev_fit_", state$stapelev_revision, "_", i)]]
      if (is.null(value)) state$stapelev_fit[i] else isTRUE(value)
    },
    logical(1)
  )
}

# New control identities prevent delayed browser inputs from undoing a fit.
stapelev_set_levels <- function(settings, fit = rep(TRUE, length(settings$offsets))) {
  force(fit)
  state$stapelev_offsets <- settings$offsets
  state$stapelev_windows <- settings$windows
  state$stapelev_cards <- settings[c("range", "discard_solar", "solar_angle", "continuity", "gap")]
  state$stapelev_fit <- fit
  state$stapelev_revision <- state$stapelev_revision + 1L
}

stapelev_level_label <- function(i) {
  if (is.null(names(state$stapelev_offsets))) {
    if (i == 1L) "" else paste0("elev_", i - 1L)
  } else {
    names(state$stapelev_offsets)[i]
  }
}

stapelev_level_name <- function(i) {
  if (i == 1L) "elev_0" else stapelev_level_label(i)
}

stapelev_level_color <- function(i) {
  get_marker_colors(stapelev_level_label(i))[1]
}

stapelev_input_settings <- shiny::reactive({
  list(
    offsets = stapelev_current_offsets(),
    windows = state$stapelev_windows,
    range = stapelev_card_value("range"),
    continuity = stapelev_card_value("continuity"),
    gap = stapelev_card_value("gap"),
    discard_solar = stapelev_card_value("discard_solar"),
    solar_angle = rep(
      if (is.null(input$stapelev_sun_angle)) {
        state$stapelev_cards$solar_angle[1]
      } else {
        input$stapelev_sun_angle
      },
      length(state$stapelev_offsets)
    )
  )
})

compute_stapelev_proposal <- function(query_id) {
  bundle <- state$stapelev_queries[[as.integer(query_id)]]
  idx <- state$stapelev_pressure_idx
  pressure <- state$tag$pressure[idx, , drop = FALSE]
  labels <- reactive_label_pres()[idx]
  era5_idx <- match(as.numeric(pressure$date), as.numeric(bundle$date))
  surface_pressure <- bundle$surface_pressure[era5_idx]

  state$stapelev_proposal <- data.frame(
    date = pressure$date,
    pressure_tag = pressure$value,
    surface_pressure = surface_pressure,
    residual = pressure$value - surface_pressure,
    offset = NA_real_,
    error = NA_real_,
    label = ifelse(labels == "flight", "flight", "discard"),
    stringsAsFactors = FALSE
  )
  state$stapelev_solar_elevation <- suntools::solarpos(
    matrix(c(bundle$returned_lon, bundle$returned_lat), ncol = 2),
    pressure$date
  )[, 2]
  state$stapelev_query_id <- as.integer(query_id)
}

stapelev_classified_proposal <- shiny::reactive({
  proposal <- state$stapelev_proposal
  settings <- stapelev_classification_settings()
  shiny::req(proposal, settings)
  stapelev_assign_offsets(
    proposal,
    settings$offsets,
    settings$windows,
    settings$range,
    settings$continuity,
    stapelev_eligible(proposal, state$stapelev_solar_elevation, settings),
    settings$gap
  )
})

stapelev_draft_save <- function() {
  stap_id <- state$stapelev_draft_key
  if (is.null(stap_id) || identical(stap_id, "")) {
    return()
  }
  query_id <- if (is.null(input$stapelev_query_id)) {
    state$stapelev_query_id
  } else {
    as.integer(input$stapelev_query_id)
  }
  settings <- stapelev_input_settings()
  state$stapelev_drafts[[stap_id]] <- list(
    query_requested_at = as.numeric(state$stapelev_queries[[query_id]]$requested_at),
    dates = as.numeric(state$stapelev_proposal$date),
    pressure = state$stapelev_proposal$pressure_tag,
    offsets = settings$offsets,
    windows = settings$windows,
    range = settings$range,
    continuity = settings$continuity,
    gap = settings$gap,
    fit = stapelev_current_fit(),
    discard_solar = settings$discard_solar,
    solar_angle = settings$solar_angle,
    diff_view = isTRUE(input$stapelev_diff_view)
  )
  if (auto_save_enabled) {
    tmp <- tempfile(tmpdir = label_dir)
    saveRDS(list(version = 1L, drafts = shiny::isolate(state$stapelev_drafts)), tmp)
    file.rename(tmp, stapelev_draft_file)
  }
}
