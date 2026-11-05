# Stapelev modal and offset table UI. Sourced inside `server()`.

stapelev_info <- function(text) {
  shiny::tags$span(
    class = "stapelev-info",
    `data-help` = text,
    role = "button",
    `aria-describedby` = "stapelev-help-tooltip",
    tabindex = "0",
    `aria-label` = text,
    shiny::icon("circle-info")
  )
}

show_stapelev_modal <- function() {
  current_stap <- isolate(input$stap_id)
  if (is.null(current_stap) || identical(current_stap, "")) {
    shiny::showNotification(
      "Select a stationary period before opening Auto stapelev.",
      type = "warning",
      duration = 5
    )
    return()
  }

  refresh_stap_state()
  if (!(as.numeric(current_stap) %in% state$stap_data$stap_id)) {
    shiny::showNotification(
      "The selected stationary period changed after recomputing flight boundaries.",
      type = "warning",
      duration = 6
    )
    return()
  }
  idx <- which(state$tag$pressure$stap_id == as.numeric(current_stap))
  bundles <- getFromNamespace("pressure_query_cache_read", "GeoPressureR")(
    state$tag$param$id,
    state$tag$pressure$date[idx]
  )
  if (!length(bundles)) {
    shiny::showNotification(
      "No compatible ERA5 query found. Query this stationary period in GeoPressureViz first.",
      type = "warning",
      duration = 8
    )
    return()
  }

  state$stapelev_stap_id <- as.character(current_stap)
  state$stapelev_queries <- bundles
  state$stapelev_pressure_idx <- idx
  query_choices <- stats::setNames(
    seq_along(bundles),
    vapply(seq_along(bundles), function(i) stapelev_query_label(bundles[[i]], i), character(1))
  )
  state$stapelev_draft_key <- glue::glue(
    "{as.numeric(state$tag$pressure$date[idx[1]])}-{as.numeric(state$tag$pressure$date[idx[length(idx)]])}"
  )
  draft <- state$stapelev_drafts[[state$stapelev_draft_key]]
  if (
    !is.null(draft) &&
      (!identical(draft$dates, as.numeric(state$tag$pressure$date[idx])) ||
        !identical(draft$pressure, state$tag$pressure$value[idx]))
  ) {
    draft <- NULL
  }
  query_id <- which.max(vapply(bundles, function(x) as.numeric(x$cached_at), numeric(1)))
  compute_stapelev_proposal(query_id)
  full_window <- range(as.numeric(state$stapelev_proposal$date))
  settings <- if (is.null(draft)) {
    list(
      offsets = stats::setNames(
        stapelev_best_offset(
          state$stapelev_proposal$residual[state$stapelev_proposal$label != "flight"],
          2
        ),
        ""
      ),
      windows = matrix(full_window, nrow = 1),
      range = 2,
      continuity = 6,
      discard_solar = "none",
      solar_angle = -6
    )
  } else {
    draft[c(
      "offsets",
      "windows",
      "range",
      "continuity",
      "discard_solar",
      "solar_angle",
      "gap"
    )]
  }
  # Older saved drafts predate the optional solar filter.
  if (is.null(settings$discard_solar)) {
    settings$discard_solar <- "none"
  }
  if (is.null(settings$solar_angle)) {
    settings$solar_angle <- -6
  }
  if (is.null(settings$windows) || nrow(settings$windows) != length(settings$offsets)) {
    settings$windows <- matrix(rep(full_window, length(settings$offsets)), ncol = 2, byrow = TRUE)
  }
  if (is.null(settings$gap)) {
    settings$gap <- stats::median(diff(as.numeric(state$stapelev_proposal$date))) / 60
  }
  for (field in c("range", "discard_solar", "solar_angle", "continuity", "gap")) {
    settings[[field]] <- rep_len(settings[[field]], length(settings$offsets))
  }
  stapelev_set_levels(
    settings,
    if (is.null(draft$fit)) rep(TRUE, length(settings$offsets)) else draft$fit
  )
  state$stapelev_fit_message <- ""
  stapelev_classification_settings(settings)
  stapelev_preview_settings(NULL)
  stapelev_preview_query(NULL)
  difference <- if (is.null(draft) || is.null(draft$diff_view)) FALSE else draft$diff_view

  modal <- shiny::modalDialog(
    title = shiny::div(
      class = "d-flex align-items-center justify-content-between w-100",
      shiny::span(glue::glue("Automatic stapelev proposal — Stap {current_stap}")),
      shiny::tags$button(
        type = "button",
        class = "btn-close",
        title = "Close",
        onclick = "Shiny.setInputValue('stapelev_cancel', Date.now(), {priority: 'event'});"
      )
    ),
    size = "xl",
    easyClose = FALSE,
    fade = FALSE,
    footer = NULL,
    shiny::div(
      class = "stapelev-workspace stapelev-horizontal",
      shiny::div(
        class = "stapelev-top",
        shiny::div(
          class = "stapelev-toolbar",
          shiny::tags$details(
            class = "stapelev-popover",
            shiny::tags$summary(shiny::icon("wand-magic-sparkles"), " Initialize"),
            shiny::div(
              class = "stapelev-popover-body stapelev-init-fields",
              shiny::numericInput(
                "stapelev_init_count",
                "Levels",
                length(settings$offsets),
                min = 1,
                max = 20,
                step = 1
              ),
              shiny::numericInput(
                "stapelev_init_range",
                "Tolerance ±hPa",
                2,
                min = 0.1,
                step = 0.1
              ),
              shiny::selectInput(
                "stapelev_init_solar",
                "Discard",
                c("Neither" = "none", "Day" = "day", "Night" = "night"),
                "none",
                selectize = FALSE
              ),
              shiny::actionButton(
                "stapelev_initialize",
                "Initialize",
                class = "btn btn-outline-primary btn-sm"
              ),
              stapelev_info(
                "Replaces offsets and shared filter/tolerance settings using dense unexplained pressure bands, in row order. Ignores old offsets. Keeps existing time ranges; new levels use the full period. Continuity settings are preserved."
              )
            )
          ),
          shiny::actionButton(
            "stapelev_add",
            "Level",
            icon = shiny::icon("plus"),
            class = "btn btn-outline-secondary btn-sm"
          ),
          shiny::tags$details(
            class = "stapelev-popover",
            shiny::tags$summary(shiny::icon("sliders"), " Advanced"),
            shiny::div(
              class = "stapelev-popover-body",
              shiny::numericInput(
                "stapelev_sun_angle",
                "Sun angle (°; all levels)",
                settings$solar_angle[1],
                min = -90,
                max = 90,
                step = 1
              ),
              stapelev_info(
                "0° is the horizon; −6° is civil twilight. Measurements at or above this angle are day. Shared by every level and the initializer."
              ),
              shiny::selectInput(
                "stapelev_query_id",
                "ERA5 reference",
                query_choices,
                selected = as.character(query_id)
              )
            )
          ),
          shiny::div(
            class = "stapelev-switch stapelev-inline-switch",
            shiny::checkboxInput("stapelev_diff_view", "Pressure difference", difference)
          )
        ),
        shiny::uiOutput("stapelev_offset_controls"),
        shiny::div(
          class = "stapelev-adjust-bar",
          shiny::actionButton(
            "stapelev_fit_selected",
            "Adjust",
            icon = shiny::icon("rotate"),
            class = "btn btn-outline-primary btn-sm"
          ),
          stapelev_info(
            "Adjusts only checked offsets around their current values, using each row’s time range, tolerance and filter. Unchecked levels still compete for observations. Continuity is applied afterward."
          ),
          shiny::textOutput("stapelev_fit_status", inline = TRUE),
          shiny::div(
            class = "stapelev-main-actions",
            shiny::actionButton(
              "stapelev_update",
              "Preview",
              icon = shiny::icon("eye"),
              class = "btn btn-outline-primary btn-sm"
            ),
            shiny::actionButton(
              "stapelev_apply",
              "Apply",
              icon = shiny::icon("check"),
              class = "btn btn-success btn-sm",
              disabled = "disabled"
            )
          )
        )
      ),
      shiny::div(
        class = "stapelev-plot-column",
        plotly::plotlyOutput("stapelev_proposal_plot", width = "100%", height = "100%")
      )
    )
  )

  modal$attribs$class <- paste(modal$attribs$class, "stapelev-modal")
  shiny::showModal(modal)
}

output$stapelev_offset_controls <- shiny::renderUI({
  offsets <- state$stapelev_offsets
  shiny::div(
    class = "stapelev-table-scroll",
    shiny::tags$table(
      class = "stapelev-table",
      shiny::tags$thead(
        shiny::tags$tr(
          class = "stapelev-group-head",
          shiny::tags$th(colspan = 2),
          shiny::tags$th(
            colspan = 2,
            "1 · Select times ",
            stapelev_info(
              "First choose the time range for each level. Optionally discard all day or all night measurements using the shared sun angle under Advanced."
            )
          ),
          shiny::tags$th(
            colspan = 2,
            "2 · Assign level ",
            stapelev_info(
              "Assign each eligible measurement to the closest level inside its pressure tolerance. Overlapping levels compete. Adjust refines only checked offsets from their current values."
            )
          ),
          shiny::tags$th(
            colspan = 2,
            "3 · Continuity ",
            stapelev_info(
              "Optional final cleanup after level assignment. Keep periods lasting at least Min period hours; 0 disables this filter. Bridge interruptions up to Max gap minutes, leaving interrupted measurements discarded. Longer gaps, excluded day/night measurements, flights and other levels split a period. Offsets are not changed."
            )
          ),
          shiny::tags$th(colspan = 2, "Preview")
        ),
        shiny::tags$tr(
          shiny::tags$th(title = "Check levels to include in Adjust", shiny::icon("rotate")),
          shiny::tags$th("Level"),
          shiny::tags$th(
            "Time range ",
            stapelev_info(
              "Click the range button, then drag across the plot. Dates are UTC. Initialization preserves existing ranges."
            )
          ),
          shiny::tags$th(
            "Discard ",
            stapelev_info(
              "Neither keeps all times. Day or Night excludes that part of each day. The sun angle is shared under Advanced."
            )
          ),
          shiny::tags$th("Offset ", shiny::tags$small("hPa")),
          shiny::tags$th(
            "Tolerance ",
            stapelev_info("Keep measurements within ± this many hPa of the level offset.")
          ),
          shiny::tags$th("Min period (h)"),
          shiny::tags$th("Max gap (min)"),
          shiny::tags$th(
            "Removed ",
            stapelev_info(
              "Measurements removed by continuity in the last preview. Hours use the typical sampling interval."
            )
          ),
          shiny::tags$th()
        )
      ),
      shiny::tags$tbody(lapply(seq_along(offsets), function(i) {
        id <- paste0("stapelev_card_", state$stapelev_revision, "_", i, "_")
        discard <- state$stapelev_cards$discard_solar[i]
        date <- format(
          as.POSIXct(state$stapelev_windows[i, ], origin = "1970-01-01", tz = "UTC"),
          "%d %b %Y %H:%M"
        )
        shiny::tags$tr(
          shiny::tags$td(shiny::checkboxInput(
            paste0("stapelev_fit_", state$stapelev_revision, "_", i),
            NULL,
            state$stapelev_fit[i]
          )),
          shiny::tags$td(
            shiny::span(
              class = "label-dot",
              style = glue::glue("background-color: {stapelev_level_color(i)}")
            ),
            shiny::strong(stapelev_level_name(i))
          ),
          shiny::tags$td(
            shiny::tags$button(
              id = paste0("stapelev_range_btn_", i),
              type = "button",
              class = "btn btn-outline-secondary btn-sm stapelev-range-btn",
              title = "Set range on plot",
              onclick = glue::glue(
                "Shiny.setInputValue('stapelev_range_select', {i}, {{priority: 'event'}});"
              ),
              shiny::icon("arrows-left-right")
            ),
            shiny::span(
              class = "stapelev-date",
              title = paste(date, collapse = " – "),
              if (
                identical(
                  as.numeric(state$stapelev_windows[i, ]),
                  range(as.numeric(state$stapelev_proposal$date))
                )
              ) {
                "Full"
              } else {
                shiny::tagList(date[1], shiny::tags$br(), date[2])
              }
            )
          ),
          shiny::tags$td(shiny::selectInput(
            paste0(id, "discard_solar"),
            NULL,
            c("Neither" = "none", "Day" = "day", "Night" = "night"),
            discard,
            selectize = FALSE
          )),
          shiny::tags$td(shiny::numericInput(
            paste0(
              "stapelev_offset_",
              state$stapelev_revision,
              "_",
              if (i == 1L) "baseline" else names(offsets)[i]
            ),
            NULL,
            offsets[i],
            step = 0.1
          )),
          shiny::tags$td(shiny::numericInput(
            paste0(id, "range"),
            NULL,
            state$stapelev_cards$range[i],
            min = 0.1,
            step = 0.1
          )),
          shiny::tags$td(shiny::numericInput(
            paste0(id, "continuity"),
            NULL,
            state$stapelev_cards$continuity[i],
            min = 0,
            step = 1
          )),
          shiny::tags$td(shiny::conditionalPanel(
            glue::glue("input['{id}continuity'] > 0"),
            shiny::numericInput(
              paste0(id, "gap"),
              NULL,
              state$stapelev_cards$gap[i],
              min = 0,
              step = 5
            )
          )),
          shiny::tags$td(shiny::textOutput(paste0("stapelev_continuity_summary_", i))),
          shiny::tags$td(
            if (i > 1L) {
              shiny::tags$button(
                type = "button",
                class = "btn btn-outline-danger btn-sm",
                title = "Remove level",
                onclick = glue::glue(
                  "Shiny.setInputValue('stapelev_delete', {i}, {{priority: 'event'}});"
                ),
                shiny::icon("trash")
              )
            }
          )
        )
      }))
    )
  )
})

shiny::observe({
  settings <- stapelev_classification_settings()
  proposal <- state$stapelev_proposal
  shiny::req(settings, proposal)
  eligible <- stapelev_eligible(proposal, state$stapelev_solar_elevation, settings)
  before <- stapelev_assign_offsets(
    proposal,
    settings$offsets,
    settings$windows,
    settings$range,
    0,
    eligible,
    settings$gap
  )
  after <- stapelev_classified_proposal()
  dt <- stats::median(diff(as.numeric(proposal$date))) / 3600
  for (i in seq_along(settings$offsets)) {
    local({
      level <- i
      output[[paste0("stapelev_continuity_summary_", level)]] <- shiny::renderText({
        removed <- sum(before$label == names(settings$offsets)[level] & after$label == "discard")
        if (settings$continuity[level] == 0) {
          "Off"
        } else {
          glue::glue("{removed} ({round(removed * dt, 1)} h)")
        }
      })
    })
  }
})

output$stapelev_fit_status <- shiny::renderText(state$stapelev_fit_message)
