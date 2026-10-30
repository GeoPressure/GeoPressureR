# Staged stapelev proposal workspace. Sourced inside `server()`.

source("stapelev.R", local = TRUE)

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

stapelev_query_label <- function(bundle, i) {
  lat_suffix <- if (bundle$returned_lat >= 0) "N" else "S"
  lon_suffix <- if (bundle$returned_lon >= 0) "E" else "W"
  requested <- format(bundle$requested_at, "%Y-%m-%d %H:%M UTC", tz = "UTC")
  glue::glue(
    "{i} · {sprintf('%.3f', abs(bundle$returned_lat))}° {lat_suffix}, ",
    "{sprintf('%.3f', abs(bundle$returned_lon))}° {lon_suffix} · {requested}"
  )
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
      if (is.null(value)) state$stapelev_cards[[field]][i] else value
    },
    state$stapelev_cards[[field]][1]
  )
}

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

# A one-row heatmap spans an independent, fixed full-height overlay axis.
# Update the background without rebuilding pressure traces.
stapelev_strip_data <- shiny::reactive({
  shiny::req(state$stapelev_proposal, length(state$stapelev_solar_elevation))
  settings <- stapelev_input_settings()
  i <- 1L
  strip <- stapelev_solar_strip(
    state$stapelev_proposal$date,
    state$stapelev_solar_elevation,
    settings$solar_angle[i]
  )
  strip$visible <- TRUE
  strip
})
stapelev_strip_display <- shiny::debounce(stapelev_strip_data, millis = 350)

shiny::observeEvent(
  stapelev_strip_display(),
  {
    shiny::req(state$stapelev_proposal)
    strip <- stapelev_strip_display()
    settings <- stapelev_classification_settings()
    plotly::plotlyProxyInvoke(
      plotly::plotlyProxy("stapelev_proposal_plot", session),
      "restyle",
      list(
        x = list(trainset_format_time(strip$x)),
        z = list(strip$z),
        text = list(strip$text),
        colorscale = list(list(list(0, "rgba(37,58,90,0.08)"), list(1, "rgba(255,255,255,0)"))),
        visible = list(strip$visible)
      ),
      list(length(settings$offsets) * 2L + 2L)
    )
  }
)

stapelev_fitted_data <- function(proposal) {
  hour <- floor(as.numeric(proposal$date) / 3600) * 3600
  i <- !duplicated(hour)
  data.frame(
    date = as.POSIXct(hour[i], origin = "1970-01-01", tz = "UTC"),
    surface_pressure = proposal$surface_pressure[i]
  )
}

stapelev_ribbon_y <- function(fitted, range) {
  if (!length(fitted)) {
    return(fitted)
  }
  lower <- fitted - range
  c(lower, lower[length(lower)], rev(fitted + range))
}

stapelev_ribbon_x <- function(date) {
  if (!length(date)) {
    return(date)
  }
  c(date, date[length(date)], rev(date))
}

stapelev_level_data <- function(fitted_data, window) {
  if (!all(is.finite(window))) {
    return(fitted_data[FALSE, ])
  }
  date_num <- as.numeric(fitted_data$date)
  x <- sort(unique(c(
    window[1],
    date_num[date_num > window[1] & date_num < window[2]],
    window[2]
  )))
  data.frame(
    date = as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
    surface_pressure = stats::approx(
      date_num,
      fitted_data$surface_pressure,
      xout = x,
      rule = 2
    )$y
  )
}

stapelev_restyle_levels <- function(settings, difference) {
  proxy <- plotly::plotlyProxy("stapelev_proposal_plot", session)
  fitted_data <- stapelev_fitted_data(state$stapelev_proposal)
  for (i in seq_along(settings$offsets)) {
    level_data <- stapelev_level_data(fitted_data, settings$windows[i, ])
    fitted <- if (difference) {
      rep(settings$offsets[[i]], nrow(level_data))
    } else {
      level_data$surface_pressure + settings$offsets[[i]]
    }
    plotly::plotlyProxyInvoke(
      proxy,
      "restyle",
      list(
        x = list(trainset_format_time(stapelev_ribbon_x(level_data$date))),
        y = list(stapelev_ribbon_y(fitted, settings$range[i]))
      ),
      list((i - 1L) * 2L)
    )
    plotly::plotlyProxyInvoke(
      proxy,
      "restyle",
      list(
        x = list(trainset_format_time(level_data$date)),
        y = list(fitted),
        hovertemplate = list(
          if (difference) {
            "%{x}<br>Offset: %{y:.2f} hPa<extra></extra>"
          } else {
            "%{x}<br>ERA5 fitted: %{y:.2f} hPa<extra></extra>"
          }
        )
      ),
      list((i - 1L) * 2L + 1L)
    )
  }
}

stapelev_restyle_view <- function(settings, difference) {
  stapelev_restyle_levels(settings, difference)
  proposal <- state$stapelev_proposal
  i <- downsample_indices(nrow(proposal), 10000L)
  logger <- if (difference) {
    proposal$pressure_tag[i] - proposal$surface_pressure[i]
  } else {
    proposal$pressure_tag[i]
  }
  proxy <- plotly::plotlyProxy("stapelev_proposal_plot", session)
  logger_trace <- length(settings$offsets) * 2L
  plotly::plotlyProxyInvoke(proxy, "restyle", list(y = list(logger)), list(logger_trace))
  plotly::plotlyProxyInvoke(
    proxy,
    "restyle",
    list(
      y = list(logger),
      hovertemplate = list(
        if (difference) {
          "%{x}<br>Logger − ERA5: %{y:.2f} hPa<br>%{text}<extra></extra>"
        } else {
          "%{x}<br>Logger: %{y:.2f} hPa<br>%{text}<extra></extra>"
        }
      )
    ),
    list(logger_trace + 1L)
  )
  plotly::plotlyProxyInvoke(
    proxy,
    "relayout",
    list(
      "yaxis.title.text" = if (difference) "Pressure difference (hPa)" else "Pressure (hPa)",
      "yaxis.autorange" = TRUE,
      meta = list(
        stapelev_key = glue::glue("{state$tag$param$id}-{state$stapelev_stap_id}"),
        stapelev_view = if (difference) "difference" else "pressure"
      )
    )
  )
}

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

output$stapelev_proposal_plot <- plotly::renderPlotly({
  proposal <- stapelev_classified_proposal()
  settings <- stapelev_classification_settings()
  fitted_data <- stapelev_fitted_data(proposal)
  i_plot <- downsample_indices(nrow(proposal), 10000L)
  proposal_plot <- proposal[i_plot, , drop = FALSE]
  difference <- shiny::isolate(isTRUE(input$stapelev_diff_view))

  p <- plotly::plot_ly()
  for (i in seq_along(settings$offsets)) {
    display_label <- stapelev_level_name(i)
    color <- stapelev_level_color(i)
    rgb <- grDevices::col2rgb(color)
    level_data <- stapelev_level_data(fitted_data, settings$windows[i, ])
    fitted <- if (difference) {
      rep(settings$offsets[[i]], nrow(level_data))
    } else {
      level_data$surface_pressure + settings$offsets[[i]]
    }
    p <- p |>
      plotly::add_ribbons(
        x = trainset_format_time(level_data$date),
        ymin = fitted - settings$range[i],
        ymax = fitted + settings$range[i],
        name = paste(display_label, "range"),
        line = list(color = "transparent"),
        fillcolor = glue::glue("rgba({rgb[1]}, {rgb[2]}, {rgb[3]}, 0.15)"),
        hoverinfo = "skip",
        showlegend = FALSE
      ) |>
      plotly::add_lines(
        x = trainset_format_time(level_data$date),
        y = fitted,
        name = display_label,
        line = list(color = color, width = 1),
        hovertemplate = if (difference) {
          "%{x}<br>Offset: %{y:.2f} hPa<extra></extra>"
        } else {
          "%{x}<br>ERA5 fitted: %{y:.2f} hPa<extra></extra>"
        }
      )
  }

  logger <- if (difference) {
    proposal_plot$pressure_tag - proposal_plot$surface_pressure
  } else {
    proposal_plot$pressure_tag
  }
  hover_label <- ifelse(proposal_plot$label == "", "elev_0", proposal_plot$label)
  hover_label <- glue::glue(
    "{hover_label}<br>Sun elevation: {round(state$stapelev_solar_elevation[i_plot], 1)}°"
  )
  view <- if (difference) "difference" else "pressure"
  p <- p |>
    plotly::add_lines(
      x = trainset_format_time(proposal_plot$date),
      y = logger,
      name = "Logger pressure",
      line = list(color = "#999999", width = 1),
      hoverinfo = "skip"
    ) |>
    plotly::add_markers(
      x = trainset_format_time(proposal_plot$date),
      y = logger,
      text = hover_label,
      name = "Proposed labels",
      marker = list(color = get_marker_colors(proposal_plot$label), size = 5),
      hovertemplate = if (difference) {
        "%{x}<br>Logger − ERA5: %{y:.2f} hPa<br>%{text}<extra></extra>"
      } else {
        "%{x}<br>Logger: %{y:.2f} hPa<br>%{text}<extra></extra>"
      }
    )
  if (length(state$stapelev_solar_elevation) == nrow(proposal)) {
    strip <- shiny::isolate(stapelev_strip_data())
    p <- p |>
      plotly::add_heatmap(
        x = trainset_format_time(strip$x),
        y = 0,
        z = strip$z,
        text = strip$text,
        yaxis = "y2",
        name = "Solar background",
        zmin = 0,
        zmax = 1,
        colorscale = list(list(0, "rgba(37,58,90,0.08)"), list(1, "rgba(255,255,255,0)")),
        visible = strip$visible,
        zsmooth = FALSE,
        showscale = FALSE,
        showlegend = FALSE,
        hoverinfo = "skip"
      )
  }
  p |>
    plotly::layout(
      xaxis = list(title = "Time", type = "date"),
      yaxis = list(
        title = if (difference) "Pressure difference (hPa)" else "Pressure (hPa)"
      ),
      yaxis2 = list(
        overlaying = "y",
        range = c(-0.5, 0.5),
        fixedrange = TRUE,
        visible = FALSE
      ),
      hovermode = "closest",
      meta = list(
        stapelev_key = glue::glue("{state$tag$param$id}-{state$stapelev_stap_id}"),
        stapelev_view = view
      ),
      uirevision = glue::glue("stapelev-{state$tag$param$id}-{state$stapelev_stap_id}"),
      showlegend = FALSE,
      margin = list(l = 50, r = 5, t = 25, b = 38)
    ) |>
    plotly::config(
      scrollZoom = FALSE,
      displayModeBar = TRUE,
      doubleClick = "reset",
      modeBarButtonsToRemove = list(
        "zoomIn2d",
        "zoomOut2d",
        "resetScale2d",
        "toImage",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "lasso2d",
        "select2d"
      ),
      displaylogo = FALSE
    ) |>
    htmlwidgets::onRender("function(el, x) { setupStapelevRangeBars(el); }") |>
    trainset_compact_plot()
})

shiny::observeEvent(input$stapelev_auto_btn, {
  show_stapelev_modal()
})

stapelev_preview <- function(settings = stapelev_input_settings()) {
  stapelev_classification_settings(settings)
  stapelev_preview_settings(settings)
  stapelev_preview_query(state$stapelev_query_id)
}

shiny::observeEvent(input$stapelev_update, {
  stapelev_preview()
})

shiny::observeEvent(
  input$stapelev_diff_view,
  {
    if (!is.null(state$stapelev_proposal)) {
      stapelev_restyle_view(stapelev_classification_settings(), isTRUE(input$stapelev_diff_view))
    }
  },
  ignoreInit = TRUE
)

shiny::observeEvent(input$stapelev_range_select, {
  session$sendCustomMessage("stapelevRangeSelect", as.integer(input$stapelev_range_select))
})

shiny::observeEvent(
  input$stapelev_range_selection,
  {
    i <- as.integer(input$stapelev_range_selection$level)
    window <- sort(c(
      input$stapelev_range_selection$start_ms,
      input$stapelev_range_selection$end_ms
    )) /
      1000
    proposal_time <- as.numeric(state$stapelev_proposal$date)
    time_range <- range(proposal_time)
    time_step <- stats::median(diff(sort(unique(proposal_time))))
    window <- pmin(pmax(window, time_range[1]), time_range[2])
    window <- round((window - time_range[1]) / time_step) * time_step + time_range[1]
    settings <- stapelev_input_settings()
    settings$windows[i, ] <- window
    stapelev_set_levels(settings, stapelev_current_fit())
    stapelev_preview(settings)
  },
  ignoreInit = TRUE
)

shiny::observeEvent(input$stapelev_add, {
  settings <- stapelev_input_settings()
  offset <- unname(tail(settings$offsets, 1))
  level <- max(c(0L, as.integer(sub("elev_", "", names(settings$offsets)[-1L])))) + 1L
  settings$offsets <- c(settings$offsets, stats::setNames(offset, glue::glue("elev_{level}")))
  settings$windows <- rbind(settings$windows, range(as.numeric(state$stapelev_proposal$date)))
  for (field in c("range", "discard_solar", "solar_angle", "continuity", "gap")) {
    settings[[field]] <- c(settings[[field]], tail(settings[[field]], 1))
  }
  stapelev_set_levels(settings, c(stapelev_current_fit(), TRUE))
  stapelev_classification_settings(settings)
})

output$stapelev_fit_status <- shiny::renderText(state$stapelev_fit_message)

shiny::observeEvent(input$stapelev_initialize, {
  settings <- stapelev_input_settings()
  count <- as.integer(input$stapelev_init_count)
  windows <- matrix(
    rep(range(as.numeric(state$stapelev_proposal$date)), count),
    ncol = 2,
    byrow = TRUE
  )
  retained <- seq_len(min(count, length(settings$offsets)))
  windows[retained, ] <- settings$windows[retained, , drop = FALSE]
  initial <- settings
  initial$offsets <- rep(0, count)
  initial$continuity <- rep_len(settings$continuity, count)
  initial$gap <- rep_len(settings$gap, count)
  initial$range <- rep(input$stapelev_init_range, count)
  initial$discard_solar <- rep(input$stapelev_init_solar, count)
  initial$solar_angle <- rep(settings$solar_angle[1], count)
  offsets <- stapelev_initialize_offsets(
    state$stapelev_proposal,
    windows,
    input$stapelev_init_range,
    stapelev_eligible(state$stapelev_proposal, state$stapelev_solar_elevation, initial)
  )
  if (is.null(offsets)) {
    shiny::showNotification(
      "Not enough unexplained pressure data for the requested number of levels. Adjust the count, time ranges or shared settings. Current cards kept.",
      type = "warning"
    )
    return()
  }
  labels <- names(settings$offsets)[retained]
  if (count > length(labels)) {
    next_id <- max(c(0L, as.integer(sub("elev_", "", names(settings$offsets)[-1L])))) + 1L
    labels <- c(labels, paste0("elev_", seq.int(next_id, length.out = count - length(labels))))
  }
  initial$offsets <- stats::setNames(offsets, labels)
  initial$windows <- windows
  stapelev_set_levels(initial)
  state$stapelev_fit_message <- "Offsets initialized from shared settings. Review each card before applying."
  stapelev_preview(initial)
})

shiny::observeEvent(input$stapelev_fit_selected, {
  settings <- stapelev_input_settings()
  checked <- stapelev_current_fit()
  fit <- stapelev_fit_offsets(
    state$stapelev_proposal,
    settings,
    stapelev_eligible(state$stapelev_proposal, state$stapelev_solar_elevation, settings),
    names(settings$offsets)[checked]
  )
  settings$offsets <- fit$offsets
  stapelev_set_levels(settings, checked)
  state$stapelev_fit_message <- if (!any(checked)) {
    "No levels checked; offsets unchanged."
  } else if (!fit$converged) {
    "Adjustment reached its iteration limit. Review the offsets."
  } else if (any(fit$support[checked] < 3L)) {
    "Adjustment complete. Levels without enough in-band observations were left unchanged."
  } else {
    "Checked offsets adjusted together; unchecked offsets kept fixed."
  }
  stapelev_preview(settings)
})

shiny::observeEvent(input$stapelev_delete, {
  settings <- stapelev_input_settings()
  i <- as.integer(input$stapelev_delete)
  settings$offsets <- settings$offsets[-i]
  settings$windows <- settings$windows[-i, , drop = FALSE]
  for (field in c("range", "discard_solar", "solar_angle", "continuity", "gap")) {
    settings[[field]] <- settings[[field]][-i]
  }
  stapelev_set_levels(settings, stapelev_current_fit()[-i])
  stapelev_classification_settings(settings)
})

shiny::observeEvent(
  input$stapelev_query_id,
  {
    if (!is.null(state$stapelev_proposal)) {
      if (as.integer(input$stapelev_query_id) == state$stapelev_query_id) {
        return()
      }
      compute_stapelev_proposal(input$stapelev_query_id)
      settings <- stapelev_input_settings()
      stapelev_classification_settings(settings)
      shiny::showNotification(
        "ERA5 reference changed. Levels and time ranges kept; refit checked offsets or preview before applying.",
        type = "message"
      )
    }
  },
  ignoreInit = TRUE
)

shiny::observe({
  current_stap <- input$stap_id
  if (is.null(current_stap) || identical(current_stap, "") || !has_pressure) {
    shinyjs::hide("stapelev_auto_btn")
    return()
  }
  idx <- which(state$tag$pressure$stap_id == as.numeric(current_stap))
  bundles <- getFromNamespace("pressure_query_cache_read", "GeoPressureR")(
    state$tag$param$id,
    state$tag$pressure$date[idx]
  )
  if (length(bundles)) shinyjs::show("stapelev_auto_btn") else shinyjs::hide("stapelev_auto_btn")
})

shiny::observeEvent(input$stapelev_cancel, {
  stapelev_draft_save()
  state$stapelev_proposal <- NULL
  shiny::removeModal()
})

shiny::observeEvent(input$stapelev_apply, {
  settings <- stapelev_input_settings()
  if (!isTRUE(shiny::isolate(stapelev_preview_ready()))) {
    shiny::showNotification(
      "Click Preview before applying these settings.",
      type = "warning"
    )
    return()
  }
  refresh_stap_state()
  idx <- which(state$tag$pressure$stap_id == as.numeric(state$stapelev_stap_id))
  if (!identical(idx, state$stapelev_pressure_idx)) {
    shiny::showNotification(
      "Stationary period changed. Reopen Auto stapelev to recompute the proposal.",
      type = "warning"
    )
    return()
  }
  proposal <- stapelev_assign_offsets(
    state$stapelev_proposal,
    settings$offsets,
    settings$windows,
    settings$range,
    settings$continuity,
    stapelev_eligible(state$stapelev_proposal, state$stapelev_solar_elevation, settings),
    settings$gap
  )
  idx <- state$stapelev_pressure_idx
  shiny::req(proposal, length(idx) == nrow(proposal))

  editable <- reactive_label_pres()[idx] != "flight"
  apply_label_edit("pressure", idx[editable], proposal$label[editable])
  stapelev_draft_save()

  elev <- grep("^elev_\\d+$", proposal$label, value = TRUE)
  if (length(elev)) {
    stap_elev_count(max(stap_elev_count(), as.integer(sub("elev_", "", elev))))
  }
  shinyjs::show("undo_stapelev_btn")
  refresh_detail_traces(state$view_xmin, state$view_xmax)
  state$stapelev_proposal <- NULL
  shiny::removeModal()
  shiny::showNotification("Stapelev proposal applied.", type = "message", duration = 4)
})

shiny::observeEvent(input$undo_stapelev_btn, {
  restore_label_edit(FALSE)
})

shiny::observeEvent(input$redo_labels_btn, {
  restore_label_edit(TRUE)
})
