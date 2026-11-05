# Stapelev workspace observers. Sourced inside `server()`.

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
