# Async pressure-query logic for GeoPressureViz.
# Sourced inside `server()`. Call `setup_query_position()` after `reactVal` and
# `process_pressuretimeseries()` are defined.

setup_query_position <- function(reactVal, stap, pressure, process_pressuretimeseries, session) {
  reactVal$query_proc <- NULL
  reactVal$query_target_stap_idx <- NULL
  reactVal$query_target_stap_id <- NULL
  reactVal$query_started_at <- NULL
  reactVal$query_running_note_id <- NULL
  reactVal$query_button_label <- "Query pressure"

  shiny::observeEvent(input$query_position, {
    stap_idx <- as.numeric(input$stap_id)
    stap_id <- stap$stap_id[stap_idx]
    # `reactVal$path` is indexed by the stap row (input$stap_id), not necessarily by `stap$stap_id`.
    lat0 <- reactVal$path$lat[stap_idx]
    lon0 <- reactVal$path$lon[stap_idx]
    # Keep `label` and `stap_id` columns so `geopressure_timeseries()` can compute
    # `surface_pressure_norm` correctly (it uses elev_* labels and discard/flight).
    pres_df <- pressure[pressure$stap_id == stap_id, ]

    ok <- start_query_position(lat0, lon0, pres_df, stap_idx, stap_id)
    if (isTRUE(ok)) {
      # Keep a persistent "running" notification until completion (or failure).
      if (!is.null(reactVal$query_running_note_id)) {
        try(shiny::removeNotification(reactVal$query_running_note_id), silent = TRUE)
      }
      reactVal$query_running_note_id <- shiny::showNotification(
        paste0("Query running in background… (#", stap_id, ")"),
        type = "message",
        duration = NULL
      )
    }
  })

  start_query_position <- function(lat0, lon0, pres_df, stap_idx, stap_id) {
    if (!is.null(reactVal$query_proc) && isTRUE(reactVal$query_proc$is_alive())) {
      shiny::showNotification("A query is already running. Please wait.", type = "warning", duration = 4)
      return(FALSE)
    }

    try(shinyjs::disable("query_position"), silent = TRUE)
    try(
      shinyjs::html(
        "query_position",
        "<i class='fa fa-spinner fa-spin'></i> Querying\u2026"
      ),
      silent = TRUE
    )

    p <- callr::r_bg(
      func = function(lat0, lon0, pres_df) {
        GeoPressureR::geopressure_timeseries(lat0, lon0, pressure = pres_df)
      },
      args = list(
        lat0 = lat0,
        lon0 = lon0,
        pres_df = pres_df
      ),
      stdout = "|",
      stderr = "|"
    )

    reactVal$query_proc <- p
    reactVal$query_target_stap_idx <- stap_idx
    reactVal$query_target_stap_id <- stap_id
    reactVal$query_started_at <- Sys.time()
    TRUE
  }

  query_poll_timer <- reactiveTimer(250, session)
  shiny::observe({
    query_poll_timer()

    p <- reactVal$query_proc
    if (is.null(p)) {
      return()
    }

    # Only collect results once the background process is finished.
    # (Calling `get_result()` too early yields "Still alive".)
    if (isTRUE(p$is_alive())) {
      return()
    }

    stap_idx <- reactVal$query_target_stap_idx
    stap_id <- reactVal$query_target_stap_id
    reactVal$query_proc <- NULL
    reactVal$query_target_stap_idx <- NULL
    reactVal$query_target_stap_id <- NULL
    reactVal$query_started_at <- NULL
    if (!is.null(reactVal$query_running_note_id)) {
      try(shiny::removeNotification(reactVal$query_running_note_id), silent = TRUE)
      reactVal$query_running_note_id <- NULL
    }

    tryCatch(
      {
            pressuretimeseries <- p$get_result()
            process_pressuretimeseries(pressuretimeseries, stap_idx, stap_id)
            shiny::showNotification(
              paste0("Query completed. (#", stap_id, ")"),
              type = "message",
              duration = 6
            )
      },
      error = function(e) {
        cli::cli_alert_warning(c(
          "!" = "Function {.fun geopressure_timeseries} did not work.",
          "i" = conditionMessage(e)
        ))
            shiny::showNotification(
              shiny::HTML(paste0(
                "Query failed (#",
                stap_id,
                ").<br>",
                shiny::htmlEscape(conditionMessage(e))
              )),
          type = "error",
          duration = 12
        )
      },
      finally = {
        try(shinyjs::enable("query_position"), silent = TRUE)
        try(shinyjs::html("query_position", reactVal$query_button_label), silent = TRUE)
        try(p$kill(), silent = TRUE)
      }
    )
  })

  invisible(TRUE)
}
