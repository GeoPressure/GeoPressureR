# Previous/next flight cards. Sourced inside `server()`.

# Small helper to compute distance (km) and flight duration (hours) between two stap indices
flight_info <- function(idx, nextprev) {
  nextnext_id <- stap_id_include()[idx + nextprev]
  curr_id <- as.numeric(input$stap_id)
  nb_fl <- abs(nextnext_id - curr_id)

  dist_km <- haversine_distance(
    reactVal$path[nextnext_id, c("lon", "lat")],
    reactVal$path[curr_id, c("lon", "lat")]
  )

  fl_dur <- flight_dur_reactive()[min(idx, idx + nextprev)]

  flights_val <- nb_fl
  flights_unit <- if (nb_fl > 1) "flights" else "flight"
  duration_val <- round(fl_dur, 1)
  duration_unit <- "hrs"
  distance_val <- round(dist_km)
  distance_unit <- "km"
  speed_val <- if (!is.na(fl_dur) && fl_dur > 0) {
    round(dist_km / fl_dur)
  } else {
    "—"
  }
  speed_unit <- if (identical(speed_val, "—")) "" else "km/h"

  label <- if (nextprev > 0) "Next flight" else "Previous flight"
  dot_col <- stap$col[nextnext_id]
  if (is.null(dot_col) || is.na(dot_col) || dot_col == "") {
    dot_col <- "#999999"
  }

  shiny::div(
    class = "gpv-flight-card",
    style = glue::glue("border-color:{dot_col};"),
    shiny::div(
      class = "gpv-flight-title",
      shiny::span(class = "gpv-color-dot", style = glue::glue("background-color:{dot_col};")),
      shiny::tags$span(label)
    ),
    shiny::div(
      class = "gpv-flight-metrics",
      shiny::div(
        class = "gpv-flight-metric",
        shiny::tags$i(class = "bi bi-airplane"),
        shiny::tags$span(
          class = "gpv-flight-metric-main",
          shiny::tags$span(class = "gpv-flight-metric-value", flights_val),
          shiny::tags$span(class = "gpv-flight-metric-unit", flights_unit)
        )
      ),
      shiny::div(
        class = "gpv-flight-metric",
        shiny::tags$i(class = "bi bi-hourglass-split"),
        shiny::tags$span(
          class = "gpv-flight-metric-main",
          shiny::tags$span(class = "gpv-flight-metric-value", duration_val),
          shiny::tags$span(class = "gpv-flight-metric-unit", duration_unit)
        )
      ),
      shiny::div(
        class = "gpv-flight-metric",
        shiny::tags$i(class = "bi bi-signpost-2"),
        shiny::tags$span(
          class = "gpv-flight-metric-main",
          shiny::tags$span(class = "gpv-flight-metric-value", distance_val),
          shiny::tags$span(class = "gpv-flight-metric-unit", distance_unit)
        )
      ),
      shiny::div(
        class = "gpv-flight-metric",
        shiny::tags$i(class = "bi bi-speedometer2"),
        shiny::tags$span(
          class = "gpv-flight-metric-main",
          shiny::tags$span(class = "gpv-flight-metric-value", speed_val),
          shiny::tags$span(class = "gpv-flight-metric-unit", speed_unit)
        )
      )
    )
  )
}

output$flight_prev_info <- shiny::renderUI({
  shiny::req(input$stap_id)
  if (idx() == 1) {
    return(shiny::HTML(""))
  }
  flight_info(idx(), -1)
})

output$flight_next_info <- shiny::renderUI({
  shiny::req(input$stap_id)
  if (idx() == length(stap_id_include())) {
    return(shiny::HTML(""))
  }
  flight_info(idx(), +1)
})
