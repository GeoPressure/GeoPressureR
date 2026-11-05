server <- function(input, output, session) {
  # Update browser tab title with tag ID
  session$sendCustomMessage(
    "updateTitle",
    glue::glue("GeoPressureViz - {tag$param$id}")
  )

  if (isTRUE(shiny::getShinyOption("stop_on_session_end", TRUE))) {
    session$onSessionEnded(function() {
      stopApp()
    })
  }

  # Extract shorter name for variable
  stap <- tag$stap
  pressure <- tag$pressure

  # Compute resolution of projection
  r <- maps[[1]]
  g <- map_expand(r$extent, r$scale)
  lonInEPSG3857 <- (g$lon * 20037508.34 / 180)
  latInEPSG3857 <- (log(tan((90 + g$lat) * pi / 360)) / (pi / 180)) *
    (20037508.34 / 180)
  fac_res_proj <- 4
  res_proj <- c(
    stats::median(diff(lonInEPSG3857)),
    min(abs(diff(latInEPSG3857))) / fac_res_proj
  )
  origin_proj <- c(stats::median(lonInEPSG3857), stats::median(latInEPSG3857))

  # Convert lat-lon into ind
  latlon2ind <- function(lat, lon) {
    lat_ind <- round(
      stats::approx(g$lat, seq(1, length(g$lat)), lat, rule = 2)$y
    )
    lon_ind <- round(
      stats::approx(g$lon, seq(1, length(g$lon)), lon, rule = 2)$y
    )
    ind <- (lon_ind - 1) * g$dim[1] + lat_ind
    return(ind)
  }

  ## Reactive variable ----

  reactVal <- reactiveValues(
    path = path,
    pressurepath = pressurepath,
    isEdit = FALSE # if editing position
  )

  # return the map
  map_display <- shiny::reactive({
    if (is.null(input$map_source)) {
      return(NA)
    }
    map <- maps[[input$map_source]]
    map$data <- lapply(map$data, function(m) {
      if (is.null(m)) {
        return(m)
      }
      m / sum(m, na.rm = TRUE)
    })
    terra::project(
      terra::rast(map),
      "epsg:3857",
      method = "near",
      res = res_proj,
      origin = origin_proj
    )
  }) |>
    bindEvent(input$map_source)

  # list of the stap_id which are above the threshold of duration and included in the model
  stap_id_include <- shiny::reactive({
    min_dur_stap <- ifelse(
      is.na(input$min_dur_stap),
      0,
      as.numeric(input$min_dur_stap)
    )
    which(stap$duration >= min_dur_stap & stap$include)
  }) |>
    bindEvent(input$min_dur_stap)

  # Precompute flight durations for current included stap ids
  flight_dur_reactive <- shiny::reactive({
    stap2flight(stap, stap_id_include())$duration
  }) |>
    bindEvent(stap_id_include())

  # index of the current stap_id in the stap_id_include (so not index in all stap_id, only the one to use)
  idx <- shiny::reactive({
    which(stap_id_include() == input$stap_id)
  }) |>
    bindEvent(input$stap_id)

  ## Render ----
  output$tag_id <- shiny::renderUI({
    return(shiny::HTML(glue::glue("<h3 style='margin:0;'>{tag$param$id}</h3>")))
  })

  # Split rendering and observers into smaller files (sourced into this session).
  source("server_map.R", local = TRUE)
  source("server_flight_card.R", local = TRUE)
  source("server_pressure_plot.R", local = TRUE)

  # Helper to post-process and merge pressure time series results
  process_pressuretimeseries <- function(
    pressuretimeseries,
    stap_idx,
    stap_id,
    requested_lat,
    requested_lon,
    requested_at,
    completed_at
  ) {
    # Keep `stap_id` as the identifier used in `pressure$stap_id` so grouping/normalization
    # stays consistent with `geopressure_timeseries()` and existing `pressurepath` objects.
    pressuretimeseries$stap_ref <- stap_id
    pressuretimeseries$col <- stap$col[stap_idx]

    cache_file <- try(
      getFromNamespace("pressure_query_cache_write", "GeoPressureR")(list(
        tag_id = tag$param$id,
        stap_id = stap_id,
        date = pressuretimeseries$date,
        pressure_tag = pressuretimeseries$pressure_tag,
        surface_pressure = pressuretimeseries$surface_pressure,
        requested_lat = requested_lat,
        requested_lon = requested_lon,
        returned_lat = pressuretimeseries$lat[1],
        returned_lon = pressuretimeseries$lon[1],
        requested_at = requested_at,
        completed_at = completed_at
      )),
      silent = TRUE
    )

    # Ensure existing pressurepath has columns we rely on (older saved objects may not).
    if (nrow(reactVal$pressurepath) > 0) {
      if (!"stap_id" %in% names(reactVal$pressurepath)) {
        reactVal$pressurepath$stap_id <- NA
      }
      if (!"stap_ref" %in% names(reactVal$pressurepath)) {
        reactVal$pressurepath$stap_ref <- NA
      }
      if (!"linetype" %in% names(reactVal$pressurepath)) {
        reactVal$pressurepath$linetype <- NA
      }
      if (!"col" %in% names(reactVal$pressurepath)) reactVal$pressurepath$col <- NA
    }

    stap_col <- if ("stap_id" %in% names(reactVal$pressurepath)) {
      "stap_id"
    } else if ("stap_ref" %in% names(reactVal$pressurepath)) {
      "stap_ref"
    } else {
      NULL
    }

    has_prev <- !is.null(stap_col) &&
      any(reactVal$pressurepath[[stap_col]] == stap_id, na.rm = TRUE)

    prev_linetype_max <- 0
    if (isTRUE(has_prev) && "linetype" %in% names(reactVal$pressurepath)) {
      prev_vals <- suppressWarnings(as.numeric(reactVal$pressurepath$linetype[
        reactVal$pressurepath[[stap_col]] == stap_id
      ]))
      prev_vals <- prev_vals[is.finite(prev_vals)]
      if (length(prev_vals) > 0) {
        prev_linetype_max <- max(prev_vals)
      }
    }

    pressuretimeseries$linetype <- as.factor(if (isTRUE(has_prev)) prev_linetype_max + 1 else 1)

    if ("j" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$j <- reactVal$pressurepath$j[1]
    }
    if ("ind" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$ind <- NA
    }
    if (!is.null(stap_col) && "include" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$include <- reactVal$pressurepath$include[
        reactVal$pressurepath[[stap_col]] == stap_id
      ][1]
    }
    if (!is.null(stap_col) && "known" %in% names(reactVal$pressurepath)) {
      pressuretimeseries$known <- reactVal$pressurepath$known[
        reactVal$pressurepath[[stap_col]] == stap_id
      ][1]
    }

    # Update path with potentially corrected lat/lon
    reactVal$path$lon[stap_idx] <- pressuretimeseries$lon[1]
    reactVal$path$lat[stap_idx] <- pressuretimeseries$lat[1]
    if (!"ind" %in% names(reactVal$path)) {
      reactVal$path$ind <- NA_integer_
    }
    reactVal$path$ind[stap_idx] <- latlon2ind(
      pressuretimeseries$lat[1],
      pressuretimeseries$lon[1]
    )

    # Merge new series into shiny::reactive pressurepath, aligning columns
    if (nrow(reactVal$pressurepath) > 0) {
      missing_cols <- setdiff(
        names(reactVal$pressurepath),
        names(pressuretimeseries)
      )
      pressuretimeseries[missing_cols] <- NA
      columns_to_keep <- intersect(
        names(reactVal$pressurepath),
        names(pressuretimeseries)
      )
      pressuretimeseries <- pressuretimeseries[, columns_to_keep]
      reactVal$pressurepath <- rbind(reactVal$pressurepath, pressuretimeseries)
    } else {
      reactVal$pressurepath <- pressuretimeseries
    }

    invisible(!inherits(cache_file, "try-error"))
  }

  # Async query for "Query pressure" button
  source("server_query_position.R", local = TRUE)
  setup_query_position(
    reactVal = reactVal,
    stap = stap,
    pressure = pressure,
    process_pressuretimeseries = process_pressuretimeseries,
    session = session
  )

  source("server_export.R", local = TRUE)
}
