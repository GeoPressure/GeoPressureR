# Leaflet map, navigation and position editing. Sourced inside `server()`.

output$map <- leaflet::renderLeaflet({
  map <- leaflet::leaflet() |>
    leaflet::addMapPane("raster_pane", zIndex = 210) |>
    leaflet_add_basemaps(position = "topleft")
})

## ObserveEvents ----
# Same order as the UI

shiny::observeEvent(input$full_track, {
  if (input$full_track) {
    shinyjs::hide(id = "stap_info_view", anim = TRUE)
    shinyjs::show(id = "track_info_view", anim = TRUE)
  } else {
    shinyjs::show(id = "stap_info_view", anim = TRUE)
    shinyjs::hide(id = "track_info_view", anim = TRUE)
  }
})

shiny::observeEvent(input$min_dur_stap, {
  if (length(stap_id_include()) > 0) {
    choices <- as.character(stap_id_include())
    names(choices) <- glue::glue(
      "#{stap_id_include()} ({round(stap$duration[stap_id_include()], 1)} d)"
    )
  } else {
    choices <- character()
  }
  session$onFlushed(function() {
    shiny::updateSelectizeInput(session, "stap_id", choices = choices)
  })
})

shiny::observeEvent(input$previous_position, {
  idx_new <- min(max(idx() - 1, 1), length(stap_id_include()))
  shiny::updateSelectizeInput(
    session,
    "stap_id",
    selected = as.character(stap_id_include()[idx_new])
  )
})

shiny::observeEvent(input$next_position, {
  idx_new <- min(max(idx() + 1, 1), length(stap_id_include()))
  shiny::updateSelectizeInput(
    session,
    "stap_id",
    selected = as.character(stap_id_include()[idx_new])
  )
})

shiny::observeEvent(input$edit_position, {
  if (reactVal$isEdit) {
    reactVal$isEdit <- FALSE
    shiny::updateActionButton(session, "edit_position", label = "Edit position")
    removeClass("edit_position", "primary")
  } else {
    reactVal$isEdit <- TRUE
    shiny::updateActionButton(session, "edit_position", label = "Stop editing")
    addClass("edit_position", "primary")
  }
})

shiny::observeEvent(input$map_click, {
  click <- input$map_click
  if (is.null(click)) {
    return()
  }
  if (!reactVal$isEdit) {
    return()
  }
  if (!input$full_track) {
    reactVal$path[as.numeric(input$stap_id), c("lon", "lat")] <- c(
      click$lng,
      click$lat
    )
    reactVal$path[as.numeric(input$stap_id), "ind"] <- latlon2ind(
      click$lat,
      click$lng
    )
  }
})

# Map
shiny::observe({
  proxy <- leaflet::leafletProxy("map") |>
    leaflet::clearShapes() |>
    leaflet::clearImages() |>
    leaflet::clearMarkers()
  stap_model <- stap[stap_id_include(), ]
  path_model <- reactVal$path[stap_id_include(), c("lon", "lat")]
  fl_dur <- flight_dur_reactive()
  if (input$full_track) {
    proxy <- proxy |>
      leaflet::addPolylines(
        lng = path_model$lon,
        lat = path_model$lat,
        opacity = 1,
        color = "#FFF",
        weight = 3
      ) |>
      leaflet::addCircleMarkers(
        lng = path_model$lon,
        lat = path_model$lat,
        fillOpacity = 1,
        radius = stap_model$duration^(0.3) * 10,
        weight = 1,
        color = "#FFF",
        label = glue::glue(
          "#{stap_model$stap_id}, {round(stap_model$duration, 1)} d"
        ),
        fillColor = stap_model$col
      ) |>
      leaflet::fitBounds(
        min(path_model$lon),
        min(path_model$lat),
        max(path_model$lon),
        max(path_model$lat),
        options = list(paddingBottomRight = c(300, 300))
      )
  } else {
    # Track from
    map_stap_id <- map_display()[[as.numeric(input$stap_id)]]
    if (!is.null(map_stap_id)) {
      key <- map_type_key[[input$map_source]]
      spec <- GeoPressureR::map_type()[[key]]
      if (is.null(spec)) {
        spec <- GeoPressureR::map_type()[["unknown"]]
      }
      spec_dark <- spec[["dark"]]

      proxy <- proxy |>
        leaflet::addRasterImage(
          map_stap_id,
          opacity = 0.8,
          colors = leaflet::colorNumeric(
            palette = spec_dark$palette,
            domain = NULL,
            reverse = isTRUE(spec_dark$reverse),
            na.color = "#00000000",
            alpha = TRUE
          ),
          project = FALSE,
          options = leaflet::pathOptions(pane = "raster_pane")
        )
    }
    proxy <- proxy |>
      leaflet::addPolylines(
        lng = path_model$lon,
        lat = path_model$lat,
        opacity = 0.1,
        color = "#FFF",
        weight = 3
      ) |>
      leaflet::addCircles(
        lng = path_model$lon,
        lat = path_model$lat,
        fillOpacity = 0.1,
        fillColor = "#FFF",
        weight = 0,
        radius = stap_model$duration^(0.3) * 10
      )

    if (idx() != 1) {
      proxy <- proxy |>
        leaflet::addPolylines(
          lng = path_model$lon[idx() - (1:0)],
          lat = path_model$lat[idx() - (1:0)],
          opacity = 1,
          color = "#FFF",
          weight = 3
        ) |>
        leaflet::addCircleMarkers(
          lng = path_model$lon[idx() - 1],
          lat = path_model$lat[idx() - 1],
          fillOpacity = 1,
          fillColor = stap_model$col[idx() - 1],
          weight = 1,
          color = "#FFF",
          radius = stap_model$duration[idx() - 1]^(0.3) * 10
        ) |>
        leaflet::addCircles(
          lng = path_model$lon[idx() - 1],
          lat = path_model$lat[idx() - 1],
          opacity = 1,
          color = stap_model$col[idx() - 1],
          radius = as.numeric(input$speed) * sum(fl_dur[idx() - 1]) * 1000,
          fillOpacity = 0,
          weight = 2
        )
    }
    if (idx() != length(stap_id_include())) {
      proxy <- proxy |>
        leaflet::addPolylines(
          lng = path_model$lon[idx() + (0:1)],
          lat = path_model$lat[idx() + (0:1)],
          opacity = 1,
          color = "#FFF",
          weight = 3
        ) |>
        leaflet::addCircleMarkers(
          lng = path_model$lon[idx() + 1],
          lat = path_model$lat[idx() + 1],
          fillOpacity = 1,
          fillColor = stap_model$col[idx() + 1],
          weight = 1,
          color = "#FFF",
          radius = stap_model$duration[idx() + 1]^(0.3) * 10
        ) |>
        leaflet::addCircles(
          lng = path_model$lon[idx() + 1],
          lat = path_model$lat[idx() + 1],
          opacity = 1,
          color = stap_model$col[idx() + 1],
          radius = as.numeric(input$speed) * fl_dur[idx()] * 1000,
          fillOpacity = 0,
          weight = 2
        )
    }
    proxy <- proxy |>
      leaflet::addCircleMarkers(
        lng = reactVal$path$lon[as.numeric(input$stap_id)],
        lat = reactVal$path$lat[as.numeric(input$stap_id)],
        opacity = 1,
        fillOpacity = 1,
        radius = stap$duration[as.numeric(input$stap_id)]^(0.3) * 10,
        fillColor = stap$col[as.numeric(input$stap_id)],
        color = "white",
        weight = 2
      )
  }
  proxy
}) # |> bindEvent(input$stap_id)
