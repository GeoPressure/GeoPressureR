# Leaflet map output and observer

render_map_output <- function(
  output,
  has_map,
  extent,
  map_display,
  contour_display,
  stapath,
  known_positions,
  col,
  input
) {
  if (!has_map) {
    return()
  }

  # Initial map render
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addMapPane("raster_pane", zIndex = 210) |>
      leaflet::addProviderTiles(
        "CartoDB.DarkMatterNoLabels",
        group = "Dark Matter"
      ) |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topography") |>
      leaflet::addLayersControl(
        baseGroups = c("Dark Matter", "Satellite", "Topography"),
        position = c("topleft")
      ) |>
      leaflet::fitBounds(
        extent[1],
        extent[3],
        extent[2],
        extent[4]
      ) |>
      leaflet::addControl(
        div(
          class = "map-control-box",
          div(
            radioButtons(
              "map_style",
              NULL,
              choices = c("Raster" = "raster", "Contour" = "contour"),
              selected = "raster",
              inline = TRUE
            )
          ),
          actionButton(
            "ml_position",
            "Find ML Position",
            class = "btn-sm",
            onclick = "event.stopPropagation();"
          ),
          actionButton(
            "edit_position",
            "Edit Position",
            class = "btn-sm",
            onclick = "event.stopPropagation();"
          )
        ),
        position = "topright",
        className = "legend"
      )
  })

  # Observer for updating map
  shiny::observe({
    req(input$map_style)
    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearShapes() |>
      leaflet::clearImages() |>
      leaflet::clearMarkers()

    # Add raster or contour based on selection
    if (input$map_style == "raster") {
      proxy <- proxy |>
        leaflet::addRasterImage(
          map_display(),
          opacity = 0.8,
          colors = leaflet::colorNumeric(
            palette = "magma",
            domain = NULL,
            na.color = "#00000000",
            alpha = TRUE
          ),
          project = FALSE,
          options = leaflet::pathOptions(pane = "raster_pane")
        )
    } else {
      contour_display_ <- contour_display()
      proxy <- proxy |>
        leaflet::addPolylines(
          lng = contour_display_$lng,
          lat = contour_display_$lat,
          color = col[as.numeric(input$stap_id)],
          fill = FALSE,
          weight = 3
        )
    }

    # Add known positions if they exist
    if (nrow(known_positions) > 0) {
      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = known_positions$known_lon,
          lat = known_positions$known_lat,
          fillOpacity = 1,
          radius = 10,
          weight = 2,
          color = "#000",
          label = as.character(glue::glue(
            "#{known_positions$stap_id} (known), {round(known_positions$duration, 1)} days"
          )),
          fillColor = "red"
        )
    }

    # Add current stapath
    stapath_ <- stapath() |>
      dplyr::filter(!is.na(lon), !is.na(lat))

    if (nrow(stapath_) > 0) {
      proxy <- proxy |>
        leaflet::addPolylines(
          lng = stapath_$lon,
          lat = stapath_$lat,
          opacity = 1,
          color = "#FFF",
          weight = 3
        ) |>
        leaflet::addCircleMarkers(
          lng = stapath_$lon,
          lat = stapath_$lat,
          fillOpacity = 1,
          radius = stapath_$duration^(0.3) * 6,
          weight = 1,
          color = "#FFF",
          label = as.character(glue::glue(
            "#{stapath_$stap_id}, {round(stapath_$duration, 1)} days"
          )),
          fillColor = col[stapath_$stap_id]
        )
    }
  })
}
