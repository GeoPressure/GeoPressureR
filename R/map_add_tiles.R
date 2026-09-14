#' Add GeoPressureR basemap layers
#'
#' Adds all available GeoPressureR basemap layers to a Leaflet map. CARTO, Mapbox, and MapTiler
#' layers are added only when their API key is stored in the system keyring. Esri layers are always
#' available as fallbacks.
#'
#' Store an API key once with [keyring::key_set_with_value()]:
#'
#' ```r
#' keyring::key_set_with_value("CARTO_API_KEY", password = "<your CARTO API key>")
#' keyring::key_set_with_value("MAPBOX_ACCESS_TOKEN", password = "<your Mapbox token>")
#' keyring::key_set_with_value("MAPTILER_API_KEY", password = "<your MapTiler API key>")
#' ```
#'
#' Mapbox access tokens are visible in the browser when a map is rendered. Use a public, read-only
#' token rather than a secret token.
#'
#' @param map a Leaflet map.
#' @param provider optional single provider passed to [leaflet::addProviderTiles()]. When `NULL`,
#'   adds all available GeoPressureR basemap layers.
#' @param provider_options options passed to [leaflet::addProviderTiles()] when `provider` is set.
#' @param position position of the basemap layer control.
#'
#' @return A Leaflet map.
#' @export
map_add_tiles <- function(
  map,
  provider = NULL,
  provider_options = leaflet::providerTileOptions(),
  position = "topleft"
) {
  if (!is.null(provider)) {
    return(leaflet::addProviderTiles(map, provider, options = provider_options))
  }

  base_groups <- character()
  carto_key <- map_tile_key("CARTO_API_KEY")
  if (!is.null(carto_key)) {
    map <- map |>
      leaflet::addTiles(
        glue::glue(
          "https://{{s}}.basemaps.cartocdn.com/dark_nolabels/{{z}}/{{x}}/{{y}}{{r}}.png?key={carto_key}"
        ),
        attribution = "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/attributions'>CARTO</a>",
        group = "CARTO Dark Matter No Labels",
        options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20)
      ) |>
      leaflet::addTiles(
        glue::glue(
          "https://{{s}}.basemaps.cartocdn.com/dark_all/{{z}}/{{x}}/{{y}}{{r}}.png?key={carto_key}"
        ),
        attribution = "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/attributions'>CARTO</a>",
        group = "CARTO Dark Matter",
        options = leaflet::tileOptions(subdomains = "abcd", maxZoom = 20)
      )
    base_groups <- c(base_groups, "CARTO Dark Matter No Labels", "CARTO Dark Matter")
  }

  mapbox_token <- map_tile_key("MAPBOX_ACCESS_TOKEN")
  if (!is.null(mapbox_token)) {
    map <- map |>
      leaflet::addProviderTiles(
        "MapBox",
        group = "Mapbox Dark",
        options = leaflet::providerTileOptions(
          accessToken = mapbox_token,
          id = "mapbox/dark-v11"
        )
      ) |>
      leaflet::addProviderTiles(
        "MapBox",
        group = "Mapbox Outdoors",
        options = leaflet::providerTileOptions(
          accessToken = mapbox_token,
          id = "mapbox/outdoors-v12"
        )
      ) |>
      leaflet::addProviderTiles(
        "MapBox",
        group = "Mapbox Satellite",
        options = leaflet::providerTileOptions(
          accessToken = mapbox_token,
          id = "mapbox/satellite-v9"
        )
      ) |>
      leaflet::addProviderTiles(
        "MapBox",
        group = "Mapbox Satellite Streets",
        options = leaflet::providerTileOptions(
          accessToken = mapbox_token,
          id = "mapbox/satellite-streets-v12"
        )
      )
    base_groups <- c(
      base_groups,
      "Mapbox Dark",
      "Mapbox Outdoors",
      "Mapbox Satellite",
      "Mapbox Satellite Streets"
    )
  }

  maptiler_key <- map_tile_key("MAPTILER_API_KEY")
  if (!is.null(maptiler_key)) {
    map <- map |>
      leaflet::addProviderTiles(
        "MapTiler.DatavizDark",
        group = "MapTiler DataViz Dark",
        options = leaflet::providerTileOptions(key = maptiler_key)
      ) |>
      leaflet::addProviderTiles(
        "MapTiler.Topo",
        group = "MapTiler Topographic",
        options = leaflet::providerTileOptions(key = maptiler_key)
      ) |>
      leaflet::addProviderTiles(
        "MapTiler.Satellite",
        group = "MapTiler Satellite",
        options = leaflet::providerTileOptions(key = maptiler_key)
      ) |>
      leaflet::addProviderTiles(
        "MapTiler.Hybrid",
        group = "MapTiler Hybrid",
        options = leaflet::providerTileOptions(key = maptiler_key)
      )
    base_groups <- c(
      base_groups,
      "MapTiler DataViz Dark",
      "MapTiler Topographic",
      "MapTiler Satellite",
      "MapTiler Hybrid"
    )
  }

  map <- map |>
    leaflet::addProviderTiles("Esri.WorldGrayCanvas", group = "Esri Gray Canvas") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri Imagery") |>
    leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Esri Topographic")
  base_groups <- c(base_groups, "Esri Gray Canvas", "Esri Imagery", "Esri Topographic")

  leaflet::addLayersControl(map, baseGroups = base_groups, position = position)
}

#' @noRd
map_tile_key <- function(service) {
  if (nrow(keyring::key_list(service)) == 0) {
    return(NULL)
  }
  keyring::key_get(service)
}
