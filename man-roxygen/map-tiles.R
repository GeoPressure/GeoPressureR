#' @section Basemap API keys:
#' Interactive maps always offer Esri basemaps (gray canvas, imagery and topographic). CARTO,
#' Mapbox and MapTiler basemaps are added to the layer control only when their API key is stored
#' in your system keyring, which needs to be done once:
#'
#' ```r
#' keyring::key_set_with_value("CARTO_API_KEY", password = "<your CARTO API key>")
#' keyring::key_set_with_value("MAPBOX_ACCESS_TOKEN", password = "<your Mapbox token>")
#' keyring::key_set_with_value("MAPTILER_API_KEY", password = "<your MapTiler API key>")
#' ```
#'
#' Mapbox access tokens are visible in the browser when a map is rendered. Use a public, read-only
#' token rather than a secret token.
