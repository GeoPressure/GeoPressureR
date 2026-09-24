#' @section Basemap API keys:
#' Interactive maps always offer Esri basemaps (gray canvas, imagery and topographic). CARTO,
#' Mapbox and MapTiler basemaps are added to the layer control only when their API key is stored
#' in your system keyring. Get a key from each provider:
#'
#' * CARTO: "Get your API key" on [CARTO Basemaps](https://carto.com/basemaps).
#' * Mapbox: [Access tokens](https://account.mapbox.com/access-tokens/) page of your account
#'   (see [Mapbox documentation](https://docs.mapbox.com/help/getting-started/access-tokens/)).
#' * MapTiler: [API keys](https://cloud.maptiler.com/account/keys/) page of your MapTiler Cloud
#'   account (see [MapTiler documentation](https://docs.maptiler.com/cloud/api/authentication-key/)).
#'
#' Then store each key once:
#'
#' ```r
#' keyring::key_set_with_value("CARTO_API_KEY", password = "<your CARTO API key>")
#' keyring::key_set_with_value("MAPBOX_ACCESS_TOKEN", password = "<your Mapbox token>")
#' keyring::key_set_with_value("MAPTILER_API_KEY", password = "<your MapTiler API key>")
#' ```
#'
#' Mapbox access tokens are visible in the browser when a map is rendered. Use a public, read-only
#' token rather than a secret token.
