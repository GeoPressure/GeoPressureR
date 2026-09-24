#' ARCO store layout
#'
#' ECMWF's ARCO archive splits ERA5 into zarr stores addressed as
#' `{bucket}/arco/{product}/{group}/geoChunked.zarr/{short_name}`, and uses GRIB short names
#' rather than the CDS long names [pressurepath_create()] takes. This table maps one to the other
#' so callers use the same `variable` vocabulary whichever `source` they pick.
#'
#' ERA5 single levels lives in a single store; ERA5-Land is spread over six, grouped by theme.
#' The archive exposes no listing endpoint, so this map was built by probing store metadata
#' (2026-09-16) and may not be exhaustive.
#'
#' All ERA5-Land stores share one geometry (shape 672288 x 1801 x 3600, chunks 33792 x 4 x 8,
#' hours since 1970-01-01) and all single-levels arrays another (759984 x 721 x 1440,
#' chunks 67584 x 4 x 4, seconds since 1970-01-01), so the index arithmetic depends only on the
#' product, never on the store.
#'
#' @noRd
era5_arco_store_table <- function() {
  land <- function(group, bucket, short, variable) {
    data.frame(
      variable = variable,
      short_name = short,
      era5_dataset = "land",
      store = paste0("cadl-arco-geo-", bucket, "/arco/reanalysis_era5_land/", group),
      stringsAsFactors = FALSE
    )
  }
  single <- data.frame(
    short_name = c(
      "blh",
      "cbh",
      "d2m",
      "fdir",
      "fg10",
      "msl",
      "skt",
      "slhf",
      "sp",
      "sshf",
      "ssrd",
      "sst",
      "strd",
      "t2m",
      "tcc",
      "tp",
      "u10",
      "u100",
      "v10",
      "v100"
    ),
    variable = c(
      "boundary_layer_height",
      "cloud_base_height",
      "dewpoint_temperature_2m",
      "total_sky_direct_solar_radiation_at_surface",
      "wind_gust_since_previous_post_processing_10m",
      "mean_sea_level_pressure",
      "skin_temperature",
      "surface_latent_heat_flux",
      "surface_pressure",
      "surface_sensible_heat_flux",
      "surface_solar_radiation_downwards",
      "sea_surface_temperature",
      "surface_thermal_radiation_downwards",
      "temperature_2m",
      "total_cloud_cover",
      "total_precipitation",
      "u_component_of_wind_10m",
      "u_component_of_wind_100m",
      "v_component_of_wind_10m",
      "v_component_of_wind_100m"
    ),
    era5_dataset = "single-levels",
    store = "cadl-arco-geo-002/arco/reanalysis_era5_single_levels/sfc",
    stringsAsFactors = FALSE
  )
  rbind(
    single[c("variable", "short_name", "era5_dataset", "store")],
    land(
      "sfc-soil-water",
      "005",
      paste0("swvl", 1:4),
      paste0("volumetric_soil_water_layer_", 1:4)
    ),
    land(
      "sfc-soil-temperature",
      "006",
      paste0("stl", 1:4),
      paste0("soil_temperature_level_", 1:4)
    ),
    land(
      "sfc-2m-temperature",
      "007",
      c("d2m", "t2m"),
      c("dewpoint_temperature_2m", "temperature_2m")
    ),
    land(
      "sfc-wind",
      "008",
      c("u10", "v10"),
      c("u_component_of_wind_10m", "v_component_of_wind_10m")
    ),
    land(
      "sfc-pressure-precipitation",
      "009",
      c("sp", "tp"),
      c("surface_pressure", "total_precipitation")
    ),
    land(
      "sfc-radiation-heat",
      "010",
      c("ssrd", "strd"),
      c("surface_solar_radiation_downwards", "surface_thermal_radiation_downwards")
    )
  )
}

#' Variables ARCO can serve for a given product
#'
#' `"both"` reads ERA5-Land over land and ERA5 single levels over water within one request, so a
#' variable is only usable when both products carry it.
#'
#' @param era5_dataset `"single-levels"`, `"land"`, or `"both"`.
#' @return Character vector of GeoPressureR variable names, excluding the derived `"altitude"`.
#' @noRd
era5_arco_variable <- function(era5_dataset) {
  tbl <- era5_arco_store_table()
  if (era5_dataset == "both") {
    return(sort(intersect(
      tbl$variable[tbl$era5_dataset == "land"],
      tbl$variable[tbl$era5_dataset == "single-levels"]
    )))
  }
  sort(tbl$variable[tbl$era5_dataset == era5_dataset])
}

#' Resolve a variable to its ARCO array URL
#'
#' @param variable A GeoPressureR variable name.
#' @param era5_dataset `"single-levels"` or `"land"` (never `"both"`: the caller resolves that
#'   per point before reading).
#' @return The full zarr array URL.
#' @noRd
era5_arco_array <- function(variable, era5_dataset) {
  tbl <- era5_arco_store_table()
  hit <- tbl[tbl$variable == variable & tbl$era5_dataset == era5_dataset, ]
  if (nrow(hit) != 1) {
    cli::cli_abort(
      "{.val {variable}} is not available from ARCO for {.code era5_dataset = {.str {era5_dataset}}}."
    )
  }
  glue::glue(
    "https://arco.datastores.ecmwf.int/{hit$store}/geoChunked.zarr/{hit$short_name}"
  )
}
