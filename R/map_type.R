#' @keywords internal
NULL

.MAP_TYPE <- local({
  list(
    preslight = list(
      light = list(palette = "viridis"),
      dark = list(palette = "viridis"),
      name = c("map_pressure", "map_light"),
      display = "Pres.&Light"
    ),
    pressure = list(
      light = list(palette = "GnBu"),
      dark = list(palette = "magma"),
      name = "map_pressure",
      display = "Pressure"
    ),
    light = list(
      light = list(palette = "OrRd"),
      dark = list(palette = "inferno"),
      name = "map_light",
      display = "Light"
    ),
    magnetic = list(
      light = list(palette = "RdPu"),
      dark = list(palette = "plasma"),
      name = "map_magnetic",
      display = "Magnetic"
    ),
    pressure_mse = list(
      # We are interested in small MSE: reverse so low values are more visible.
      light = list(palette = "BuPu", reverse = TRUE),
      dark = list(palette = "magma", reverse = TRUE),
      name = "map_pressure_mse",
      display = "Pres. MSE"
    ),
    pressure_mask = list(
      light = list(palette = "YlOrBr"),
      dark = list(palette = "YlOrBr"),
      name = "map_pressure_mask",
      display = "Pres. mask"
    ),
    mask_water = list(
      light = list(palette = "Greys"),
      dark = list(palette = "Greys"),
      name = "mask_water",
      display = "Water mask"
    ),
    magnetic_inclination = list(
      light = list(palette = "YlGnBu"),
      dark = list(palette = "viridis"),
      name = "map_magnetic_inclination",
      display = "Mag. incl."
    ),
    magnetic_intensity = list(
      light = list(palette = "YlGn"),
      dark = list(palette = "viridis"),
      name = "map_magnetic_intensity",
      display = "Mag. int."
    ),
    marginal = list(
      light = list(palette = "plasma"),
      dark = list(palette = "plasma"),
      name = "map_marginal",
      display = "Marginal"
    ),
    twilight = list(
      light = list(palette = "OrRd"),
      dark = list(palette = "inferno"),
      name = "map_light_twl",
      display = "Twilight"
    ),
    unknown = list(
      light = list(palette = "viridis"),
      dark = list(palette = "viridis"),
      name = "map_unknown",
      display = "Unknown"
    )
  )
})
