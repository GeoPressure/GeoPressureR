# short names are translated where they used to be accepted

    Code
      era5_variable_canonical(c("u", "v"), allow_short = TRUE)
    Condition
      Warning:
      Passing NetCDF short names to `variable` was deprecated in GeoPressureR 3.7.0.
      i Use the same names as `tag_download_wind()`, e.g. "u_component_of_wind" rather than "u".
    Output
      [1] "u_component_of_wind" "v_component_of_wind"

# short names are refused where CDS is called

    Code
      era5_variable_canonical(c("u", "v"))
    Condition
      Error in `era5_variable_canonical()`:
      ! `variable` must use the names CDS understands, not NetCDF short names.
      x Received "u" and "v".
      i Use "u_component_of_wind" and "v_component_of_wind".

# unknown names are refused, with a suggestion when close

    Code
      era5_variable_canonical("u_component_of_win")
    Condition
      Error in `era5_variable_canonical()`:
      ! `variable` must be an ERA5 pressure-level variable.
      x Unknown: "u_component_of_win".
      i Did you mean "u_component_of_wind"?
      i Available: "divergence", "fraction_of_cloud_cover", "geopotential", "ozone_mass_mixing_ratio", "potential_vorticity", "relative_humidity", "specific_cloud_ice_water_content", "specific_cloud_liquid_water_content", "specific_humidity", "specific_rain_water_content", "specific_snow_water_content", "temperature", "u_component_of_wind", "v_component_of_wind", "vertical_velocity", and "vorticity".

---

    Code
      era5_variable_canonical("banana")
    Condition
      Error in `era5_variable_canonical()`:
      ! `variable` must be an ERA5 pressure-level variable.
      x Unknown: "banana".
      i Available: "divergence", "fraction_of_cloud_cover", "geopotential", "ozone_mass_mixing_ratio", "potential_vorticity", "relative_humidity", "specific_cloud_ice_water_content", "specific_cloud_liquid_water_content", "specific_humidity", "specific_rain_water_content", "specific_snow_water_content", "temperature", "u_component_of_wind", "v_component_of_wind", "vertical_velocity", and "vorticity".

