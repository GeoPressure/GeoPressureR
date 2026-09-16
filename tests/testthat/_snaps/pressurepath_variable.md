# an unavailable variable names the configuration that would work

    Code
      pressurepath_variable_check(c("surface_pressure", "boundary_layer_height"),
      "api", "land")
    Condition
      Error in `pressurepath_variable_check()`:
      ! Variable "boundary_layer_height" is not available with `source = "api"` and `era5_dataset = "land"`.
      x "boundary_layer_height": available with `era5_dataset = "single-levels"`.
      i See `pressurepath_variable_available()` for the full list (71 variables for this configuration).

---

    Code
      pressurepath_variable_check("snow_cover", "api", "single-levels")
    Condition
      Error in `pressurepath_variable_check()`:
      ! Variable "snow_cover" is not available with `source = "api"` and `era5_dataset = "single-levels"`.
      x "snow_cover": available with `era5_dataset = "land"`.
      i See `pressurepath_variable_available()` for the full list (293 variables for this configuration).

---

    Code
      pressurepath_variable_check(c("altitude", "total_precipitation"), "arco",
      "land")
    Condition
      Error in `pressurepath_variable_check()`:
      ! Variable "total_precipitation" is not available with `source = "arco"` and `era5_dataset = "land"`.
      x "total_precipitation": available with `source = "api"`.
      i See `pressurepath_variable_available()` for the full list (2 variables for this configuration).

# a typo suggests the intended variable

    Code
      pressurepath_variable_check("boundary_layer_heigt", "api", "single-levels")
    Condition
      Error in `pressurepath_variable_check()`:
      ! Variable "boundary_layer_heigt" is not available with `source = "api"` and `era5_dataset = "single-levels"`.
      x "boundary_layer_heigt": unknown variable. Did you mean "boundary_layer_height"?
      i See `pressurepath_variable_available()` for the full list (293 variables for this configuration).

---

    Code
      pressurepath_variable_check("banana", "api", "single-levels")
    Condition
      Error in `pressurepath_variable_check()`:
      ! Variable "banana" is not available with `source = "api"` and `era5_dataset = "single-levels"`.
      x "banana": unknown ERA5 variable.
      i See `pressurepath_variable_available()` for the full list (293 variables for this configuration).

