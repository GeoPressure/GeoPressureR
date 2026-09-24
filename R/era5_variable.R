#' ERA5 pressure-level variable registry
#'
#' CDS accepts long names (`"u_component_of_wind"`), but the NetCDF it returns stores the arrays
#' under GRIB short names (`"u"`). [tag_download_wind()] therefore speaks one vocabulary and
#' [edge_add_wind()] used to speak the other, with the mapping between them living nowhere. This
#' table is that mapping, so both halves of the wind pipeline take the same names.
#'
#' Verified against a CDS download of all sixteen variables (2026-09-16).
#'
#' @return A data.frame with `variable` (the name CDS takes) and `short_name` (the name the
#'   NetCDF uses).
#' @noRd
era5_variable_pressure_level <- function() {
  data.frame(
    variable = c(
      "divergence",
      "fraction_of_cloud_cover",
      "geopotential",
      "ozone_mass_mixing_ratio",
      "potential_vorticity",
      "relative_humidity",
      "specific_cloud_ice_water_content",
      "specific_cloud_liquid_water_content",
      "specific_humidity",
      "specific_rain_water_content",
      "specific_snow_water_content",
      "temperature",
      "u_component_of_wind",
      "v_component_of_wind",
      "vertical_velocity",
      "vorticity"
    ),
    short_name = c(
      "d",
      "cc",
      "z",
      "o3",
      "pv",
      "r",
      "ciwc",
      "clwc",
      "q",
      "crwc",
      "cswc",
      "t",
      "u",
      "v",
      "w",
      "vo"
    ),
    stringsAsFactors = FALSE
  )
}

#' Resolve pressure-level variables to their canonical long names
#'
#' @param variable Character vector of variable names.
#' @param allow_short Whether GRIB short names are accepted as deprecated aliases. `TRUE` for
#'   readers that historically took them, `FALSE` for anything that talks to CDS, which only
#'   understands long names.
#' @param arg Name of the calling argument, used in messages.
#' @return `variable` with any short name replaced by its long equivalent.
#' @noRd
era5_variable_canonical <- function(variable, allow_short = FALSE, arg = "variable") {
  tbl <- era5_variable_pressure_level()
  assertthat::assert_that(is.character(variable))

  is_short <- variable %in% tbl$short_name & !(variable %in% tbl$variable)
  if (any(is_short)) {
    if (!allow_short) {
      cli::cli_abort(c(
        "{.arg {arg}} must use the names CDS understands, not NetCDF short names.",
        "x" = "Received {.val {variable[is_short]}}.",
        "i" = "Use {.val {tbl$variable[match(variable[is_short], tbl$short_name)]}}."
      ))
    }
    lifecycle::deprecate_warn(
      when = "3.7.0",
      what = I(sprintf("Passing NetCDF short names to `%s`", arg)),
      details = c(
        "i" = "Use the same names as `tag_download_wind()`, e.g. \"u_component_of_wind\" \\
               rather than \"u\"."
      ),
      id = "era5_variable_short_name"
    )
    variable[is_short] <- tbl$variable[match(variable[is_short], tbl$short_name)]
  }

  unknown <- setdiff(variable, tbl$variable)
  if (length(unknown) > 0) {
    near <- tbl$variable[utils::adist(unknown[1], tbl$variable) <= 3]
    cli::cli_abort(c(
      "{.arg {arg}} must be {cli::qty(length(unknown))}{?an/} ERA5 pressure-level variable{?s}.",
      "x" = "Unknown: {.val {unknown}}.",
      if (length(near) > 0) c("i" = "Did you mean {.val {near[1]}}?"),
      "i" = "Available: {.val {tbl$variable}}."
    ))
  }
  variable
}

#' Translate canonical variable names to the NetCDF short names
#'
#' @param variable Canonical long names, as returned by `era5_variable_canonical()`.
#' @return The matching GRIB short names, in the same order.
#' @noRd
era5_variable_short <- function(variable) {
  tbl <- era5_variable_pressure_level()
  tbl$short_name[match(variable, tbl$variable)]
}
