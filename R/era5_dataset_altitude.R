#' Warn when altitude is requested from an ERA5 product that cannot support it
#'
#' ERA5-Land's `surface_pressure` is not the exact hydrostatic image of the orography
#' ERA5-Land publishes as `geopotential`: the two disagree by up to ~10 hPa in steep terrain.
#' The orography term therefore fails to cancel out of the barometric relation and the whole
#' discrepancy lands in the retrieved altitude. Validated against 41,653 hourly station-pressure
#' observations from 271 NOAA ISD stations (2-3576 m, Alps, July 2020), mean absolute error is
#' 9 m for `"single-levels"` against 55 m for `"land"`.
#'
#' ERA5-Land assimilates no atmospheric observations of its own -- it replays the land component
#' of ERA5 -- so it carries no independent information about absolute altitude. It remains the
#' right choice for [geopressure_map()], whose mismatch is differential, and for retrieving
#' variables other than `"altitude"` at 0.1 degree resolution.
#'
#' @param era5_dataset The product actually used.
#' @param altitude Whether altitude is being computed.
#' @return Invisibly `TRUE` when a deprecation warning was signalled.
#' @noRd
era5_dataset_deprecate_altitude <- function(era5_dataset, altitude) {
  if (!isTRUE(altitude) || !era5_dataset %in% c("land", "both")) {
    return(invisible(FALSE))
  }
  lifecycle::deprecate_warn(
    when = "3.7.0",
    what = I(sprintf(
      'Computing `"altitude"` with `era5_dataset = "%s"`',
      era5_dataset
    )),
    with = I('`era5_dataset = "single-levels"`'),
    details = c(
      "*" = "ERA5-Land surface pressure is not hydrostatically consistent with ERA5-Land \\
             orography, so the orography term does not cancel from the barometric relation.",
      "*" = "Against station barometers the mean absolute error is 55 m, against 9 m for \\
             'single-levels', reaching several hundred metres in steep terrain.",
      "i" = "'land' and 'both' remain supported for variables other than 'altitude', and \\
             for geopressure_map()."
    ),
    id = paste0("era5_dataset_altitude_", era5_dataset)
  )
  invisible(TRUE)
}
