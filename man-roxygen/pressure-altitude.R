#' @section Pressure-derived altitude:
#' When altitude is produced, GeoPressureR uses the same barometric relation as
#' [GeoPressureAPI](https://github.com/GeoPressure/GeoPressureAPI):
#' \deqn{z_\mathrm{tag}=z_\mathrm{ERA5}+\frac{T_\mathrm{ERA5}}{L_b}\left[\left(
#' \frac{P_\mathrm{tag}}{P_\mathrm{ERA5}}\right)^{-R L_b/(g M)}-1\right].}
#' Here, \eqn{P_\mathrm{tag}} is tag pressure, while \eqn{P_\mathrm{ERA5}}, \eqn{T_\mathrm{ERA5}},
#' and \eqn{z_\mathrm{ERA5}}
#' are ERA5 surface pressure, 2 m temperature, and model-surface elevation. Model-surface elevation
#' is obtained from ERA5 surface geopotential divided by standard gravity. The constants are the
#' standard temperature lapse rate \eqn{L_b=-0.0065} K/m, universal gas constant
#' \eqn{R=8.31432} J/(mol K), standard gravity \eqn{g=9.80665} m/s², and molar mass of dry air
#' \eqn{M=0.0289644} kg/mol. Pressure is converted to Pa internally and altitude is returned in
#' metres above mean sea level.
#'
#' @section Choosing `era5_dataset`:
#' **Use `era5_dataset = "single-levels"` whenever altitude is computed.**
#'
#' ERA5-Land's `surface_pressure` is not the exact hydrostatic image of the orography ERA5-Land
#' publishes as `geopotential` — the two disagree by up to ~10 hPa in steep terrain. The
#' \eqn{z_\mathrm{ERA5}} term therefore fails to cancel from the relation above and the whole discrepancy
#' lands in the retrieved altitude. Measured against 41,653 hourly station-pressure observations
#' from 271 NOAA ISD stations (2–3576 m, Alps, July 2020), compared with surveyed station
#' elevation:
#'
#' | `era5_dataset`      | bias   | MAE    | RMSE   |
#' | ------------------- | ------ | ------ | ------ |
#' | `"single-levels"`   | −0.6 m | 9.0 m  | 27.4 m |
#' | `"land"` / `"both"` | +2.5 m | 55.3 m | 76.9 m |
#'
#' ERA5-Land replays the land component of ERA5 and assimilates no atmospheric observations of its
#' own, so it carries no independent information about absolute altitude. Substituting the
#' orography implied by its own surface pressure removes the error exactly and reproduces
#' `"single-levels"` to 0.1 m.
#'
#' `"land"` and `"both"` remain the defaults for backward compatibility. They are appropriate for
#' retrieving other variables at 0.1 degree resolution, and for [geopressure_map()], whose
#' pressure mismatch is differential and unaffected.
#'
#' @section Accuracy:
#' With `era5_dataset = "single-levels"` the altitude error separates into two parts that behave
#' very differently. A static per-site offset (median 3.7 m, p90 15 m) reflects station metadata
#' and sub-grid terrain and does not vary in time, while the temporal scatter (median SD 3.1 m,
#' p90 7.7 m) is the actual reanalysis error. Expect roughly **3 m for relative altitude changes
#' at a fixed location** and **10 m mean absolute error for absolute altitude**, degrading to tens
#' of metres in steep terrain — where it is a fixed offset rather than noise. Precision is nearly
#' independent of flight altitude: de-biased RMSE stays 2–7 m up to 1000 m above the model surface
#' and about 12 m at 1000–3000 m above it.
#'
#' @section A note on accumulated variables:
#' `total_precipitation`, `surface_solar_radiation_downwards` and
#' `surface_thermal_radiation_downwards` do **not** mean the same thing across backends when
#' `era5_dataset = "land"`. GeoPressureAPI serves Earth Engine's ERA5-Land collection, where these
#' bands are overwritten by their `*_hourly` counterparts and are therefore hourly increments.
#' ARCO serves ECMWF's own ERA5-Land fields, which accumulate from 00 UTC. For
#' `era5_dataset = "single-levels"` both are hourly and agree. Take the difference between
#' consecutive hours if you need increments from ARCO over land.
