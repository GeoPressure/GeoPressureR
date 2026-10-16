#' @section Pressure-derived altitude:
#' When altitude is produced, GeoPressureR uses the same barometric relation as
#' [GeoPressureAPI](https://github.com/GeoPressure/GeoPressureAPI):
#' \deqn{z_\mathrm{tag}=z_\mathrm{ERA5}+\frac{T_\mathrm{ERA5}}{L_b}\left[\left(
#' \frac{P_\mathrm{tag}}{P_\mathrm{ERA5}}\right)^{-R L_b/(g M)}-1\right].}
#' Here, \eqn{P_\mathrm{tag}} is tag pressure, while \eqn{P_\mathrm{ERA5}}, \eqn{T_\mathrm{ERA5}},
#' and \eqn{z_\mathrm{ERA5}}
#' are ERA5 surface pressure, 2 m temperature, and model-surface elevation. Model-surface elevation
#' is obtained from ERA5 surface geopotential divided by standard gravity.
#' `altitude_formula = "virtual"` (default) uses the virtual temperature computed from ERA5 2 m
#' dewpoint, and the temperature lapse rate of the GPT3 empirical troposphere model
#' ([Landskron & Böhm 2018](https://doi.org/10.1007/s00190-017-1066-2)), a mean, annual and
#' semi-annual cycle on a 1° grid, downloaded once from TU Wien and cached.
#' `altitude_formula = "standard"` uses 2 m temperature and \eqn{L_b=-0.0065} K/m.
#' The constants are the universal gas constant
#' \eqn{R=8.31432} J/(mol K), standard gravity \eqn{g=9.80665} m/s², and molar mass of dry air
#' \eqn{M=0.0289644} kg/mol. Pressure is converted to Pa internally and altitude is returned in
#' metres above mean sea level.
#'
#' @section Choosing `era5_dataset`:
#' **Use `era5_dataset = "single-levels"` whenever altitude is computed.** It is the default.
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
#' `"land"` and `"both"` remain supported. They are appropriate for retrieving other variables
#' at 0.1 degree resolution. For altitude, prefer `"single-levels"` based on the
#' [altitude validation](https://github.com/GeoPressure/altitude-validation).
#'
#' @section Accuracy:
#' The [altitude validation](https://github.com/GeoPressure/altitude-validation) evaluates the
#' conversion against weather stations, GNSS and Swiss stations with documented barometer heights,
#' and radiosondes, for a perfect pressure reading with `era5_dataset = "single-levels"` and the
#' default `altitude_formula = "virtual"`:
#' - **Absolute altitude on the ground** is a fixed offset per site: median 2.6 m (p90 11 m), with
#'   a temporal scatter (SD) of 3.7 m around it, growing to tens of metres in steep terrain where
#'   the 0.25° orography departs from the real one.
#' - **Altitude changes at one site** are therefore more reliable than absolute altitudes.
#' - **Height above the ground in flight** is nearly unbiased (−4 m at 1–1.5 km), but its scatter
#'   grows with height (RMSE 20 m at 1–1.5 km and 43 m at 2–3 km). Over the heights birds fly,
#'   the mean absolute error is 13.1 m, against 16.4 m with `"standard"`, which places birds too
#'   low by 1–1.5% of their height.
#'
#' The tag's own sensor adds to these: `altitude` is computed from the raw tag pressure, so an
#' absolute sensor offset of 1 hPa shifts it by about 8 m near the ground.
#'
#' @section A note on accumulated variables:
#' `total_precipitation`, `surface_solar_radiation_downwards` and
#' `surface_thermal_radiation_downwards` do **not** mean the same thing across backends when
#' `era5_dataset = "land"`. GeoPressureAPI serves Earth Engine's ERA5-Land collection, where these
#' bands are overwritten by their `*_hourly` counterparts and are therefore hourly increments.
#' ARCO serves ECMWF's own ERA5-Land fields, which accumulate from 00 UTC. For
#' `era5_dataset = "single-levels"` both are hourly and agree. Take the difference between
#' consecutive hours if you need increments from ARCO over land.
