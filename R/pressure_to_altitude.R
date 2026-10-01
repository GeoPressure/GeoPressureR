pressure_to_altitude <- function(
  pressure_tag,
  surface_pressure,
  temperature,
  elevation,
  dewpoint = NULL,
  lat = NULL,
  date = NULL,
  altitude_formula = "virtual"
) {
  lapse <- -0.0065
  if (altitude_formula == "virtual") {
    # Match GeoPressureAPI PR #32, based on https://github.com/GeoPressure/altitude-validation.
    dewpoint <- dewpoint - 273.15
    vapour_pressure <- 611.2 * exp(17.67 * dewpoint / (dewpoint + 243.5))
    temperature <- temperature *
      (1 + 0.608 * 0.622 * vapour_pressure / (surface_pressure - 0.378 * vapour_pressure))
    date <- as.POSIXlt(date, tz = "UTC")$yday + 1
    date <- ifelse(rep_len(lat < 0, max(length(lat), length(date))), (date + 182) %% 365 + 1, date)
    lat <- abs(lat) / 90
    lapse <- pmin(
      pmax(
        -6.6306 + 3.7168 * lat + (-0.0744 + 3.1974 * lat) * cos(2 * pi * (date - 15) / 365.25),
        -9.5
      ),
      -2
    ) /
      1000
  }
  elevation +
    temperature /
      lapse *
      ((pressure_tag / surface_pressure)^(-8.31432 * lapse / 9.80665 / 0.0289644) - 1)
}
