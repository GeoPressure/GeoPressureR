pressure_to_altitude <- function(
  pressure_tag,
  surface_pressure,
  temperature,
  elevation,
  dewpoint = NULL,
  lon = NULL,
  lat = NULL,
  date = NULL,
  altitude_formula = "virtual"
) {
  lapse <- -0.0065
  if (altitude_formula == "virtual") {
    # Virtual temperature and GPT3 lapse rate (https://github.com/GeoPressure/altitude-validation)
    dewpoint <- dewpoint - 273.15
    vapour_pressure <- 611.2 * exp(17.67 * dewpoint / (dewpoint + 243.5))
    temperature <- temperature *
      (1 + 0.608 * 0.622 * vapour_pressure / (surface_pressure - 0.378 * vapour_pressure))
    # GPT3 lapse rates are positive under inversions and can cross 0, where the formula is 0/0.
    lapse <- gpt3_lapse_rate(lon, lat, date)
    lapse[abs(lapse) < 1e-5] <- -1e-5
  }
  elevation +
    temperature /
      lapse *
      ((pressure_tag / surface_pressure)^(-8.31432 * lapse / 9.80665 / 0.0289644) - 1)
}
