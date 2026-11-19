# Stapelev plot helpers. The proposal algorithm lives in the package (R/stapelev.R).

stapelev_proposal <- getFromNamespace("stapelev_proposal", "GeoPressureR")
stapelev_best_offset <- getFromNamespace("stapelev_best_offset", "GeoPressureR")
stapelev_daylight_keep <- getFromNamespace("stapelev_daylight_keep", "GeoPressureR")
stapelev_assign_offsets <- getFromNamespace("stapelev_assign_offsets", "GeoPressureR")
stapelev_eligible <- getFromNamespace("stapelev_eligible", "GeoPressureR")
stapelev_fit_offsets <- getFromNamespace("stapelev_fit_offsets", "GeoPressureR")
stapelev_initialize_offsets <- getFromNamespace("stapelev_initialize_offsets", "GeoPressureR")

# Compress day/night into intervals with interpolated threshold crossings.
stapelev_solar_strip <- function(date, solar_elevation, solar_angle = 0) {
  time <- as.numeric(date)
  day <- solar_elevation >= solar_angle
  i <- which(diff(day) != 0L)
  crossing <- time[i] +
    (solar_angle - solar_elevation[i]) /
      (solar_elevation[i + 1L] - solar_elevation[i]) *
      (time[i + 1L] - time[i])
  edges <- unique(c(time[1], crossing, time[length(time)]))
  middle <- (head(edges, -1L) + tail(edges, -1L)) / 2
  day <- stats::approx(time, solar_elevation, xout = middle, rule = 2)$y >= solar_angle
  date <- as.POSIXct(edges, origin = "1970-01-01", tz = "UTC")
  list(
    x = date,
    z = matrix(as.integer(day), nrow = 1),
    text = matrix(
      glue::glue(
        "{ifelse(day, 'Day', 'Night')} · threshold {solar_angle}°<br>",
        "{format(head(date, -1L), '%d %b %H:%M', tz = 'UTC')}–",
        "{format(tail(date, -1L), '%d %b %H:%M', tz = 'UTC')} UTC"
      ),
      nrow = 1
    )
  )
}


stapelev_query_label <- function(bundle, i) {
  lat_suffix <- if (bundle$returned_lat >= 0) "N" else "S"
  lon_suffix <- if (bundle$returned_lon >= 0) "E" else "W"
  requested <- format(bundle$requested_at, "%Y-%m-%d %H:%M UTC", tz = "UTC")
  glue::glue(
    "{i} · {sprintf('%.3f', abs(bundle$returned_lat))}° {lat_suffix}, ",
    "{sprintf('%.3f', abs(bundle$returned_lon))}° {lon_suffix} · {requested}"
  )
}

stapelev_fitted_data <- function(proposal) {
  hour <- floor(as.numeric(proposal$date) / 3600) * 3600
  i <- !duplicated(hour)
  data.frame(
    date = as.POSIXct(hour[i], origin = "1970-01-01", tz = "UTC"),
    surface_pressure = proposal$surface_pressure[i]
  )
}

stapelev_ribbon_y <- function(fitted, range) {
  if (!length(fitted)) {
    return(fitted)
  }
  lower <- fitted - range
  c(lower, lower[length(lower)], rev(fitted + range))
}

stapelev_ribbon_x <- function(date) {
  if (!length(date)) {
    return(date)
  }
  c(date, date[length(date)], rev(date))
}

stapelev_level_data <- function(fitted_data, window) {
  if (!all(is.finite(window))) {
    return(fitted_data[FALSE, ])
  }
  date_num <- as.numeric(fitted_data$date)
  x <- sort(unique(c(
    window[1],
    date_num[date_num > window[1] & date_num < window[2]],
    window[2]
  )))
  data.frame(
    date = as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
    surface_pressure = stats::approx(
      date_num,
      fitted_data$surface_pressure,
      xout = x,
      rule = 2
    )$y
  )
}
