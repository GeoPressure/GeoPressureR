.DEG2RAD <- pi / 180
#' Solar zenith angle using NOAA / Meeus equations
#'
#' Compute solar zenith angles from time and location using the formulation
#' implemented in the NOAA Solar Calculator, itself based on the astronomical
#' algorithms of Meeus.
#'
#' The computation is split internally into three steps:
#' 1. `geolight_solar_constants()`: Time-only solar quantities (solar time and declination)
#' 2. `geolight_solar_zenith()`: Geometric solar zenith angle
#' 3. `geolight_solar_refracted()`: Approximate atmospheric refraction correction
#'
#' ## Conventions
#' - `lon` is expressed in **degrees east** (positive eastward).
#' - `lat` is expressed in **degrees north**.
#' - All angles are in **degrees** unless stated otherwise.
#' - `date` is provided as POSIXct and internally converted to Julian day.
#'
#' When `lat` and `lon` are vectors, the result is a 3-D array with dimensions:
#'
#' `c(length(lat), length(lon), length(date))`
#'
#' When `lat` and `lon` are scalars, the result reduces to a vector of length
#' `length(date)`.
#'
#' ## Solar time and hour angle
#' Internally, the solar time is computed as NOAA's *true solar time* expressed
#' as an angle (degrees) at Greenwich, without longitude adjustment. Longitude
#' enters later through the hour angle:
#'
#' `hour_angle = solar_time + lon - 180`
#'
#' Values are intentionally not wrapped to `[0, 360)` because all downstream
#' trigonometric functions are periodic.
#'
#' ## Atmospheric refraction
#' Atmospheric refraction is applied using NOAA's standard piecewise
#' approximation as a function of solar elevation. This correction is
#' approximate and does not account for local pressure or temperature.
#'
#' @param date POSIXct vector of times.
#' @param lat Numeric vector of latitudes (degrees north).
#' @param lon Numeric vector of longitudes (degrees east).
#'
#' @return Solar zenith angle(s) in degrees, corrected for atmospheric refraction.
#'
#' @references
#' NOAA Global Monitoring Laboratory. Solar Calculation Details.
#' \url{https://gml.noaa.gov/grad/solcalc/calcdetails.html}
#'
#' Meeus, J. (1998). *Astronomical Algorithms*. Willmann-Bell.
#'
#' @family geolight
#' @export
geolight_solar <- function(date, lat, lon) {
  sun <- geolight_solar_constants(date)

  zenith <- geolight_solar_zenith(
    sun = sun,
    lat = lat,
    lon = lon
  )

  geolight_solar_refracted(zenith)
}

#' @rdname geolight_solar
#'
#' @param date POSIXct vector of times.
#'
#' @return A list of time-dependent solar quantities:
#' \itemize{
#'   \item `solar_time`: true solar time angle at Greenwich (degrees)
#'   \item `sin_solar_dec`: sine of solar declination
#'   \item `cos_solar_dec`: cosine of solar declination
#' }
#' @noRd
geolight_solar_constants <- function(date) {
  # Julian day from POSIXct (Unix epoch to JD)
  jd <- as.numeric(date) / 86400 + 2440587.5

  # Julian century (Meeus/NOAA convention)
  jc <- (jd - 2451545) / 36525

  # Geometric mean longitude of the sun (deg)
  l0 <- (280.46646 + jc * (36000.76983 + 0.0003032 * jc)) %% 360

  # Geometric mean anomaly (deg)
  m <- 357.52911 + jc * (35999.05029 - 0.0001537 * jc)

  # Eccentricity of Earth's orbit
  e <- 0.016708634 - jc * (0.000042037 + 0.0000001267 * jc)

  # Sun equation of center (deg)
  eqctr <-
    sin(.DEG2RAD * m) *
    (1.914602 - jc * (0.004817 + 0.000014 * jc)) +
    sin(.DEG2RAD * 2 * m) * (0.019993 - 0.000101 * jc) +
    sin(.DEG2RAD * 3 * m) * 0.000289

  # True longitude (deg)
  lambda0 <- l0 + eqctr

  # Apparent longitude (deg)
  omega <- 125.04 - 1934.136 * jc
  lambda <- lambda0 - 0.00569 - 0.00478 * sin(.DEG2RAD * omega)

  # Mean obliquity of the ecliptic (deg)
  seconds <- 21.448 - jc * (46.815 + jc * (0.00059 - jc * 0.001813))
  obliq0 <- 23 + (26 + seconds / 60) / 60

  # Corrected obliquity (deg)
  obliq <- obliq0 + 0.00256 * cos(.DEG2RAD * omega)

  # Equation of time (minutes)
  y <- tan(.DEG2RAD * obliq / 2)^2
  eqn_time <- 4 /
    .DEG2RAD *
    (y *
      sin(.DEG2RAD * 2 * l0) -
      2 * e * sin(.DEG2RAD * m) +
      4 * e * y * sin(.DEG2RAD * m) * cos(.DEG2RAD * 2 * l0) -
      0.5 * y^2 * sin(.DEG2RAD * 4 * l0) -
      1.25 * e^2 * sin(.DEG2RAD * 2 * m))

  # Declination (radians)
  solar_dec <- asin(sin(.DEG2RAD * obliq) * sin(.DEG2RAD * lambda))

  # "True solar time" angle at Greenwich:
  # fractional day -> minutes, add equation of time, convert minutes to degrees (/4).
  # Not wrapped to [0,360) on purpose; cosine is periodic.
  solar_time <- ((jd - 0.5) %% 1 * 1440 + eqn_time) / 4

  data.frame(
    solar_time = solar_time,
    sin_solar_dec = sin(solar_dec),
    cos_solar_dec = cos(solar_dec)
  )
}


#' @rdname geolight_solar
#'
#' @param sun List returned by `geolight_solar_constants()`.
#' @param lat Numeric vector of latitudes (degrees north).
#' @param lon Numeric vector of longitudes (degrees east).
#' 
#' @return Geometric solar zenith angle(s) in degrees, without refraction.
#' @noRd
geolight_solar_zenith <- function(sun, lat, lon) {
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)

  nlat <- length(lat)
  nlon <- length(lon)
  nT <- length(sun$solar_time)

  sin_lat <- sin(.DEG2RAD * lat)
  cos_lat <- cos(.DEG2RAD * lat)

  # hour angle: nlon x nT
  H <- outer(lon, sun$solar_time, `+`) - 180
  cosH <- cos(.DEG2RAD * H)

  A <- sin_lat %o% sun$sin_solar_dec # nlat x nT
  B <- cos_lat %o% sun$cos_solar_dec # nlat x nT

  z <- array(NA_real_, dim = c(nlat, nlon, nT))

  for (j in seq_len(nlon)) {
    cosz <- A + sweep(B, 2, cosH[j, ], `*`) # correct column-wise scaling
    cosz <- pmin(1, pmax(-1, cosz))
    z[, j, ] <- acos(cosz) / .DEG2RAD
  }

  z
}


#' @rdname geolight_solar
#'
#' @param zenith Numeric array of geometric solar zenith angles (degrees).
#'
#' @return Apparent solar zenith angle(s) in degrees after refraction correction.
#' @noRd
geolight_solar_refracted <- function(zenith) {
  # elevation in degrees
  h <- 90 - zenith
  th <- tan(.DEG2RAD * h)

  r <- numeric(length(h))

  i2 <- h <= 85 & h > 5
  i3 <- h <= 5 & h > -0.575
  i4 <- h <= -0.575

  # NOAA piecewise model, in arcseconds (then /3600 to degrees)
  r[i2] <- 58.1 / th[i2] - 0.07 / th[i2]^3 + 0.000086 / th[i2]^5
  r[i3] <- 1735 + h[i3] * (-518.2 + h[i3] * (103.4 + h[i3] * (-12.79 + 0.711 * h[i3])))
  r[i4] <- -20.774 / th[i4]

  zenith - r / 3600
}
