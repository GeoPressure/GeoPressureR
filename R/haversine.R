#' Calculate great-circle distances
#'
#' Vectorized implementation of the Haversine formula.
#'
#' @param from_coords Matrix of source coordinates (longitude, latitude) in decimal degrees
#' @param to_coords Matrix of target coordinates (longitude, latitude) in decimal degrees
#'
#' @return A vector of distances in kilometers.
#'
#' @keywords internal
#' @export
haversine_distance <- function(from_coords, to_coords) {
  # Convert to radians
  to_rad <- pi / 180

  # Default radius is WGS84 equatorial radius in kilometers
  radius <- 6378.137

  # Extract coordinates (vectorized operations)
  lon1 <- from_coords[, 1] * to_rad
  lat1 <- from_coords[, 2] * to_rad
  lon2 <- to_coords[, 1] * to_rad
  lat2 <- to_coords[, 2] * to_rad

  # Compute differences
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1

  # Haversine formula (vectorized)
  a <- (sin(dlat / 2))^2 + cos(lat1) * cos(lat2) * (sin(dlon / 2))^2
  a <- pmin(a, 1) # Avoid numerical issues from floating point precision

  # Final distance calculation
  dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * radius

  as.vector(dist)
}

#' @noRd
haversine_bearing <- function(from_coords, to_coords) {
  # Convert to radians
  to_rad <- pi / 180

  # Extract coordinates (vectorized operations)
  lon1 <- from_coords[, 1] * to_rad
  lat1 <- from_coords[, 2] * to_rad
  lon2 <- to_coords[, 1] * to_rad
  lat2 <- to_coords[, 2] * to_rad

  # Compute longitude difference
  dlon <- lon2 - lon1

  # Forward azimuth formula
  y <- sin(dlon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)

  # Calculate bearing in radians, then convert to degrees
  bearing_rad <- atan2(y, x)
  bearing_deg <- bearing_rad * 180 / pi

  # Normalize to 0-360°
  bearing_normalized <- (bearing_deg + 360) %% 360

  # Handle NA values (occurs when from_coords == to_coords, i.e., distance = 0)
  # Set bearing to 0° (North) for zero-distance cases
  bearing_normalized[is.na(bearing_normalized)] <- 0

  as.vector(bearing_normalized)
}
