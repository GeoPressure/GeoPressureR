#' Memory-efficient coordinate calculations with automatic chunking
#'
#' @description
#' These functions compute calculations between coordinate pairs with automatic memory management.
#' For large datasets, automatically chunks the calculation to avoid memory issues.
#' For small datasets, processes directly without chunking overhead.
#'
#' - `graph_create_coord_calc()`: Generic chunking function
#' - `graph_create_distance()`: Computes Haversine distances in kilometers
#' - `graph_create_bearing()`: Computes forward azimuth bearings in degrees
#' - `graph_compute_chunk_size()`: Calculates optimal chunk size based on available memory
#'
#' @param from_coords Matrix of source coordinates (longitude, latitude) in decimal degrees
#' @param to_coords Matrix of target coordinates (longitude, latitude) in decimal degrees
#' @param calc_fun Function to apply to coordinate pairs (e.g., haversine_distance,
#' haversine_bearing)
#' @param n_coords Number of coordinate pairs to process
#' @param ... Additional arguments for memory management (for coordinate calculation functions)
#' @param memory_fraction Fraction of installed physical memory to use (default: 0.2 for 20%).
#' This controls the chunk-memory budget, up to `max_memory_mb`.
#' - 0.05 (5%): Very conservative, good for shared systems or when running other processes
#' - 0.10 (10%): Moderate usage, balances performance and system stability
#' - 0.20 (20%): Aggressive usage for dedicated analysis, faster but may impact other apps
#' - 0.50+ (50%+): Only for dedicated high-memory analysis on powerful machines
#' @param min_memory_mb Minimum memory to allocate in MiB (default: 50)
#' @param max_memory_mb Maximum memory to allocate in MiB (default: 1000)
#'
#' @return
#' - `graph_create_distance()`: Vector of distances in kilometers
#' - `graph_create_bearing()`: Vector of bearings in degrees
#' - `graph_compute_chunk_size()`: Integer chunk size
#' @noRd
graph_create_coord_calc <- function(from_coords, to_coords, calc_fun, ...) {
  n_coords <- nrow(from_coords)

  # Determine optimal chunk size based on available memory
  chunk_size <- graph_compute_chunk_size(n_coords, ...)

  # For small datasets, calculate directly without chunking overhead
  if (n_coords <= chunk_size) {
    return(calc_fun(from_coords, to_coords))
  }

  # For large datasets, process in memory-safe chunks
  results <- numeric(n_coords)

  for (chunk_start in seq(1, n_coords, chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_coords)
    chunk_indices <- chunk_start:chunk_end

    # Extract coordinates for this chunk
    from_chunk <- from_coords[chunk_indices, , drop = FALSE]
    to_chunk <- to_coords[chunk_indices, , drop = FALSE]

    # Calculate for this chunk
    results[chunk_indices] <- calc_fun(from_chunk, to_chunk)
  }

  results
}

#' @noRd
graph_create_distance <- function(from_coords, to_coords, ...) {
  graph_create_coord_calc(
    from_coords,
    to_coords,
    haversine_distance,
    ...
  )
}

#' @noRd
graph_create_bearing <- function(from_coords, to_coords, ...) {
  graph_create_coord_calc(
    from_coords,
    to_coords,
    haversine_bearing,
    ...
  )
}

#' @noRd
graph_compute_chunk_size <- function(
  n_coords,
  memory_fraction = 0.2,
  min_memory_mb = 50,
  max_memory_mb = 1000
) {
  # Detect installed physical memory rather than transient free memory so chunk sizes are
  # reproducible across runs on the same machine.
  total_memory_mb <- if (Sys.info()["sysname"] == "Windows") {
    # Query total memory through PowerShell on Windows.
    tryCatch(
      {
        total_mem <- system(
          "powershell -NoProfile -Command \"(Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory\"",
          intern = TRUE
        )
        if (length(total_mem) > 0 && !is.na(as.numeric(total_mem))) {
          as.numeric(total_mem) / (1024^2)
        } else {
          # Use a conservative fallback when the platform query is unavailable.
          8000
        }
      },
      error = function(e) 8000
    )
  } else if (Sys.info()["sysname"] == "Darwin") {
    # Query total memory in bytes on macOS and convert it to MiB.
    tryCatch(
      {
        # Get total memory in bytes, then convert it to MiB.
        total_mem <- system("sysctl -n hw.memsize", intern = TRUE)
        if (length(total_mem) > 0 && !is.na(as.numeric(total_mem))) {
          as.numeric(total_mem) / (1024^2)
        } else {
          # Use a conservative fallback when the platform query is unavailable.
          8000
        }
      },
      error = function(e) 8000
    )
  } else if (Sys.info()["sysname"] == "Linux") {
    # Read total physical memory from /proc/meminfo on Linux (reported in KiB), then convert to MiB.
    tryCatch(
      {
        mem_info <- system(
          "awk '/MemTotal/ {print $2}' /proc/meminfo",
          intern = TRUE
        )
        if (length(mem_info) > 0 && !is.na(as.numeric(mem_info))) {
          as.numeric(mem_info) / 1024
        } else {
          # Use a conservative fallback when /proc/meminfo is unavailable.
          8000
        }
      },
      error = function(e) 8000
    )
  } else {
    8000
  }

  # Calculate memory allocation for chunking with bounds (all values are MiB).
  memory_for_chunk_mb <- max(
    min_memory_mb,
    min(max_memory_mb, total_memory_mb * memory_fraction)
  )

  # Each coordinate pair uses ~500 bytes (a conservative estimate for intermediate calculations).
  coords_per_mb <- 1024^2 / 500 # ~2k coordinate pairs per MiB
  max_chunk_from_memory <- as.integer(memory_for_chunk_mb * coords_per_mb)

  # Ensure a useful minimum chunk size, but never exceed the number of pairs requested.
  chunk_size <- max(100, min(max_chunk_from_memory, n_coords))

  return(chunk_size)
}


#' Calculate great-circle distances
#'
#' Vectorized implementation of the Haversine formula.
#'
#' @param from_coords Matrix of source coordinates (longitude, latitude) in decimal degrees
#' @param to_coords Matrix of target coordinates (longitude, latitude) in decimal degrees
#'
#' @return A vector of distances in kilometers.

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
