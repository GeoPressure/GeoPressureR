#' Create a `graph` object
#'
#' @description
#' This function returns a trellis graph representing the trajectory of a bird based on filtering
#' and pruning the likelihood maps provided.
#'
#' In the final graph, we only keep the likely nodes (i.e., position of the bird at each
#' stationary periods) defined as (1) those whose likelihood value are within the threshold of
#' percentile `thr_likelihood` of the total likelihood map and (2) those which are connected to
#' at least one edge of the previous and next stationary periods requiring an average ground speed
#' lower than `thr_gs` (in km/h).
#'
#' For more details and illustration, see [section 2.2 of Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0004-title)
#' and the [GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory.html#create-the-graph)
#'
#' @param tag a GeoPressureR `tag` object.
#' @param thr_likelihood threshold of percentile (see details).
#' @param thr_gs threshold of groundspeed (km/h)  (see details).
#' @param quiet logical to hide messages about the progress.
#' @param geosphere_dist `r lifecycle::badge("deprecated")` This argument is no longer used.
#' Distance calculations now use a custom memory-efficient Haversine implementation.
#' @param geosphere_bearing `r lifecycle::badge("deprecated")` This argument is no longer used.
#' Bearing calculations now use a custom memory-efficient implementation.
#' @param workers `r lifecycle::badge("deprecated")` This argument is no longer used.
#' Parallel processing has been removed to avoid memory issues.
#' @inheritParams tag2path
#'
#' @return Graph as a list
#' - `s`: source node (index in the 3d grid lat-lon-stap)
#' - `t`: target node (index in the 3d grid lat-lon-stap)
#' - `gs`: average ground speed required to make that transition (km/h) as complex number
#' representing the E-W as real and S-N as imaginary
#' - `obs`: observation model, corresponding to the normalized likelihood in a 3D matrix of size
#' `sz`
#' - `sz`: size of the 3d grid lat-lon-stap
#' - `stap`: data.frame of all stationary periods (same as `tag$stap`)
#' - `equipment`: node(s) of the first stap (index in the 3d grid lat-lon-stap)
#' - `retrieval`: node(s) of the last stap (index in the 3d grid lat-lon-stap)
#' - `mask_water`: logical matrix of water-land
#' - `param`: list of parameters including `thr_likelihood` and `thr_gs` (same as `tag$param`)
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read() |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
#'     ) |>
#'     geopressure_map(quiet = TRUE) |>
#'     geolight_map(quiet = TRUE)
#' })
#'
#' # Create graph
#' graph <- graph_create(tag, thr_likelihood = 0.95, thr_gs = 100, quiet = TRUE)
#'
#' print(graph)
#'
#' @seealso [GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory.html#create-the-graph)
#' @family graph
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' \doi{10.1111/2041-210X.14082}.}
#' @export
graph_create <- function(
  tag,
  thr_likelihood = 0.99,
  thr_gs = 150,
  likelihood = NULL,
  quiet = FALSE,
  geosphere_dist = lifecycle::deprecated(),
  geosphere_bearing = lifecycle::deprecated(),
  workers = lifecycle::deprecated()
) {
  # Handle deprecated arguments
  if (lifecycle::is_present(geosphere_dist)) {
    lifecycle::deprecate_warn(
      "3.4.0",
      "graph_create(geosphere_dist)",
      details = "Distance calculations now use a custom memory-efficient Haversine implementation."
    )
  }

  if (lifecycle::is_present(workers)) {
    lifecycle::deprecate_warn(
      "3.4.0",
      "graph_create(workers)",
      details = "Parallel processing has been removed to avoid memory issues."
    )
  }

  if (lifecycle::is_present(geosphere_bearing)) {
    lifecycle::deprecate_warn(
      "3.4.0",
      "graph_create(geosphere_bearing)",
      details = "Bearing calculations now use a custom memory-efficient implementation."
    )
  }
  assertthat::assert_that(is.numeric(thr_gs))
  assertthat::assert_that(length(thr_gs) == 1)
  assertthat::assert_that(thr_gs >= 0)

  map <- tag_prepare_likelihood(
    tag,
    likelihood = likelihood,
    thr_likelihood = thr_likelihood,
    thr_gs = thr_gs,
    quiet = quiet
  )

  lk_norm <- map$lk_norm
  nds <- map$dist_mask
  likelihood <- map$likelihood
  flight_duration <- map$flight_duration
  resolution <- map$resolution
  sz <- map$sz
  nll <- sz[1] * sz[2]

  stap_include <- which(map$stap$include)

  g <- map_expand(map$extent, map$scale)

  # Create the graph from nds with the exact groundspeed

  # Initialize results list
  n_transitions <- length(nds) - 1
  gr <- vector("list", n_transitions)
  nds_sum <- vapply(nds, sum, numeric(1))

  if (!quiet) {
    cli::cli_progress_done()
    i_s <- 0
    nds_expend_sum <- utils::head(nds_sum, -1) * utils::tail(nds_sum, -1)
    cli::cli_progress_step(
      "Compute the groundspeed for stationary period {i_s}/{n_transitions}: { round(sum(nds_expend_sum[seq_len(i_s)])/sum(nds_expend_sum)*100)}% of transitions done",
      msg_done = "Compute the groundspeed"
    )
  }

  for (i_s in seq_len(n_transitions)) {
    nds_i_s <- which(nds[[i_s]])
    nds_i_s_1 <- which(nds[[i_s + 1]])

    # Pre-compute coordinates for source and target nodes (more efficient)
    s_coords <- arrayInd(nds_i_s, c(sz[1], sz[2]))
    t_coords <- arrayInd(nds_i_s_1, c(sz[1], sz[2]))

    s_lat <- g$lat[s_coords[, 1]]
    s_lon <- g$lon[s_coords[, 2]]
    t_lat <- g$lat[t_coords[, 1]]
    t_lon <- g$lon[t_coords[, 2]]

    # Pre-filter coordinate combinations using rough distance approximation
    if ((length(s_lat) * length(t_lat)) > 10000) {
      # Use rough distance approximation
      max_distance <- thr_gs * flight_duration[i_s] * 1.1 # Add 10% buffer for rough approximation

      # Vectorized pre-filtering using rough distance approximation
      # Create coordinate matrices and compute distances in one go
      s_lat_matrix <- matrix(s_lat, nrow = length(s_lat), ncol = length(t_lat))
      s_lon_matrix <- matrix(s_lon, nrow = length(s_lon), ncol = length(t_lon))
      t_lat_matrix <- matrix(
        t_lat,
        nrow = length(s_lat),
        ncol = length(t_lat),
        byrow = TRUE
      )
      t_lon_matrix <- matrix(
        t_lon,
        nrow = length(s_lon),
        ncol = length(t_lon),
        byrow = TRUE
      )

      # Compute rough distances and filter in one step
      lat_diff <- abs(t_lat_matrix - s_lat_matrix) * 111.32
      lon_diff <- abs(t_lon_matrix - s_lon_matrix) *
        111.32 *
        cos((s_lat_matrix + t_lat_matrix) * pi / 360)
      rough_valid_matrix <- sqrt(lat_diff^2 + lon_diff^2) < max_distance

      # Extract coordinates for only valid combinations
      valid_indices <- which(rough_valid_matrix, arr.ind = TRUE)
      from_coords <- cbind(s_lon[valid_indices[, 1]], s_lat[valid_indices[, 1]])
      to_coords <- cbind(t_lon[valid_indices[, 2]], t_lat[valid_indices[, 2]])
      combinations <- data.frame(
        s_idx = valid_indices[, 1],
        t_idx = valid_indices[, 2]
      )

      # Clean up large matrices immediately (memory management handled here)
      rm(
        s_lat_matrix,
        s_lon_matrix,
        t_lat_matrix,
        t_lon_matrix,
        lat_diff,
        lon_diff,
        rough_valid_matrix,
        valid_indices
      )
      gc() # Force garbage collection
    } else {
      # Direct approach for smaller combinations (no pre-filtering)
      combinations <- expand.grid(
        s_idx = seq_along(s_lat),
        t_idx = seq_along(t_lat)
      )
      from_coords <- cbind(s_lon[combinations$s_idx], s_lat[combinations$s_idx])
      to_coords <- cbind(t_lon[combinations$t_idx], t_lat[combinations$t_idx])
    }

    # Clean up coordinate vectors and force garbage collection
    rm(s_lat, s_lon, t_lat, t_lon, t_coords)
    gc()

    # Compute the exact groundspeed for remaining transitions
    # Use memory-efficient distance calculation with automatic method selection
    gs_abs <- graph_create_distance(from_coords, to_coords) /
      flight_duration[i_s]

    # Filter the transition based on the groundspeed
    id <- gs_abs < thr_gs

    # Check that at least one transition exist
    if (sum(id) == 0) {
      # The minimal distance between grid cell is not from the center of the cell, but from one
      # edge to the other (opposite) edge. So the minimal distance between cell should be reduce
      # by the grid resolution. We still want to keep the distance 0 only to the actual same
      # pixel, so we make the distance at a minimum of 1 if initial distance is greater than 1.

      # Extract resolution values for source coordinates where distance > 0
      gs_abs_gt_0 <- gs_abs > 0
      source_indices <- combinations$s_idx[gs_abs_gt_0]
      source_coords_subset <- s_coords[source_indices, , drop = FALSE]
      # Extract resolution for each source coordinate (lat, lon)
      resolution_values <- resolution[cbind(
        source_coords_subset[, 1],
        source_coords_subset[, 2]
      )]
      gs_abs[gs_abs_gt_0] <- pmax(
        gs_abs[gs_abs_gt_0] - resolution_values / flight_duration[i_s],
        1
      )

      id <- gs_abs < thr_gs

      if (sum(id) == 0) {
        cli::cli_abort(c(
          x = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the exact distance of
            edges, there are not any node combinaison possible between stationary period
            {.val {stap_include[i_s]}} and {.val {stap_include[i_s + 1]}}.",
          ">" = "Check flight duration, likelihood map (and labeling) as well as grid resolution."
        ))
      } else {
        cli::cli_warn(c(
          "!" = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the exact distance
            of edges, there are not any node combinaison possible between stationary period
            {.val {stap_include[i_s]}} and {.val {stap_include[i_s + 1]}}.",
          "i" = "We modified the distance by using the minimal distance between cell rather than
            the distance between the center to fix this issue.",
          ">" = "Consider using a grid with a higher resolution."
        ))
      }
    }

    # Compute the bearing of the trajectory
    gs_bearing <- graph_create_bearing(
      from_coords[id, , drop = FALSE],
      to_coords[id, , drop = FALSE]
    )

    # Convert bearing to radians and adjust for GeoPressureR convention
    # GeoPressureR uses 0° = North, 90° = East
    gs_bearing <- ((450 - gs_bearing) %% 360) * pi / 180

    # Create the final result with proper node indices
    gr[[i_s]] <- data.frame(
      s = as.integer(nds_i_s[combinations$s_idx[id]] + (i_s - 1) * nll),
      t = as.integer(nds_i_s_1[combinations$t_idx[id]] + i_s * nll),
      gs = gs_abs[id] * cos(gs_bearing) + 1i * gs_abs[id] * sin(gs_bearing)
    )

    # Clean up remaining variables from this iteration
    rm(gs_bearing, from_coords, to_coords, s_coords)
    gc()

    if (!quiet) {
      cli::cli_progress_update()
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  # Prune
  gr <- graph_create_prune(gr, quiet = quiet)

  if (!quiet) {
    cli::cli_progress_step("Format graph output", msg_done = "Graph formatted")
  }

  # Convert gr to a graph list using pre-allocation for efficiency
  total_rows <- sum(sapply(gr, nrow))

  # Pre-allocate vectors
  s_vec <- integer(total_rows)
  t_vec <- integer(total_rows)
  gs_vec <- complex(total_rows)

  # Fill vectors in chunks
  start_idx <- 1
  for (i in seq_along(gr)) {
    end_idx <- start_idx + nrow(gr[[i]]) - 1
    s_vec[start_idx:end_idx] <- gr[[i]]$s
    t_vec[start_idx:end_idx] <- gr[[i]]$t
    gs_vec[start_idx:end_idx] <- gr[[i]]$gs
    start_idx <- end_idx + 1
  }

  # Create graph list with proper class
  graph <- structure(
    list(s = s_vec, t = t_vec, gs = gs_vec),
    class = "graph"
  )

  # Add observation model as matrix
  graph$obs <- do.call(c, lk_norm)
  dim(graph$obs) <- sz

  # Add metadata information
  graph$sz <- sz
  graph$stap <- map$stap
  graph$equipment <- which(nds[[1]])
  graph$retrieval <- as.integer(which(nds[[sz[3]]]) + (sz[3] - 1) * nll)
  # After pruning some retrieval nodes might not be present anymore.
  graph$retrieval <- graph$retrieval[graph$retrieval %in% graph$t]
  graph$mask_water <- tag$map_pressure$mask_water

  # Create the param from tag
  graph$param <- tag$param
  graph$param$graph_create <- list(
    thr_likelihood = thr_likelihood,
    thr_gs = thr_gs,
    likelihood = likelihood
  )

  # Check graph validity
  assertthat::assert_that(all(
    graph$s[!(graph$s %in% graph$equipment)] %in% graph$t
  ))
  assertthat::assert_that(all(graph$equipment %in% graph$s))
  assertthat::assert_that(all(graph$retrieval %in% graph$t))

  if (!quiet) {
    cli::cli_progress_done()
  }

  return(graph)
}


#' Prune a graph
#'
#' Pruning consists in removing "dead branch" of a graph, that is removing the edges which are not
#' connected to both the source (i.e, equipment) or sink (i.e. retrieval site).
#'
#' @param gr graph constructed with [`graph_create()`].
#' @return graph prunned
#' @family graph
#' @noRd
graph_create_prune <- function(gr, quiet = FALSE) {
  if (length(gr) < 2) {
    return(gr)
  }

  if (!quiet) {
    i <- 0
    cli::cli_progress_step(
      "Pruning the graph: {i}/{(length(gr) - 1) * 2} transitions (forward and backward).",
      msg_done = "Graph pruned"
    )
  }

  # First, trim the graph from equipment to retrieval
  for (i_s in seq(2, length(gr))) {
    # Select the source id which exist in the target of the previous stationary period.
    s <- unique(gr[[i_s]]$s)
    t_b <- unique(gr[[i_s - 1]]$t)
    unique_s_new <- s[s %in% t_b]

    # Keep the edge from which the source id was found in the previous step
    id <- gr[[i_s]]$s %in% unique_s_new
    gr[[i_s]] <- gr[[i_s]][id, ]

    if (nrow(gr[[i_s]]) == 0) {
      cli::cli_abort(c(
        "x" = "Triming the graph killed it at stationary period {.val {i_s}} moving forward."
      ))
    }
    if (!quiet) {
      i <- i_s
      cli::cli_progress_update()
    }
  }
  # Then, trim the graph from retrieval to equipment
  for (i_s in seq(length(gr) - 1, 1)) {
    t <- unique(gr[[i_s]]$t)
    s_a <- unique(gr[[i_s + 1]]$s)
    unique_t_new <- t[t %in% s_a]

    id <- gr[[i_s]]$t %in% unique_t_new
    gr[[i_s]] <- gr[[i_s]][id, ]

    if (nrow(gr[[i_s]]) == 0) {
      cli::cli_abort(c(
        "x" = "Triming the graph killed it at stationary period {.val {i_s}} moving backward"
      ))
    }
    if (!quiet) {
      i <- length(gr) * 2 - i_s
      cli::cli_progress_update()
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  return(gr)
}
