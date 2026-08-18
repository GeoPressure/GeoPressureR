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
#' and the [GeoPressureManual](https://geopressure.org/GeoPressureManual/trajectory.html#create-the-graph)
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
#' @seealso [GeoPressureManual](https://geopressure.org/GeoPressureManual/trajectory.html#create-the-graph)
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
  # Bound temporary pair arrays to an approximately 250 MiB working set.
  max_pairs_per_block <- 1048576L # 250 bytes per candidate pair

  if (!quiet) {
    cli::cli_progress_done()
    i_s <- 0
    nds_expend_sum <- utils::head(nds_sum, -1) * utils::tail(nds_sum, -1)
    n_pairs_done <- 0
    n_pairs_total <- sum(nds_expend_sum)
    cli::cli_progress_step(
      "Compute the groundspeed for stationary period {i_s}/{n_transitions}: {round(n_pairs_done / n_pairs_total * 100)}% of candidate pairs processed",
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

    # Use one vectorized calculation when the complete transition is small enough.
    n_pairs <- as.double(length(s_lat)) * length(t_lat)
    direct <- n_pairs <= 10000
    sources_per_block <- if (direct) {
      length(s_lat)
    } else {
      min(length(s_lat), max_pairs_per_block)
    }
    targets_per_block <- if (direct) {
      length(t_lat)
    } else {
      min(length(t_lat), max(1L, max_pairs_per_block %/% sources_per_block))
    }
    source_starts <- seq.int(1L, length(s_lat), by = sources_per_block)
    target_starts <- seq.int(1L, length(t_lat), by = targets_per_block)
    n_blocks <- length(source_starts) * length(target_starts)
    edge_blocks <- vector("list", n_blocks)
    fallback_blocks <- vector("list", n_blocks)
    has_valid_edge <- FALSE
    i_block <- 0L

    # Process target blocks in order so final edge ordering stays unchanged.
    for (target_start in target_starts) {
      target_idx <- seq.int(
        target_start,
        min(target_start + targets_per_block - 1L, length(t_lat))
      )
      for (source_start in source_starts) {
        i_block <- i_block + 1L
        source_idx <- seq.int(
          source_start,
          min(source_start + sources_per_block - 1L, length(s_lat))
        )

        if (direct) {
          combinations <- expand.grid(s_idx = source_idx, t_idx = target_idx)
        } else {
          # Apply the inexpensive rough-distance filter before allocating exact-distance inputs.
          max_distance <- thr_gs * flight_duration[i_s] * 1.1
          s_lat_matrix <- matrix(
            s_lat[source_idx],
            nrow = length(source_idx),
            ncol = length(target_idx)
          )
          s_lon_matrix <- matrix(
            s_lon[source_idx],
            nrow = length(source_idx),
            ncol = length(target_idx)
          )
          t_lat_matrix <- matrix(
            t_lat[target_idx],
            nrow = length(source_idx),
            ncol = length(target_idx),
            byrow = TRUE
          )
          t_lon_matrix <- matrix(
            t_lon[target_idx],
            nrow = length(source_idx),
            ncol = length(target_idx),
            byrow = TRUE
          )
          lat_diff <- abs(t_lat_matrix - s_lat_matrix) * 111.32
          lon_diff <- abs(t_lon_matrix - s_lon_matrix) *
            111.32 *
            cos((s_lat_matrix + t_lat_matrix) * pi / 360)
          rough_valid_matrix <- sqrt(lat_diff^2 + lon_diff^2) < max_distance
          valid_indices <- which(rough_valid_matrix, arr.ind = TRUE)
          combinations <- data.frame(
            s_idx = source_idx[valid_indices[, 1]],
            t_idx = target_idx[valid_indices[, 2]]
          )
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
        }

        if (nrow(combinations) == 0) {
          if (!quiet) {
            n_pairs_done <- n_pairs_done + length(source_idx) * length(target_idx)
            cli::cli_progress_update()
          }
          next
        }

        # Calculate exact speed and bearing only for pairs surviving this block's rough filter.
        from_coords <- cbind(s_lon[combinations$s_idx], s_lat[combinations$s_idx])
        to_coords <- cbind(t_lon[combinations$t_idx], t_lat[combinations$t_idx])
        gs_abs <- haversine_distance(from_coords, to_coords) / flight_duration[i_s]
        id <- gs_abs < thr_gs

        if (any(id)) {
          has_valid_edge <- TRUE
          fallback_blocks <- NULL
          gs_bearing <- haversine_bearing(
            from_coords[id, , drop = FALSE],
            to_coords[id, , drop = FALSE]
          )
          gs_bearing <- ((450 - gs_bearing) %% 360) * pi / 180
          edge_blocks[[i_block]] <- data.frame(
            s = as.integer(nds_i_s[combinations$s_idx[id]] + (i_s - 1) * nll),
            t = as.integer(nds_i_s_1[combinations$t_idx[id]] + i_s * nll),
            gs = gs_abs[id] * cos(gs_bearing) + 1i * gs_abs[id] * sin(gs_bearing)
          )
        } else if (!has_valid_edge) {
          fallback_gs <- gs_abs
          gs_abs_gt_0 <- gs_abs > 0
          source_coords_subset <- s_coords[combinations$s_idx[gs_abs_gt_0], , drop = FALSE]
          resolution_values <- resolution[cbind(
            source_coords_subset[, 1],
            source_coords_subset[, 2]
          )]
          fallback_gs[gs_abs_gt_0] <- pmax(
            fallback_gs[gs_abs_gt_0] - resolution_values / flight_duration[i_s],
            1
          )
          fallback_id <- fallback_gs < thr_gs
          fallback_blocks[[i_block]] <- data.frame(
            s_idx = combinations$s_idx[fallback_id],
            t_idx = combinations$t_idx[fallback_id],
            gs = fallback_gs[fallback_id]
          )
        }

        if (!quiet) {
          n_pairs_done <- n_pairs_done + length(source_idx) * length(target_idx)
          cli::cli_progress_update()
        }
      }
    }

    if (has_valid_edge) {
      # Avoid copying the edge data when this transition used only one block.
      gr[[i_s]] <- if (n_blocks == 1) edge_blocks[[1]] else do.call(rbind, edge_blocks)
    } else {
      # Apply the existing cell-edge fallback only after all blocks are known to lack an edge.
      fallback <- do.call(rbind, fallback_blocks)
      if (is.null(fallback) || nrow(fallback) == 0) {
        cli::cli_abort(c(
          x = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the exact distance of
            edges, there are not any node combinaison possible between stationary period
            {.val {stap_include[i_s]}} and {.val {stap_include[i_s + 1]}}.",
          ">" = "Check flight duration, likelihood map (and labeling) as well as grid resolution."
        ))
      }

      cli::cli_warn(c(
        "!" = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the exact distance
          of edges, there are not any node combinaison possible between stationary period
          {.val {stap_include[i_s]}} and {.val {stap_include[i_s + 1]}}.",
        "i" = "We modified the distance by using the minimal distance between cell rather than
          the distance between the center to fix this issue.",
        ">" = "Consider using a grid with a higher resolution."
      ))
      gs_bearing <- graph_create_bearing(
        cbind(s_lon[fallback$s_idx], s_lat[fallback$s_idx]),
        cbind(t_lon[fallback$t_idx], t_lat[fallback$t_idx])
      )
      gs_bearing <- ((450 - gs_bearing) %% 360) * pi / 180
      gr[[i_s]] <- data.frame(
        s = as.integer(nds_i_s[fallback$s_idx] + (i_s - 1) * nll),
        t = as.integer(nds_i_s_1[fallback$t_idx] + i_s * nll),
        gs = fallback$gs * cos(gs_bearing) + 1i * fallback$gs * sin(gs_bearing)
      )
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
  total_rows <- sum(vapply(gr, nrow, integer(1)))

  # Pre-allocate vectors
  s_vec <- integer(total_rows)
  t_vec <- integer(total_rows)
  gs_vec <- complex(total_rows)

  # Fill vectors in chunks
  start_idx <- 1
  for (i in seq_along(gr)) {
    gr_i <- gr[[i]]
    end_idx <- start_idx + nrow(gr_i) - 1
    s_vec[start_idx:end_idx] <- gr_i$s
    t_vec[start_idx:end_idx] <- gr_i$t
    gs_vec[start_idx:end_idx] <- gr_i$gs
    gr[i] <- list(NULL)
    rm(gr_i)
    start_idx <- end_idx + 1
  }
  rm(gr)
  gc()

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
