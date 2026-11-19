#' Compute windspeed and airspeed on a `graph`
#'
#' @description
#' Reads the downloaded NetCDF files and interpolates the average windspeed experienced by the
#' bird on each possible edge, as well as the corresponding airspeed.
#'
#' In addition, the graph can be further pruned based on a threshold of airspeed `thr_as`.
#'
#' `pressure` (the tag pressure, e.g. `tag$pressure`) is required to interpolate the wind at the
#' flight altitude.
#'
#' With `constant_speed = "groundspeed"` (default), the wind is averaged at the nearest ERA5 grid
#' cell along the straight edge flown at constant groundspeed. With `constant_speed = "airspeed"`,
#' the bird flies a constant air velocity (speed and heading), so its track is curved by the wind
#' between the two nodes. This track is approximated by the straight edge plus the drift by the
#' wind anomaly along it, and the wind is averaged along that track. This mostly matters for long
#' flights (> 8 h) and takes about 1.5–3 times longer. The choice is stored in `graph$param` and
#' used by default in [edge_add_wind()]. To keep the graph fast, you can also use the default here
#' and compute the wind along a path with `edge_add_wind(constant_speed = "airspeed")`.
#'
#' See section [2.2.4 in Nussbaumer et al. (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0008-title)
#' for more technical details and the [GeoPressureManual](
#' https://geopressure.org/GeoPressureManual/trajectory-with-wind.html) for an
#' illustration on how to use it.
#'
#' @param graph a GeoPressureR `graph` object.
#' @param thr_as threshold of airspeed (km/h).
#' @param variable `r lifecycle::badge("deprecated")` No longer used.
#'   `graph_add_wind()` always uses `c("u", "v")`.
#' @param constant_speed speed held constant during each flight: `"groundspeed"` (straight edge)
#'   or `"airspeed"` (track drifted by the wind).
#' @inheritParams edge_add_wind
#' @inheritDotParams edge_add_wind -graph -edge_s -edge_t -variable -return_averaged_variable
#'   -interp_spatial_linear -constant_speed
#'
#' @return A `graph` object with windspeed and airspeed as `ws` and `as` respectively.
#'
#' @examplesIf FALSE
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(extent = c(-16, 23, 0, 50), scale = 1) |>
#'     geopressure_map(quiet = TRUE)
#' })
#' graph <- graph_create(tag, quiet = TRUE)
#' graph <- graph_add_wind(graph, pressure = tag$pressure, quiet = TRUE)
#'
#' @family graph
#' @references{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
#' Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
#' optimized hidden Markov model. *Methods in Ecology and Evolution*, 14, 1118–1129
#' \doi{10.1111/2041-210X.14082}.}
#' @seealso [GeoPressureManual](
#' https://geopressure.org/GeoPressureManual/trajectory-with-wind.html)
#' @export
# nocov start
graph_add_wind <- function(
  graph,
  thr_as = Inf,
  variable = lifecycle::deprecated(),
  constant_speed = c("groundspeed", "airspeed"),
  interp_spatial_linear = lifecycle::deprecated(),
  ...
) {
  graph_assert(graph, "full")
  if (!("pressure" %in% names(list(...)))) {
    cli::cli_abort(c(
      "x" = "{.arg pressure} is required to interpolate wind at flight altitude.",
      ">" = "Use {.code graph_add_wind(graph, pressure = tag$pressure)}."
    ))
  }
  assertthat::assert_that(is.numeric(thr_as))
  assertthat::assert_that(length(thr_as) == 1)
  assertthat::assert_that(thr_as >= 0)
  constant_speed <- match.arg(constant_speed)
  if (lifecycle::is_present(interp_spatial_linear)) {
    lifecycle::deprecate_warn(
      "3.9.0",
      "graph_add_wind(interp_spatial_linear)",
      details = "The nearest ERA5 grid cell is always used."
    )
  }
  if (lifecycle::is_present(variable)) {
    lifecycle::deprecate_warn(
      "3.5.4",
      "graph_add_wind(variable)",
      details = "{.fun graph_add_wind} now always uses the two wind components."
    )
    wind <- c("u_component_of_wind", "v_component_of_wind")
    if (!identical(era5_variable_canonical(variable, allow_short = TRUE), wind)) {
      cli::cli_abort("{.fun graph_add_wind} only supports {.code variable = {.str {wind}}}.")
    }
  }

  # Check that all the files of wind_speed exist and match the data request
  graph$ws <- graph_add_wind_edge(graph, constant_speed = constant_speed, ...)
  gc()

  if (is.finite(thr_as)) {
    # Filter edges based on airspeed, in blocks to avoid full-length complex temporaries.
    # Keep stay-in-place edges, which graph_transition() treats as zero airspeed
    keep <- logical(length(graph$s))
    for (from in seq.int(1, length(keep), by = 1e6)) {
      b <- seq.int(from, min(from + 1e6 - 1, length(keep)))
      keep[b] <- abs(graph$gs[b] - graph$ws[b]) <= thr_as | graph$gs[b] == 0
    }

    # Check that there are always at least one node left by stap
    n_grid <- prod(graph$sz[1:2])
    n_layer <- graph$sz[3] - 1L
    stap_id <- graph$stap$stap_id[graph$stap$include]
    sta_pass <- which(tabulate((graph$s[keep] - 1L) %/% n_grid + 1L, n_layer) == 0)
    if (length(sta_pass) > 0) {
      as <- abs(graph$gs - graph$ws)[((graph$s - 1L) %/% n_grid + 1L) %in% sta_pass]
      cli::cli_abort(c(
        x = "Using {.arg thr_as} of {thr_as} km/h, there are no nodes left for the stationary
        period: {.field {stap_id[sta_pass]}} with a minimum airspeed of {.val {round(min(as))}} km/h."
      ))
    }

    # Prune edges disconnected from the previous layer (forward) and next layer (backward). Edges
    # are ordered by source layer, so each layer is a contiguous range.
    n_edge <- tabulate((graph$s - 1L) %/% n_grid + 1L, n_layer)
    r <- Map(seq.int, cumsum(n_edge) - n_edge + 1, cumsum(n_edge))
    for (l in seq_along(r)[-1]) {
      keep[r[[l]]] <- keep[r[[l]]] & graph$s[r[[l]]] %in% graph$t[r[[l - 1]]][keep[r[[l - 1]]]]
      if (!any(keep[r[[l]]])) {
        cli::cli_abort(c(
          x = "No complete path remains: the graph is disconnected at stationary period
            {.val {stap_id[l]}} when moving forward from the equipment location.",
          ">" = "Consider increasing {.arg thr_as}."
        ))
      }
    }
    for (l in rev(seq_len(length(r) - 1))) {
      keep[r[[l]]] <- keep[r[[l]]] & graph$t[r[[l]]] %in% graph$s[r[[l + 1]]][keep[r[[l + 1]]]]
      if (!any(keep[r[[l]]])) {
        cli::cli_abort(c(
          x = "No complete path remains: the graph is disconnected at stationary period
            {.val {stap_id[l + 1]}} when moving backward from the retrieval location.",
          ">" = "Consider increasing {.arg thr_as}."
        ))
      }
    }
    graph$s <- graph$s[keep]
    graph$t <- graph$t[keep]
    graph$gs <- graph$gs[keep]
    graph$ws <- graph$ws[keep]

    # After pruning some equipment and retrieval nodes might not be present anymore.
    graph$equipment <- graph$equipment[graph$equipment %in% graph$s]
    graph$retrieval <- graph$retrieval[graph$retrieval %in% graph$t]
  }

  # Update param
  dots <- list(...)
  graph$param$graph_add_wind$thr_as <- thr_as
  graph$param$graph_add_wind$constant_speed <- constant_speed
  graph$param$graph_add_wind$rounding_interval <- if ("rounding_interval" %in% names(dots)) {
    dots$rounding_interval
  } else {
    formals(graph_add_wind_edge)$rounding_interval
  }
  graph$param$graph_add_wind$pressure_source <- "provided"

  # Store the file function (provided or default) without source reference or environment
  file <- if ("file" %in% names(dots)) dots$file else wind_file_default
  attr(file, "srcref") <- NULL
  attr(file, "srcfile") <- NULL
  environment(file) <- baseenv()
  graph$param$graph_add_wind$file <- file

  return(graph)
}
# nocov end

#' Add wind to graph edges
#'
#' @noRd
graph_add_wind_edge <- function(
  graph,
  pressure = NULL,
  rounding_interval = 60,
  file = wind_file_default,
  quiet = FALSE,
  constant_speed = "groundspeed"
) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ncdf4} is required for {.fun graph_add_wind}.",
      "i" = "Install it with {.run install.packages('ncdf4')}."
    ))
  }

  variable <- c("u_component_of_wind", "v_component_of_wind")
  ms_to_kmh <- 3.6
  tag_id <- graph$param$id

  # Reuse the public extractor checks so graph-scale wind reads the same validated ERA5 files.
  if (!quiet) {
    cli::cli_alert_info("Check wind files and coverage")
  }
  edge_add_wind_check(
    graph,
    pressure = pressure,
    variable = variable,
    file = file
  )
  if (!quiet) {
    cli::cli_alert_success("Wind files and coverage checked")
  }

  # Prepare edge groups without expanding all source and target coordinates at once.
  if (!quiet) {
    cli::cli_alert_info("Prepare graph edges for wind interpolation")
  }
  g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)
  flight <- stap2flight(graph$stap, format = "list")
  stap_include <- if ("include" %in% names(graph$stap)) {
    graph$stap$stap_id[graph$stap$include]
  } else {
    graph$stap$stap_id
  }
  n_grid <- prod(g$dim)
  edge_s <- graph$s
  edge_t <- graph$t
  # Edges are ordered by source layer (graph_create), so each layer is a contiguous range. Process
  # it in blocks of at most `block_size` edges so memory does not scale with the largest layer.
  block_size <- 1e6
  n_edge <- tabulate((edge_s - 1L) %/% n_grid + 1L)
  layer_end <- cumsum(n_edge)
  block <- do.call(
    rbind,
    lapply(which(n_edge > 0), function(l) {
      from <- seq.int(layer_end[l] - n_edge[l] + 1, layer_end[l], by = block_size)
      cbind(layer = l, from = from, to = pmin(from + block_size - 1, layer_end[l]))
    })
  )

  ws <- complex(length(edge_s))

  # Drop graph metadata while retaining compact edge indices for one-period coordinate expansion.
  graph <- list(param = list(id = graph$param$id))

  if (!quiet) {
    stap_layer <- 0
    cli::cli_progress_bar(
      "Compute wind speed for edges of stationary period:",
      format = "{cli::col_blue(cli::symbol$info)} {cli::pb_name} {stap_layer}/{length(n_edge)} {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str} [{cli::pb_elapsed}]",
      format_done = "{cli::col_green(cli::symbol$tick)} Compute wind speed for edges of stationary periods {cli::col_white('[', cli::pb_elapsed, ']')}",
      clear = FALSE,
      total = length(edge_s)
    )
  }

  n_edge_done <- 0L
  for (i_block in seq_len(nrow(block))) {
    # Work on one block of a source stationary period at a time and write the output into ws[st_id].
    # Graph layers are compressed to included staps; flight names use original stap_id.
    stap_layer <- block[i_block, "layer"]
    stap_id <- stap_include[stap_layer]
    fl_s <- flight[[as.character(stap_id)]]
    st_id <- seq.int(block[i_block, "from"], block[i_block, "to"])

    edge_s0 <- edge_s[st_id] - 1L
    edge_t0 <- edge_t[st_id] - 1L
    lat_edge_s <- g$lat[edge_s0 %% g$dim[1] + 1L]
    lon_edge_s <- g$lon[(edge_s0 %/% g$dim[1]) %% g$dim[2] + 1L]
    lat_edge_t <- g$lat[edge_t0 %% g$dim[1] + 1L]
    lon_edge_t <- g$lon[(edge_t0 %/% g$dim[1]) %% g$dim[2] + 1L]

    # If a graph edge spans several labelled flights, split the straight edge by flight duration
    # and combine flight means with duration weights.
    ratio_stap <- c(0, cumsum(fl_s$duration) / sum(fl_s$duration))
    duration_weight <- fl_s$duration / sum(fl_s$duration)

    # Read the wind fields of each flight once per block, on the box between the source and target
    # ranges where straight edges lie, padded by 2.5° for drifted tracks (clamped to it).
    pad <- if (constant_speed == "airspeed") c(-2.5, 2.5) else 0
    lat_rng <- cbind(range(lat_edge_s), range(lat_edge_t)) + pad
    lon_rng <- cbind(range(lon_edge_s), range(lon_edge_t)) + pad
    u_stap <- numeric(length(st_id))
    v_stap <- numeric(length(st_id))
    for (i_fl in seq_len(nrow(fl_s))) {
      nc <- ncdf4::nc_open(file(fl_s$stap_s[i_fl], tag_id))
      f <- edge_add_wind_flight(
        nc,
        pressure,
        fl_s,
        i_fl,
        rounding_interval,
        lat_rng,
        lon_rng,
        ratio_stap[i_fl + 0:1]
      )
      ncdf4::nc_close(nc)

      # Split the edge by flight duration and average the wind along the flown track
      lat_s <- lat_edge_s + ratio_stap[i_fl] * (lat_edge_t - lat_edge_s)
      lon_s <- lon_edge_s + ratio_stap[i_fl] * (lon_edge_t - lon_edge_s)
      out <- edge_add_wind_track(
        f,
        lat_s,
        lon_s,
        diff(ratio_stap[i_fl + 0:1]) * (lat_edge_t - lat_edge_s) / fl_s$duration[i_fl],
        diff(ratio_stap[i_fl + 0:1]) * (lon_edge_t - lon_edge_s) / fl_s$duration[i_fl],
        airspeed = constant_speed == "airspeed"
      )

      # Combine multiple flights inside this stap transition into one mean wind per edge.
      u_stap <- u_stap + out$u * duration_weight[i_fl]
      v_stap <- v_stap + out$v * duration_weight[i_fl]
    }

    # Store wind as a complex vector in km/h: Re = eastward u, Im = northward v.
    ws[st_id] <- (u_stap + 1i * v_stap) * ms_to_kmh
    if (!quiet) {
      n_edge_done <- n_edge_done + length(st_id)
      cli::cli_progress_update(
        set = n_edge_done,
        force = TRUE
      )
    }
  }

  ws
}
