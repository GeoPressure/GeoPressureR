#' Compute windspeed and airspeed on a `graph`
#'
#' @description
#' Reads the NetCDF files downloaded and interpolate the average windspeed experienced by the
#' bird on each possible edge, as well as the corresponding airspeed.
#'
#' In addition, the graph can be further pruned based on a threshold of airspeed `thr_as`.
#'
#' See section [2.2.4 in Nussbaumer (2023b)](
#' https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0008-title)
#' for more technical details and the [GeoPressureManual](
#' https://geopressure.org/GeoPressureManual/trajectory-with-wind.html) for an
#' illustration on how to use it.
#'
#' @param graph a GeoPressureR graph object.
#' @param thr_as threshold of airspeed (km/h).
#' @param variable `r lifecycle::badge("deprecated")` No longer used.
#'   `graph_add_wind()` always uses `c("u", "v")`.
#' @inheritParams edge_add_wind
#' @inheritDotParams edge_add_wind -graph -edge_s -edge_t -variable -return_averaged_variable
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
  ...
) {
  graph_assert(graph, "full")
  assertthat::assert_that(is.numeric(thr_as))
  assertthat::assert_that(length(thr_as) == 1)
  assertthat::assert_that(thr_as >= 0)
  if (lifecycle::is_present(variable)) {
    lifecycle::deprecate_soft(
      "3.5.4",
      "graph_add_wind(variable)",
      details = "{.fun graph_add_wind} now always uses {.code c('u', 'v')}."
    )
    if (!identical(variable, c("u", "v"))) {
      cli::cli_abort("{.fun graph_add_wind} only supports {.code variable = c('u', 'v')}.")
    }
  }

  # Check that all the files of wind_speed exist and match the data request
  graph$ws <- add_wind_graph_edge(
    graph,
    ...
  )
  gc()

  if (is.finite(thr_as)) {
    # filter edges based on airspeed
    id <- abs(graph$gs - graph$ws) <= thr_as

    # Check that there are always at least one node left by stap
    g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)
    edge_s <- arrayInd(graph$s[id], c(g$dim, nrow(graph$stap)))
    sta_pass <- which(!(seq_len(graph$sz[3] - 1) %in% unique(edge_s[, 3])))
    if (length(sta_pass) > 0) {
      as <- abs(graph$gs - graph$ws)
      edge_s_all <- arrayInd(graph$s, c(g$dim, nrow(graph$stap)))
      cli::cli_abort(c(
        x = "Using {.arg thr_as} of {thr_as} km/h,  there are not any nodes left for the stationary
        period: {.field {sta_pass}} with a minimum airspeed of
        {.val {round(min(as[edge_s_all[, 3] %in% sta_pass]))}} km/h."
      ))
    }
    rm(g, edge_s)
    gc()

    # Filter node
    graph$s <- graph$s[id]
    graph$t <- graph$t[id]
    graph$gs <- graph$gs[id]
    graph$ws <- graph$ws[id]

    # Prune the graph
    # First, reconstruction the stap list graph for graph_create_prune to work
    gr <- split(
      data.frame(s = graph$s, t = graph$t, gs = graph$gs, ws = graph$ws),
      arrayInd(graph$s, graph$sz)[, 3]
    )
    gr <- graph_create_prune(gr)
    # Convert it back to a full list
    tmp <- as.list(do.call("rbind", gr))
    # Overwrite all edges vectors
    graph$s <- tmp$s
    graph$t <- tmp$t
    graph$gs <- tmp$gs
    graph$ws <- tmp$ws

    # After pruning some retrieval nodes might not be present anymore.
    graph$retrieval <- graph$retrieval[graph$retrieval %in% graph$t]
  }

  # Update param
  dots <- list(...)
  graph$param$graph_add_wind$thr_as <- thr_as

  # Handle file parameter if provided
  if ("file" %in% names(dots)) {
    file <- dots$file
    attr(file, "srcref") <- NULL
    attr(file, "srcfile") <- NULL
    environment(file) <- baseenv()
    graph$param$graph_add_wind$file <- file
  } else {
    # Use default file function if not provided
    file <- \(stap_id) {
      glue::glue("./data/wind/{graph$param$id}/{graph$param$id}_{stap_id}.nc")
    }
    attr(file, "srcref") <- NULL
    attr(file, "srcfile") <- NULL
    environment(file) <- baseenv()
    graph$param$graph_add_wind$file <- file
  }

  return(graph)
}
# nocov end

#' Add wind to graph edges
#'
#' @noRd
add_wind_graph_edge <- function(
  graph,
  pressure = NULL,
  rounding_interval = 60,
  interp_spatial_linear = FALSE,
  file = \(stap_id, tag_id) {
    glue::glue(
      "./data/wind/{tag_id}/{tag_id}_{stap_id}.nc"
    )
  },
  quiet = FALSE
) {
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(c(
      "x" = "Package {.pkg ncdf4} is required for {.fun graph_add_wind}.",
      "i" = "Install it with {.run install.packages('ncdf4')}."
    ))
  }

  variable <- c("u", "v")
  ms_to_kmh <- 3.6
  tag_id <- graph$param$id
  if (is.null(pressure) && inherits(graph, "tag")) {
    pressure <- graph$pressure
  }

  # Reuse the public extractor checks so graph-scale wind reads the same validated ERA5 files.
  edge_add_wind_check(
    graph,
    pressure = pressure,
    variable = variable,
    file = file
  )

  # Prepare the same edge groups and flight segments as edge_add_wind(), but keep only
  # vectors needed for direct weighted accumulation.
  if (!quiet) {
    cli::cli_alert_info("Prepare graph edges for wind interpolation")
  }
  g <- map_expand(graph$param$tag_set_map$extent, graph$param$tag_set_map$scale)
  flight <- stap2flight(graph$stap, format = "list")
  n_grid <- prod(g$dim)
  edge_s0 <- graph$s - 1
  edge_t0 <- graph$t - 1
  edge_s <- cbind(
    as.integer(edge_s0 %% g$dim[1] + 1),
    as.integer((edge_s0 %/% g$dim[1]) %% g$dim[2] + 1),
    as.integer(edge_s0 %/% n_grid + 1)
  )
  edge_t <- cbind(
    as.integer(edge_t0 %% g$dim[1] + 1),
    as.integer((edge_t0 %/% g$dim[1]) %% g$dim[2] + 1),
    as.integer(edge_t0 %/% n_grid + 1)
  )
  table_edge_s <- table(edge_s[, 3])
  list_st_id <- split(seq_len(nrow(edge_s)), edge_s[, 3])
  edge_stap <- names(list_st_id)

  ws <- complex(nrow(edge_s))

  # Drop the large graph object before opening wind files; only tag_id is needed from here.
  graph <- list(param = list(id = graph$param$id))
  gc()

  if (!quiet) {
    i_stap <- 0
    cli::cli_progress_bar(
      "Compute wind speed for edges of stationary period:",
      format = "{cli::col_blue(cli::symbol$info)} {cli::pb_name} {i_stap}/{length(edge_stap)} {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str} [{cli::pb_elapsed}]",
      format_done = "{cli::col_green(cli::symbol$tick)} Compute wind speed for edges of stationary periods {cli::col_white('[', cli::pb_elapsed, ']')}",
      clear = FALSE,
      total = sum(table_edge_s)
    )
  }

  n_edge_done <- 0L
  for (i_stap in seq_along(edge_stap)) {
    # Work on one source stationary period at a time so the output can be written into ws[st_id].
    stap_id <- edge_stap[i_stap]
    fl_s <- flight[[stap_id]]
    st_id <- list_st_id[[stap_id]]
    if (length(st_id) == 0) {
      next
    }
    lat_edge_s <- g$lat[edge_s[st_id, 1]]
    lon_edge_s <- g$lon[edge_s[st_id, 2]]
    lat_edge_t <- g$lat[edge_t[st_id, 1]]
    lon_edge_t <- g$lon[edge_t[st_id, 2]]

    # If a graph edge spans several labelled flights, split the straight edge by flight duration
    # and combine flight means with duration weights.
    ratio_stap <- as.matrix(c(0, cumsum(fl_s$duration) / sum(fl_s$duration)))
    duration_weight <- fl_s$duration / sum(fl_s$duration)
    u_stap <- numeric(length(st_id))
    v_stap <- numeric(length(st_id))

    for (i_fl in seq_len(nrow(fl_s))) {
      i_s <- fl_s$stap_s[i_fl]
      file_path <- file(i_s, tag_id)
      nc <- ncdf4::nc_open(file_path)
      nc_info <- edge_add_wind_nc_info(nc, file_path)
      time <- nc_info$time
      pres <- nc_info$pres
      lat <- nc_info$lat
      dlat <- nc_info$dlat
      lon <- nc_info$lon
      dlon <- nc_info$dlon

      # Split the edge by flight duration, then interpolate one query-time position vector at a
      # time to avoid allocating time-by-edge position matrices.
      lat_s <- lat_edge_s + ratio_stap[i_fl] * (lat_edge_t - lat_edge_s)
      lon_s <- lon_edge_s + ratio_stap[i_fl] * (lon_edge_t - lon_edge_s)
      lat_e <- lat_edge_s + ratio_stap[i_fl + 1] * (lat_edge_t - lat_edge_s)
      lon_e <- lon_edge_s + ratio_stap[i_fl + 1] * (lon_edge_t - lon_edge_s)
      t_q <- add_wind_graph_query_times(fl_s, i_fl, rounding_interval)
      w <- edge_add_wind_weights(fl_s, i_fl, t_q, rounding_interval)
      dlat_se <- (lat_e - lat_s) / fl_s$duration[i_fl]
      dlon_se <- (lon_e - lon_s) / fl_s$duration[i_fl]

      u_fl <- numeric(length(st_id))
      v_fl <- numeric(length(st_id))
      # Read and interpolate one time slice at a time, accumulating weighted u/v means directly.
      # This avoids allocating detailed data.frames or time-by-edge matrices for graph_add_wind().
      for (i_time in seq_len(length(t_q))) {
        time_offset <- pmax(
          pmin(
            as.numeric(difftime(t_q[i_time], fl_s$start[i_fl], units = "hours")),
            fl_s$duration[i_fl]
          ),
          0
        )
        lat_i <- lat_s + dlat_se * time_offset
        lon_i <- lon_s + dlon_se * time_offset
        lat_ind_i <- NULL
        lon_ind_i <- NULL
        if (!interp_spatial_linear) {
          lat_ind_i <- match(round(lat_i * 4) / 4, lat)
          lon_ind_i <- match(round(lon_i * 4) / 4, lon)
        }
        out <- add_wind_graph_values_time(
          pressure,
          t_q[i_time],
          pres,
          time,
          lat,
          lon,
          dlat,
          dlon,
          lat_i,
          lon_i,
          lat_ind_i,
          lon_ind_i,
          interp_spatial_linear,
          nc,
          i_s,
          i_fl,
          fl_s
        )
        u_fl <- u_fl + out$u * w[i_time]
        v_fl <- v_fl + out$v * w[i_time]
      }

      # Combine multiple flights inside this stap transition into one mean wind per edge.
      u_stap <- u_stap + u_fl * duration_weight[i_fl]
      v_stap <- v_stap + v_fl * duration_weight[i_fl]
      ncdf4::nc_close(nc)

      rm(
        nc,
        nc_info,
        time,
        pres,
        lat,
        dlat,
        lon,
        dlon,
        t_q,
        w,
        lat_s,
        lon_s,
        lat_e,
        lon_e,
        dlat_se,
        dlon_se,
        time_offset,
        lat_i,
        lon_i,
        lat_ind_i,
        lon_ind_i,
        file_path,
        u_fl,
        v_fl
      )
      gc()
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
    rm(
      fl_s,
      st_id,
      lat_edge_s,
      lon_edge_s,
      lat_edge_t,
      lon_edge_t,
      ratio_stap,
      duration_weight,
      u_stap,
      v_stap
    )
    gc()
  }

  ws
}


#' Graph wind query times
#'
#' @noRd
add_wind_graph_query_times <- function(fl_s, i_fl, rounding_interval) {
  t_s <- as.POSIXct(
    trunc(as.numeric(fl_s$start[i_fl]) / (60 * rounding_interval)) *
      (60 * rounding_interval),
    origin = "1970-01-01",
    tz = "UTC"
  )
  t_e <- as.POSIXct(
    ceiling(as.numeric(fl_s$end[i_fl]) / (60 * rounding_interval)) *
      (60 * rounding_interval),
    origin = "1970-01-01",
    tz = "UTC"
  )

  seq(from = t_s, to = t_e, by = 60 * rounding_interval)
}


#' Interpolate u/v wind for graph edges at one query time
#'
#' @noRd
add_wind_graph_values_time <- function(
  pressure,
  t_q_i,
  pres,
  time,
  lat,
  lon,
  dlat,
  dlon,
  lat_i,
  lon_i,
  lat_ind_i,
  lon_ind_i,
  interp_spatial_linear,
  nc,
  i_s,
  i_fl,
  fl_s
) {
  p_q <- stats::approx(
    pressure$date,
    pressure$value,
    t_q_i,
    rule = 2
  )$y
  if (p_q >= pres[1]) {
    id_pres <- 1
    n_pres <- 1
    if (p_q > pres[1] && pres[1] != 1000) {
      cli::cli_warn(c(
        "!" = "Pressure is above the highest level while the highest level is not 1000hPa.",
        "i" = "Stationary period: {i_s}",
        "i" = "Flight index: {i_fl} of {nrow(fl_s)}",
        "i" = "Date: {format(t_q_i, '%Y-%m-%d %H:%M:%S')}",
        "i" = "Pressure: {round(p_q, 2)} hPa",
        "i" = "Highest available level: {pres[1]} hPa",
        "i" = "Pressure difference: {round(p_q - pres[1], 2)} hPa"
      ))
    }
  } else if (p_q <= pres[length(pres)]) {
    id_pres <- length(pres)
    n_pres <- 1
    if (p_q < pres[length(pres)]) {
      cli::cli_warn(
        "Pressure is below the lowest level. This should never happen!"
      )
    }
  } else {
    id_pres <- max(which(pres >= p_q))
    n_pres <- 2
  }

  id_time <- findInterval(t_q_i, time)
  n_time <- ifelse(
    id_time == length(time) | time[id_time] == t_q_i,
    1,
    2
  )

  if (interp_spatial_linear) {
    id_lon <- which(
      lon >= (min(lon_i) - dlon) &
        (max(lon_i) + dlon) >= lon
    )
    id_lat <- which(
      lat >= (min(lat_i) - dlat) &
        (max(lat_i) + dlat) >= lat
    )
  } else {
    id_lon <- seq.int(min(lon_ind_i), max(lon_ind_i))
    id_lat <- seq.int(min(lat_ind_i), max(lat_ind_i))
  }

  nc_start <- c(id_lon[1], id_lat[1], id_pres, id_time)
  nc_count <- c(length(id_lon), length(id_lat), n_pres, n_time)
  u_nc <- ncdf4::ncvar_get(nc, "u", start = nc_start, count = nc_count, collapse_degen = FALSE)
  v_nc <- ncdf4::ncvar_get(nc, "v", start = nc_start, count = nc_count, collapse_degen = FALSE)

  if (n_time == 2) {
    w_time <- as.numeric(difftime(
      t_q_i,
      time[id_time],
      units = "hours"
    )) /
      as.numeric(difftime(
        time[id_time + 1],
        time[id_time],
        units = "hours"
      ))
    u_nc <- array(
      u_nc[,,, 1] + w_time * (u_nc[,,, 2] - u_nc[,,, 1]),
      dim = c(length(id_lon), length(id_lat), n_pres)
    )
    v_nc <- array(
      v_nc[,,, 1] + w_time * (v_nc[,,, 2] - v_nc[,,, 1]),
      dim = c(length(id_lon), length(id_lat), n_pres)
    )
  } else {
    u_nc <- array(u_nc[,,, 1], dim = c(length(id_lon), length(id_lat), n_pres))
    v_nc <- array(v_nc[,,, 1], dim = c(length(id_lon), length(id_lat), n_pres))
  }

  if (n_pres == 2) {
    w_pres <- (p_q - pres[id_pres]) /
      (pres[id_pres + 1] - pres[id_pres])
    u_nc <- array(
      u_nc[,, 1] + w_pres * (u_nc[,, 2] - u_nc[,, 1]),
      dim = c(length(id_lon), length(id_lat))
    )
    v_nc <- array(
      v_nc[,, 1] + w_pres * (v_nc[,, 2] - v_nc[,, 1]),
      dim = c(length(id_lon), length(id_lat))
    )
  } else {
    u_nc <- array(u_nc[,, 1], dim = c(length(id_lon), length(id_lat)))
    v_nc <- array(v_nc[,, 1], dim = c(length(id_lon), length(id_lat)))
  }

  if (interp_spatial_linear) {
    ll_i <- (round(lat_i, 1) + 90) *
      10 *
      10000 +
      (round(lon_i, 1) + 180) * 10 +
      1
    ll_uniq <- unique(ll_i)
    lat_uniq <- ((ll_uniq - 1) %/% 10000) / 10 - 90
    lon_uniq <- ((ll_uniq - 1) %% 10000) / 10 - 180
    id_uniq <- match(ll_i, ll_uniq)
    u <- pracma::interp2(
      rev(lat[id_lat]),
      lon[id_lon],
      u_nc[, rev(seq_len(ncol(u_nc)))],
      lat_uniq,
      lon_uniq,
      method = "linear"
    )
    v <- pracma::interp2(
      rev(lat[id_lat]),
      lon[id_lon],
      v_nc[, rev(seq_len(ncol(v_nc)))],
      lat_uniq,
      lon_uniq,
      method = "linear"
    )
    assertthat::assert_that(!anyNA(u))
    assertthat::assert_that(!anyNA(v))
    u <- u[id_uniq]
    v <- v[id_uniq]
  } else {
    lon_ind_off <- lon_ind_i - id_lon[1] + 1
    lat_ind_off <- lat_ind_i - id_lat[1] + 1
    ind <- (lat_ind_off - 1) * nrow(u_nc) + lon_ind_off
    u <- u_nc[ind]
    v <- v_nc[ind]
  }

  list(u = u, v = v)
}
