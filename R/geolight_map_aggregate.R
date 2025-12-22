#' @family geolight
#' @rdname geolight_map
#' @export
geolight_map_aggregate <- function(
  tag,
  compute_known = formals(geolight_map)$compute_known,
  twl_llp = eval(formals(geolight_map)$twl_llp),
  keep_twl = formals(geolight_map)$keep_twl,
  quiet = FALSE
) {
  tag_assert(tag, "map_light_twl")
  assertthat::assert_that(is.logical(compute_known), length(compute_known) == 1)
  assertthat::assert_that(is.function(twl_llp))

  # Compute grid information
  g <- map_expand(tag$param$tag_set_map$extent, tag$param$tag_set_map$scale)

  twl <- tag$map_light_twl$stap
  twl$stap_id <- NULL # stap_id was used for unique identifier of twilight for plot
  # Re-assign stap_id based on stap
  twl$stap_id <- find_stap(tag$stap, twl$twilight)

  # Group twilight by stap
  twl_idx <- which(twl$include)
  twl_id_stap_id <- split(twl_idx, twl$stap_id[twl_idx])

  # Compute the number of twilight per stap
  stopifnot(lengths(twl_id_stap_id) > 0)

  pgz <- tag$map_light_twl$data

  # Initialize the likelihood list from stap to make sure all stap are present
  lk <- replicate(
    nrow(tag$stap),
    matrix(1, nrow = g$dim[1], ncol = g$dim[2]),
    simplify = FALSE
  )

  n_twl_per_stap <- lengths(twl_id_stap_id)

  if (!quiet) {
    cli::cli_progress_bar(
      name = "Combine maps per stationary periods",
      total = sum(n_twl_per_stap)
    )
  }
  for (i in seq_len(length(twl_id_stap_id))) {
    twl_id <- twl_id_stap_id[[i]]
    maps <- pgz[twl_id]

    # Combine with a log-linear pooling (computed in log-space for stability)
    if (length(maps) == 1) {
      l <- maps[[1]]
    } else {
      w <- twl_llp(length(maps))
      log_l <- Reduce(
        `+`,
        lapply(maps, \(m) w * log(m + .Machine$double.eps))
      )
      l <- exp(log_l)
    }

    stap_id <- as.numeric(names(twl_id_stap_id)[i])
    lk[[stap_id]] <- l
    if (!quiet) {
      cli::cli_progress_update(inc = n_twl_per_stap[[i]])
    }
  }
  if (!quiet) {
    cli::cli_progress_done()
  }

  # Add known location
  if (!compute_known) {
    known_stap_id <- tag$stap$stap_id[
      !is.na(tag$stap$known_lat) & !is.na(tag$stap$known_lon)
    ]
    for (stap_id in known_stap_id) {
      stap_row <- match(stap_id, tag$stap$stap_id)
      # Initiate an empty map
      lk[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
      # Compute the index of the known position
      known_lon_id <- which.min(abs(tag$stap$known_lon[stap_row] - g$lon))
      known_lat_id <- which.min(abs(tag$stap$known_lat[stap_row] - g$lat))
      # Assign a likelihood of 1 for that position
      lk[[stap_id]][known_lat_id, known_lon_id] <- 1
    }
  }

  # Create map object
  tag$map_light <- map_create(
    data = lk,
    extent = tag$param$tag_set_map$extent,
    scale = tag$param$tag_set_map$scale,
    stap = tag$stap,
    id = tag$param$id,
    type = "light"
  )

  attr(twl_llp, "srcref") <- NULL
  attr(twl_llp, "srcfile") <- NULL
  environment(twl_llp) <- baseenv()

  # Add parameters
  tag$param$geolight_map$twl_llp <- twl_llp
  tag$param$geolight_map$compute_known <- compute_known

  # remove mse maps computed by geopressure_map_mismatch()
  if (!keep_twl) {
    tag[names(tag) == "map_light_twl"] <- NULL
  }

  tag
}
