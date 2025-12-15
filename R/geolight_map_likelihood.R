#' @family geolight
#' @rdname geolight_map
#' @export
geolight_map_likelihood <- function(
  tag,
  compute_known = formals(geolight_map)$compute_known,
  twl_llp = eval(formals(geolight_map)$twl_llp),
  keep_twl = FALSE,
  quiet = FALSE
) {
  tag_assert(tag, "map_twilight")
  assertthat::assert_that(is.logical(compute_known), length(compute_known) == 1)
  assertthat::assert_that(is.function(twl_llp))

  # Compute grid information
  g <- map_expand(tag$param$tag_set_map$extent, tag$param$tag_set_map$scale)

  twl <- tag$map_twilight$stap
  twl$stap_id <- NULL # stap_id was used for unique identifier of twilight for plot
  # Re-assign stap_id based on stap
  twl$stap_id <- find_stap(tag$stap, twl$twilight)

  # Group twilight by stap
  twl_id_stap_id <- split(seq_along(twl$stap_id[twl$include]), twl$stap_id[twl$include])

  # Compute the number of twilight per stap
  stopifnot(lengths(twl_id_stap_id) > 0)

  pgz <- tag$map_twilight$data

  # Initialize the likelihood list from stap to make sure all stap are present
  lk <- replicate(
    nrow(tag$stap),
    matrix(1, nrow = g$dim[1], ncol = g$dim[2]),
    simplify = FALSE
  )

  if (!quiet) {
    cli::cli_progress_bar(
      name = "Combine maps per stationary periods",
      total = sum(lengths(twl_id_stap_id))
    )
  }
  for (i in seq_len(length(twl_id_stap_id))) {
    # find all twilight from this stap
    id <- twl_id_stap_id[[i]]

    # Combine with a Log-linear equation express in log
    if (length(id) > 1) {
      l <- exp(rowSums(
        twl_llp(length(id)) * log(pgz[, id] + .Machine$double.eps)
      ))
    } else if (length(id) == 1) {
      l <- pgz[, id]
    }
    lk[[as.numeric(names(twl_id_stap_id[i]))]] <- matrix(
      l,
      nrow = g$dim[1],
      ncol = g$dim[2]
    )
    if (!quiet) {
      cli::cli_progress_update(inc = ntwl[[i]])
    }
  }
  if (!quiet) {
    cli::cli_progress_done()
  }

  # Add known location
  if (!compute_known) {
    for (stap_id in tag$stap$stap_id[!is.na(tag$stap$known_lat)]) {
      # Initiate an empty map
      lk[[stap_id]] <- matrix(0, nrow = g$dim[1], ncol = g$dim[2])
      # Compute the index of the known position
      known_lon_id <- which.min(abs(tag$stap$known_lon[stap_id] - g$lon))
      known_lat_id <- which.min(abs(tag$stap$known_lat[stap_id] - g$lat))
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
    tag[names(tag) == "map_twilight"] <- NULL
  }

  tag
}
