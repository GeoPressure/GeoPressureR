#' Create a `path` from indices of coordinates
#'
#' @description
#' Convert a vector of 2D coordinate indices into a `path` data.frame. If `ind` is a matrix,
#' each row represents the path of a different trajectory `j`.
#'
#' This function is the main way to create `path` in GeoPressureR, and is used by
#' [`graph_most_likely`], [`graph_simulation`] and [`tag2path`].
#'
#' @param ind vector of indices of the 2D map (vector or matrix)
#' @param tag_graph either a `tag` or a `graph` object, must contains `stap`, `extent` and `scale`.
#' @param use_known If true, enforce the known position in the path created. The known positions are
#' approximated to the map resolution in order to corresponds to integer index.
#' @return A path data.frame
#' - `stap_id` stationary period identification
#' - `j` identification for each trajectory (`1` to `nj`).
#' - `ind` indices of the coordinate in the 2D grid.
#' - `lat` Latitude,
#' - `lon` longitude
#' - `...` other columns from `stap`
#' @family path
#' @noRd
ind2path <- function(ind, tag_graph, use_known = TRUE) {
  assertthat::assert_that(
    inherits(tag_graph, "tag") | inherits(tag_graph, "graph")
  )
  assertthat::assert_that(is.logical(use_known))

  stap <- tag_graph$stap

  # Compute the grid information
  g <- map_expand(
    tag_graph$param$tag_set_map$extent,
    tag_graph$param$tag_set_map$scale
  )

  # Check path
  assertthat::assert_that(is.numeric(ind))
  assertthat::assert_that(all(prod(g$dim) >= ind[!is.na(ind)]))
  if (is.vector(ind)) {
    ind <- matrix(ind, nrow = 1)
  }
  assertthat::assert_that(dim(ind)[2] == nrow(stap))

  # Convert the index in 2D grid into 1D lat and lon coordinate
  ind_lon <- ceiling(ind / g$dim[1]) # (ind - ind_lat) / g$dim[1] + 1
  ind_lat <- (ind - 1) %% g$dim[1] + 1 # (ind %% g$dim[1])

  # Create the data.frame with all information
  path0 <- data.frame(
    stap_id = rep(stap$stap_id, each = dim(ind)[1]),
    j = rep(seq_len(dim(ind)[1]), times = dim(ind)[2]),
    ind = as.integer(as.vector(ind)),
    lat = g$lat[ind_lat],
    lon = g$lon[ind_lon]
  )

  stap$known <- !is.na(stap$known_lat)

  # Combine with stap
  path <- merge(path0, stap, by = "stap_id", all.x = TRUE)

  # Enforce known position in path
  if (use_known && any(path$known)) {
    path$lon[path$known] <- path$known_lon[path$known]
    path$lat[path$known] <- path$known_lat[path$known]

    # lon_ind_known <- sapply(path$known_lon[path$known], \(x) which.min(abs(g$lon - x)))
    # lat_ind_known <- sapply(path$known_lat[path$known], \(x) which.min(abs(g$lat - x)))
    # path$ind[path$known] <- (lon_ind_known - 1) * g$dim[1] + lat_ind_known
  }

  # Remove known_lat and known_lon
  path <- path[, -which(names(path) %in% c("known_lat", "known_lon"))]

  return(path)
}
