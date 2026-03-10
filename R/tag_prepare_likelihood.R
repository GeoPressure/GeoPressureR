#' Prepare likelihood maps for modeling
#'
#' @param tag a GeoPressureR `tag` object.
#' @param likelihood Field of the `tag` list containing the likelihood map (character).
#' @param thr_likelihood threshold of percentile to keep likely locations in each stationary
#' period.
#' @param quiet logical to hide progress messages.
#'
#' @return A likelihood `map` object with:
#' - `lk_norm`: list of normalized likelihood maps for included staps.
#' - `lk_mask`: list of logical masks for included staps.
#' - `lk_sparse`: list of sparse likelihood entries (`idx`, `prob`) for included staps.
#' - `likelihood`: resolved likelihood field(s).
#'
#' @noRd
tag_prepare_likelihood <- function(
  tag,
  likelihood = NULL,
  thr_likelihood = 0.99,
  thr_gs = 150,
  quiet = FALSE
) {
  # Resolve map field names up front so we can keep the exact selection for messages and metadata.
  likelihood <- tag2likelihood(tag, likelihood)

  # Construct the likelihood map (tag2map would also resolve, but we keep the resolved names here).
  map <- tag2map(tag, likelihood = likelihood)
  g <- map_expand(map$extent, map$scale)

  if (!("include" %in% names(map$stap))) {
    map$stap$include <- TRUE
  }
  if (!is.logical(map$stap$include)) {
    cli::cli_abort("{.field stap$include} must be logical.")
  }
  if (anyNA(map$stap$include)) {
    cli::cli_abort("{.field stap$include} contains NA values.")
  }

  # Select only the map for the stap to model
  stap_include <- which(map$stap$include)
  if (length(stap_include) == 0) {
    cli::cli_abort(
      "No stationary periods selected for modeling (all {.field stap$include} are FALSE)."
    )
  }
  stap_id_model <- map$stap$stap_id[stap_include]
  lk <- map[stap_include]
  n_stap <- length(stap_include)
  sz <- c(g$dim[1], g$dim[2], n_stap)

  if (!quiet) {
    lk_mask_nb <- NULL
    cli::cli_progress_done()
    cli::cli_progress_step(
      "Create nodes from likelihood maps {.field {likelihood}} ({.val {g$dim[1]}}x{.val {g$dim[2]}}x{.val {n_stap}})",
      msg_done = "Nodes created from {round(lk_mask_nb/prod(sz)*100)}% of the likelihood maps {.field {likelihood}} ({.val {g$dim[1]}}x{.val {g$dim[1]}}x{.val {n_stap}})",
    )
  }

  # Check that the likelihood is not null for the selected stationary periods
  lk_null <- vapply(lk, is.null, logical(1))
  if (any(lk_null)) {
    cli::cli_abort(c(
      x = "The {.field {likelihood}} in {.var tag} is/are null for stationary periods {.var {stap_include[lk_null]}} while those stationary period are required in {.var stap$include}",
      i = "Check your input and re-run {.fun geopressure_map} if necessary."
    ))
  }

  # Process likelihood map
  # We use here the normalized likelihood assuming that the bird needs to be somewhere at each
  # stationary period. The log-linear pooling (`geopressure_map_likelihood`) is supposed to account
  # for the variation in stationary period duration.

  mask_water <- if ("mask_water" %in% names(map)) map$mask_water else FALSE

  lk_norm <- lapply(lk, function(l) {
    # Remove over water
    l[mask_water] <- NA_real_

    # replace empty map with 1 everywhere
    if (sum(l, na.rm = TRUE) == 0) {
      l[l == 0] <- 1
    }

    # replace NA by 0
    l[is.na(l)] <- 0

    # Normalize
    l / sum(l, na.rm = TRUE)
  })

  # Check for invalid map
  stap_id_0 <- vapply(lk_norm, sum, numeric(1)) == 0
  if (anyNA(stap_id_0)) {
    cli::cli_abort(c(
      x = "{.var likelihood} is invalid for the stationary period: {stap_include[which(is.na(stap_id_0))]}"
    ))
  }
  if (any(stap_id_0)) {
    cli::cli_abort(c(
      x = "Using the {.var likelihood} provided has an invalid probability map for the stationary period: {stap_include[which(stap_id_0)]}"
    ))
  }

  # find the pixels above to the percentile
  assertthat::assert_that(is.numeric(thr_likelihood))
  assertthat::assert_that(length(thr_likelihood) == 1)
  assertthat::assert_that(thr_likelihood >= 0 & thr_likelihood <= 1)
  lk_mask <- lapply(lk_norm, function(l) {
    # First, compute the threshold of prob corresponding to percentile
    ls <- sort(l)
    id_prob_percentile <- sum(cumsum(ls) < (1 - thr_likelihood))
    thr_prob <- ls[id_prob_percentile + 1]

    # return matrix if the values are above the threshold
    l >= thr_prob
  })

  # Check that there are still values
  lk_mask_0 <- vapply(lk_mask, sum, numeric(1)) == 0
  if (any(lk_mask_0)) {
    cli::cli_abort(c(
      x = "Using the {.var thr_likelihood} of {.val {thr_likelihood}}, there are not any nodes left at stationary period: {.val {stap_include[which(lk_mask_0)]}}"
    ))
  }

  if (!quiet) {
    lk_mask_nb <- sum(vapply(lk_mask, \(l) sum(l), numeric(1)))
    dist_mask_nb <- NULL
    cli::cli_progress_done()
    cli::cli_progress_step(
      "Filter nodes by binary distance",
      msg_done = "Nodes filtered by binary distance, {round(dist_mask_nb/lk_mask_nb*100)}% left"
    )
  }

  # filter the pixels which are not in reach of any location of the previous and next stationary
  # period
  # Create resolution matrix for the grid (length(g$lat) x length(g$lon))
  lat_res <- abs(stats::median(diff(g$lat))) * 111.320
  lon_res <- stats::median(diff(g$lon)) * 110.574
  resolution <- outer(g$lat, g$lon, function(lat, lon) {
    pmin(lat_res, lon_res * cos(lat * pi / 180))
  })

  # Construct flight
  flight_duration <- stap2flight(map$stap, units = "hours", return_numeric = TRUE)$duration
  # If the flight duration is 0, we assume a full day
  flight_duration[flight_duration == 0] <- 24
  assertthat::assert_that(all(flight_duration > 0))

  dist_mask <- lk_mask

  # The "-1" of distmap accounts for the fact that the shortest distance between two grid cell is
  # not the center of the cell but the side. This should only impact short flight distance/duration.
  for (i_s in seq_len(n_stap - 1)) {
    # Compute distance map and apply resolution matrix
    dist_map <- EBImage::distmap(!dist_mask[[i_s]]) - 1
    dist_km <- dist_map * resolution # Element-wise multiplication with resolution matrix
    dist_mask[[i_s + 1]] <- dist_km < flight_duration[i_s] * thr_gs & dist_mask[[i_s + 1]]
    if (sum(dist_mask[[i_s + 1]]) == 0) {
      cli::cli_abort(c(
        x = "Using the {.var thr_gs} of {.val {thr_gs}} km/h provided with the binary distance edges, there are not any nodes left at stationary period {.val {stap_id_model[i_s + 1]}} from stationary period {.val {stap_id_model[i_s]}}"
      ))
    }
  }
  for (i_sr in seq_len(n_stap - 1)) {
    i_s <- n_stap - i_sr + 1
    # Compute distance map and apply resolution matrix
    dist_map <- EBImage::distmap(!dist_mask[[i_s]]) - 1
    dist_km <- dist_map * resolution # Element-wise multiplication with resolution matrix
    dist_mask[[i_s - 1]] <- dist_km < flight_duration[i_s - 1] * thr_gs &
      dist_mask[[i_s - 1]]
    if (sum(dist_mask[[i_s - 1]]) == 0) {
      cli::cli_abort(c(
        x = "Using the {.var thr_gs} of {thr_gs} km/h provided with the binary distance edges, there are not any nodes left at stationary period {.val {stap_id_model[i_s - 1]}} from stationary period {.val {stap_id_model[i_s]}}"
      ))
    }
  }

  # Check that there are still pixel present
  dist_mask_sum <- vapply(dist_mask, sum, numeric(1))
  if (any(dist_mask_sum == 0)) {
    cli::cli_abort(c(
      x = "Using the {.val thr_gs} of {thr_gs} km/h provided with the binary distance edges, there are not any nodes left."
    ))
  }
  if (!quiet) {
    dist_mask_nb <- sum(vapply(dist_mask, \(l) sum(l), numeric(1)))
    cli::cli_progress_done()
  }

  # Add the nomarlize likelikood map to the map object
  map$lk_norm <- lk_norm
  map$lk_mask <- lk_mask
  # Also return the resolved likelihood value
  map$likelihood <- likelihood

  map$dist_mask <- dist_mask

  map$flight_duration <- flight_duration
  map$resolution <- resolution
  map$sz <- sz
  map
}
