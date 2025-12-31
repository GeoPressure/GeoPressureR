#' @family geolight
#' @rdname geolight_map
#' @export
geolight_map_likelihood <- function(
  tag,
  compute_known = FALSE,
  quiet = FALSE
) {
  tag_assert(tag, "twl_calib")
  tag_assert(tag, "setmap")
  assertthat::assert_that(is.logical(compute_known), length(compute_known) == 1)
  assertthat::assert_that(is.logical(quiet), length(quiet) == 1)

  if (!quiet) {
    cli::cli_progress_step("Compute twilight likelihood maps")
  }

  # Get map parameters
  g <- map_expand(tag$param$tag_set_map$extent, tag$param$tag_set_map$scale)

  # Get twilight to include
  twl <- twilight_include(tag$twilight)

  # Filter out known stationary periods to reduce computation time
  if (!compute_known) {
    known_stap_id <- tag$stap$stap_id[
      !is.na(tag$stap$known_lat) & !is.na(tag$stap$known_lon)
    ]
    twl$include <- twl$include & !(twl$stap_id %in% known_stap_id)
  }

  # Compute solar zenith angle for included twilight
  z <- geolight_solar(twl$twilight[twl$include], lat = g$lat, lon = g$lon)

  # Compute light likelihood for each included twilight
  twl_calib <- tag$param$geolight_map$twl_calib
  lk <- array(
    stats::approx(
      twl_calib$x,
      twl_calib$y,
      z,
      yleft = 0,
      yright = 0
    )$y,
    dim = dim(z)
  )

  pgz <- vector("list", nrow(twl))
  pgz[twl$include] <- lapply(
    seq_len(sum(twl$include)),
    function(k) lk[,, k]
  )

  # Transform twl to stap format for map_create
  twl$stap_id_grp <- twl$stap_id
  twl$stap_id <- seq_len(nrow(twl))
  twl$start <- twl$twilight
  twl$end <- twl$twilight

  tag$map_light_twl <- map_create(
    data = pgz,
    extent = tag$param$tag_set_map$extent,
    scale = tag$param$tag_set_map$scale,
    stap = twl,
    id = tag$param$id,
    type = "twilight"
  )

  tag
}
