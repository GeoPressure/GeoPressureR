#' @family geolight
#' @rdname geolight_map
#' @export
geolight_map_twilight <- function(tag, compute_known = formals(geolight_map)$compute_known) {
  tag_assert(tag, "twl_calib")
  tag_assert(tag, "setmap")

  assertthat::assert_that(is.logical(compute_known), length(compute_known) == 1)

  twl <- get_twl_include(tag, compute_known = compute_known)

  g <- map_expand(tag$param$tag_set_map$extent, tag$param$tag_set_map$scale)

  z <- geolight_solar(twl$twilight[twl$include], lat = g$lat, lon = g$lon)

  lk <- array(
    stats::approx(
      tag$param$geolight_map$twl_calib$x,
      tag$param$geolight_map$twl_calib$y,
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

  twl$stap_id_grp <- twl$stap_id
  twl$stap_id <- seq_len(nrow(twl))
  twl$start <- twl$twilight
  twl$end <- twl$twilight

  tag$map_twilight <- map_create(
    data = pgz,
    extent = tag$param$tag_set_map$extent,
    scale = tag$param$tag_set_map$scale,
    stap = twl,
    id = tag$param$id,
    type = "twilight"
  )

  tag
}
