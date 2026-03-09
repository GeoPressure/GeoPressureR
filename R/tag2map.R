#' Extract a likelihood `map` from a `tag`
#'
#' @param tag a GeoPressureR `tag` object.
#' @param likelihood Field of the `tag` list containing the likelihood map (character). Possible
#' value are `map_pressure`, `map_light`, `map_pressure_mse`, `map_pressure_mse`,
#' `map_pressure_mse`, `mask_water`. Default `NA` is to take the product of `map_pressure` and
#' `map_light`, or if not available, taking the first of the possible values.
#'
#' @return Likelihood map
#' @noRd
tag2map <- function(tag, likelihood = NULL) {
  likelihood <- tag2likelihood(tag, likelihood)

  map <- tag[[likelihood[1]]]

  # Deal with multiple likelihood
  if (length(likelihood) > 1) {
    for (i in seq(2, length(likelihood))) {
      map <- map * tag[[likelihood[i]]]
    }
  }
  return(map)
}

#' Return a valid likelihood map name
#
#' @inheritParams tag2map
#'
#' @return Likelihood map name
#' @noRd
tag2likelihood <- function(tag, likelihood = NULL) {
  tag_assert(tag)
  map_types <- map_type()

  # Allowed inputs are either `map_type()[[*]]$name` (tag field name(s)) or `names(map_type())`.
  type_name <- lapply(map_types, `[[`, "name")
  allowed_fields <- unique(unlist(type_name, use.names = FALSE))
  allowed_user <- unique(c(names(map_types), allowed_fields))

  # Automatic determination
  if (is.null(likelihood)) {
    # Priority 1: pressure x light (if both exist)
    if (all(c("map_pressure", "map_light") %in% names(tag))) {
      return(c("map_pressure", "map_light"))
    }

    # Priority 2: first map type entry (in `map_type()` order) that is available on the tag
    available <- vapply(type_name, function(cand) all(cand %in% names(tag)), logical(1))
    if (any(available)) {
      return(type_name[[which(available)[1]]])
    }

    cli::cli_abort(c(
      x = "No likelihood map is present in {.var tag}.",
      i = "Make sure you've run {.fun geopressure_map} and/or {.fun geolight_map}.",
      ">" = "{.var likelihood} must be one of {.val {allowed_user}}."
    ))
  }

  likelihood <- unlist(
    lapply(likelihood, function(lk) if (lk %in% names(map_types)) map_types[[lk]]$name else lk),
    use.names = FALSE
  )
  bad <- setdiff(likelihood, allowed_fields)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "x" = "The likelihood map{?s} {.val {bad}} {?is/are} not recognized.",
      ">" = "{.var likelihood} must be one of {.val {allowed_user}}."
    ))
  }
  missing <- setdiff(likelihood, names(tag))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "x" = "The likelihood map{?s} {.val {missing}} {?is/are} not present in {.var tag}.",
      "i" = "Available likelihood maps are: {.val {intersect(allowed_fields, names(tag))}}."
    ))
  }

  return(likelihood)
}
