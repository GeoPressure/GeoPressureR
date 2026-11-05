#' Assert the status of a `tag`
#'
#' This function checks the condition of a `tag` object.
#'
#' @param tag a GeoPressureR `tag` object.
#' @param condition condition to assert `tag` for. One of `"tag"` (default), `"read"`,
#' `"pressure"`, `"light"`, `"acceleration"`, `"temperature_internal"`,
#' `"temperature_external"`, `"magnetic"`, `"label"`, `"stap"`, `"setmap"`,
#' `"map_pressure"`, `"map_pressure_mse"`, `"map_pressure_mask"`, `"map_light"`,
#' `"map_light_twl"`, `"mask_water"`, `"map_magnetic"`, `"map_magnetic_intensity"`,
#' `"map_magnetic_inclination"`, `"twl_calib"`, and `"twilight"`.
#' @param type Message type to display. One of `"abort"` (default), `"warn"`, `"inform"`,
#' or `"logical"` (return TRUE/FALSE without messaging).
#'
#' @return logical indicating the `tag` object has the relevant element
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |> tag_label(quiet = TRUE)
#' })
#'
#' tag_assert(tag)
#'
#' tag_assert(tag, "stap")
#'
#' tag_assert(tag, "setmap", type = "warn")
#'
#' tag_assert(tag, "map_pressure", type = "inform")
#'
#' tag_assert(tag, "setmap", type = "logical")
#'
#' @export
tag_assert <- function(tag, condition = "tag", type = "abort") {
  if (identical(type, "")) {
    type <- "logical"
  }
  type <- match.arg(type, choices = c("abort", "warn", "inform", "logical"))
  status <- tag_status(tag)

  # Message shown for each condition when it is not met
  msgs <- list(
    tag = c("x" = "tag is not a {.var tag} object."),
    read = c("x" = "Sensors data not yet read. Use {.fun tag_create}."),
    pressure = c("x" = "The {.var tag} object does not have {.field pressure} data."),
    light = c("x" = "The {.var tag} object does not have {.field light} data."),
    acceleration = c("x" = "The {.var tag} object does not have {.field acceleration} data."),
    temperature_internal = c(
      "x" = "The {.var tag} object does not have {.field temperature_internal} data."
    ),
    temperature_external = c(
      "x" = "The {.var tag} object does not have {.field temperature_external} data."
    ),
    magnetic = c("x" = "The {.var tag} object does not have {.field magnetic} data."),
    label = c(
      "x" = "The {.var tag} object has not yet been labelled.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    ),
    stap = c(
      "x" = "Stationary periods have not yet been computed for {.var tag}.",
      ">" = "Use {.fun tag_label} to define the stationary periods."
    ),
    setmap = c(
      "x" = "The parameters for the geographical and stationary period have not yet been defined in {.var tag}.",
      ">" = "Use {.fun tag_set_map} to define them."
    ),
    map_pressure = c(
      "x" = "The pressure likelihood map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geopressure_map} to compute the maps."
    ),
    map_pressure_mse = c(
      "x" = "The pressure mean square error map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geopressure_map_mismatch} to compute the maps."
    ),
    map_pressure_mask = c(
      "x" = "The pressure mask map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geopressure_map_mismatch} to compute the maps."
    ),
    twl_calib = c(
      "x" = "The twilight calibration has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geolight_map_calibrate} to compute the twilight calibration stored in {.var tag$param$geolight_map$twl_calib}."
    ),
    map_light_twl = c(
      "x" = "The twilight likelihood map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geolight_map_likelihood} to compute the maps."
    ),
    twilight = c(
      "x" = "The twilight has not yet been computed for {.var tag}",
      ">" = "Use {.fun twilight_create} to compute the twilight"
    ),
    map_light = c(
      "x" = "The light likelihood map has not yet been computed for {.var tag}.",
      ">" = "Use {.fun geolight_map} to compute the maps."
    ),
    mask_water = c("x" = "The water mask map has not yet been computed for {.var tag}."),
    map_magnetic = c("x" = "The magnetic likelihood map has not yet been computed for {.var tag}."),
    map_magnetic_intensity = c(
      "x" = "The magnetic intensity map has not yet been computed for {.var tag}."
    ),
    map_magnetic_inclination = c(
      "x" = "The magnetic inclination map has not yet been computed for {.var tag}."
    )
  )

  # A non-tag object always reports the tag message, before checking the condition
  if (!inherits(tag, "tag")) {
    msg <- msgs$tag
  } else if (condition %in% names(msgs)) {
    msg <- msgs[[condition]]
  } else {
    cli::cli_abort(c(
      "x" = "Condition {.var {condition}} is unknown."
    ))
  }

  if (condition %in% status) {
    return(TRUE)
  }

  if (type == "logical") {
    return(FALSE)
  } else if (type == "inform") {
    cli::cli_bullets(msg)
  } else if (type == "warn") {
    cli::cli_warn(msg)
  } else if (type == "abort") {
    cli::cli_abort(msg)
  }
  invisible(FALSE)
}

#' Get tag processing status
#'
#' Internal helper used by GeoPressure companion packages.
#'
#' @param tag a GeoPressureR `tag` object.
#'
#' @return Character vector of status labels.
#'
#' @keywords internal
#' @export
tag_status <- function(tag) {
  if (!inherits(tag, "tag")) {
    return(c())
  }
  status <- c("tag")

  sensors <- intersect(tag_sensors, names(tag))
  status <- c(status, rbind(rep("read", length(sensors)), sensors))
  if (
    (assertthat::has_name(tag, "pressure") && assertthat::has_name(tag$pressure, "label")) ||
      (assertthat::has_name(tag, "acceleration") && assertthat::has_name(tag$acceleration, "label"))
  ) {
    status <- append(status, "label")
  }
  if (
    assertthat::has_name(tag, "stap") &&
      assertthat::has_name(tag$stap, "stap_id")
  ) {
    status <- append(status, "stap")
  }
  if (
    assertthat::has_name(tag$param$tag_set_map, c("extent", "scale")) &&
      assertthat::has_name(tag$stap, c("known_lat", "known_lon", "include"))
  ) {
    status <- append(status, "setmap")
  }
  if (assertthat::has_name(tag, "map_pressure")) {
    status <- append(status, "map_pressure")
  }
  if (assertthat::has_name(tag, "map_pressure_mse")) {
    status <- append(status, "map_pressure_mse")
  }
  if (assertthat::has_name(tag, "map_pressure_mask")) {
    status <- append(status, "map_pressure_mask")
  }
  if (assertthat::has_name(tag, "mask_water")) {
    status <- append(status, "mask_water")
  }
  if (
    assertthat::has_name(tag$param, "geolight_map") &&
      assertthat::has_name(tag$param$geolight_map, "twl_calib")
  ) {
    status <- append(status, "twl_calib")
  }
  if (assertthat::has_name(tag, "map_light_twl")) {
    status <- append(status, "map_light_twl")
  }
  if (assertthat::has_name(tag, "twilight")) {
    status <- append(status, "twilight")
  }
  if (assertthat::has_name(tag, "map_light")) {
    status <- append(status, "map_light")
  }
  if (assertthat::has_name(tag, "map_magnetic")) {
    status <- append(status, "map_magnetic")
  }
  if (assertthat::has_name(tag, "map_magnetic_intensity")) {
    status <- append(status, "map_magnetic_intensity")
  }
  if (assertthat::has_name(tag, "map_magnetic_inclination")) {
    status <- append(status, "map_magnetic_inclination")
  }

  status
}
