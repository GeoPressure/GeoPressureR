library(shiny)
# Initialize parameters and shiny::reactive expressions
init <- function(
  .tag,
  .stapath,
  .twl,
  input,
  thr_likelihood = 0.95, # Threshold for likelihood map display
  llp_param = 1 # Parameter for likelihood calculation
) {
  rv <- list()
  if (is.null(.stapath)) {
    .stapath <- data.frame(
      stap_id = integer(),
      start = as.POSIXct(character(0), tz = "UTC"),
      end = as.POSIXct(character(0), tz = "UTC"),
      lat = numeric(),
      lon = numeric(),
      known_lat = numeric(),
      known_lon = numeric(),
      duration = numeric(),
      include = logical()
    )
  }
  if (!("known_lat" %in% names(.stapath))) {
    .stapath$known_lat <- rep(NA_real_, nrow(.stapath))
  }
  if (!("known_lon" %in% names(.stapath))) {
    .stapath$known_lon <- rep(NA_real_, nrow(.stapath))
  }
  if (!("include" %in% names(.stapath))) {
    .stapath$include <- rep(TRUE, nrow(.stapath))
  }

  # Color palette for stationary periods
  rv$col <- rep(RColorBrewer::brewer.pal(8, "Dark2"), times = 20)

  # Toggle states for buttons
  rv$drawing <- shiny::reactiveVal(NULL)
  rv$is_modifying <- shiny::reactiveVal(FALSE)
  rv$is_edit <- shiny::reactiveVal(FALSE)
  rv$zoom_state <- shiny::reactiveVal(NULL)

  # Data shiny::reactive values
  rv$stapath <- shiny::reactiveVal(.stapath)
  rv$twl <- shiny::reactiveVal(.twl)
  tag_for_map <- .tag
  tag_for_map$stap <- .stapath

  # Get known positions
  rv$known_positions <- .stapath |>
    dplyr::filter(!is.na(known_lat), !is.na(known_lon)) |>
    dplyr::select(stap_id, known_lat, known_lon, duration)

  # Map related stuff
  g <- NULL
  rv$extent <- NULL
  rv$map_grid <- NULL
  rv$has_map <- FALSE
  if (tag_assert(tag_for_map, "setmap", "logical") && nrow(.stapath) > 0) {
    rv$has_map <- TRUE

    g <- GeoPressureR::map_expand(
      tag_for_map$param$tag_set_map$extent,
      tag_for_map$param$tag_set_map$scale
    )
    rv$extent <- g$extent
    rv$map_grid <- g
  } else {
    shinyjs::hide("map_container")
  }

  compute_known <- .tag$param$geolight_map[["compute_known"]]
  if (is.null(compute_known)) {
    compute_known <- FALSE
  }
  rv$compute_known <- compute_known
  fitted_location_duration <- .tag$param$geolight_map[["fitted_location_duration"]]
  if (is.null(fitted_location_duration)) {
    fitted_location_duration <- 30
  }
  twl_calib_adjust <- .tag$param$geolight_map[["twl_calib_adjust"]]
  if (is.null(twl_calib_adjust)) {
    twl_calib_adjust <- 1
  }

  twl_calib <- .tag$param$geolight_map[["twl_calib"]]

  if (rv$has_map) {
    if (is.null(twl_calib)) {
      print(fitted_location_duration)
      tag_calib <- GeoPressureR:::geolight_map_calibrate(
        tag = tag_for_map,
        twl_calib_adjust = twl_calib_adjust,
        fitted_location_duration = fitted_location_duration,
        quiet = FALSE
      )
      twl_calib <- tag_calib$param$geolight_map[["twl_calib"]]
      tag_for_map$param$geolight_map[["twl_calib"]] <- twl_calib
    }
  }
  rv$twl_calib <- shiny::reactiveVal(twl_calib)

  # Pre-compute twilight likelihood maps using GeoPressureR's structure
  if (rv$has_map && !is.null(twl_calib)) {
    tag_likelihood <- tag_for_map
    tag_likelihood$stap <- .stapath
    tag_likelihood$param$geolight_map[["twl_calib"]] <- twl_calib
    tag_likelihood <- GeoPressureR:::geolight_map_likelihood(
      tag = tag_likelihood,
      compute_known = compute_known,
      quiet = FALSE
    )
    rv$map_light_twl <- shiny::reactiveVal(tag_likelihood$map_light_twl)
  } else {
    rv$map_light_twl <- shiny::reactiveVal(NULL)
  }

  # EPSG:3857 projection calculations
  res_proj <- NULL
  origin_proj <- NULL
  if (rv$has_map) {
    lonInEPSG3857 <- (g$lon * 20037508.34 / 180)
    latInEPSG3857 <- (log(tan((90 + g$lat) * pi / 360)) / (pi / 180)) *
      (20037508.34 / 180)
    fac_res_proj <- 4
    res_proj <- c(
      stats::median(diff(lonInEPSG3857)),
      stats::median(abs(diff(latInEPSG3857))) / fac_res_proj
    )
    origin_proj <- c(
      stats::median(lonInEPSG3857),
      stats::median(latInEPSG3857)
    )
  }

  # Likelihood calculation helper
  rv$llp_param <- shiny::reactiveVal(llp_param)
  twl_llp <- function(n) rv$llp_param() * log(n) / n

  rv$map_light_aggregate <- function(stapath_override) {
    map_light_twl <- rv$map_light_twl()
    if (is.null(map_light_twl) || is.null(map_light_twl$data)) {
      return(NULL)
    }

    include <- map_light_twl$stap$include
    twl_ <- rv$twl()
    include <- include & twl_$label != "discard"

    stap_id_override <- GeoPressureR:::find_stap(
      stapath_override,
      map_light_twl$stap$twilight
    )
    is_gap <- abs(stap_id_override - round(stap_id_override)) > 1e-6
    include <- include & !is_gap
    map_light_twl$stap$include <- include

    tag_agg <- tag_for_map
    tag_agg$stap <- stapath_override
    tag_agg$map_light_twl <- map_light_twl

    tag_agg <- GeoPressureR:::geolight_map_aggregate(
      tag = tag_agg,
      compute_known = compute_known,
      twl_llp = twl_llp,
      keep_twl = TRUE,
      quiet = TRUE
    )

    tag_agg[["map_light"]]
  }

  # Reactive: Calculate likelihood map
  rv$map_likelihood <- shiny::reactive({
    if (!rv$has_map) {
      return(NULL)
    }

    map_light <- rv$map_light_aggregate(rv$stapath())
    if (is.null(map_light) || is.null(map_light$data)) {
      return(matrix(NA_real_, nrow = g$dim[1], ncol = g$dim[2]))
    }

    idx <- as.numeric(input$stap_id)
    if (is.na(idx) || idx < 1 || idx > length(map_light$data)) {
      return(matrix(NA_real_, nrow = g$dim[1], ncol = g$dim[2]))
    }

    l <- map_light$data[[idx]]
    if (is.null(l)) {
      return(matrix(NA_real_, nrow = g$dim[1], ncol = g$dim[2]))
    }

    total <- sum(l, na.rm = TRUE)
    if (is.na(total) || total == 0) {
      return(matrix(NA_real_, nrow = g$dim[1], ncol = g$dim[2]))
    }
    m <- l / total

    # Find threshold of percentile
    ms <- sort(m)
    id_prob_percentile <- sum(cumsum(ms) < (1 - thr_likelihood))
    thr_prob <- ms[id_prob_percentile + 1]

    # Set to NA all values below this threshold
    m[m < thr_prob] <- NA

    matrix(m, nrow = g$dim[1], ncol = g$dim[2])
  })

  # Reactive: Project map for display
  rv$map_display <- shiny::reactive({
    map_likelihood_ <- rv$map_likelihood()
    if (!rv$has_map || is.null(map_likelihood_)) {
      return(NULL)
    }

    terra::project(
      terra::rast(
        simplify2array(map_likelihood_),
        extent = g$extent,
        crs = "epsg:4326"
      ),
      "epsg:3857",
      method = "near",
      res = res_proj,
      origin = origin_proj
    )
  })

  # Reactive: Generate contour display
  rv$contour_display <- shiny::reactive({
    map_likelihood_ <- rv$map_likelihood()
    if (!rv$has_map || is.null(map_likelihood_)) {
      return(data.frame(lng = numeric(0), lat = numeric(0)))
    }

    terra::rast(
      simplify2array(!is.na(map_likelihood_)),
      extent = g$extent,
      crs = "epsg:4326"
    ) |>
      terra::disagg(fact = 10, method = "bilinear") |>
      terra::as.contour(levels = 0.5) |>
      sf::st_as_sf() |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::rename(lng = X, lat = Y)
  })

  rv
}
