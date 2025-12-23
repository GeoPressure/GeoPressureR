#' Plot a `path`
#'
#' @description
#' This function plots a `path` data.frame. This function is used in [plot.map()].
#
#' @param path a GeoPressureR `path` data.frame.
#' @param plot_leaflet logical defining if the plot is an interactive `leaflet` map or a static
#' basic plot.
#' @param map optional `map` object to plot the path on top of.
#' @param provider tile provider name (see `leaflet::providers`).
#' @param provider_options tile options. See leaflet::addProviderTiles() and
#' leaflet::providerTileOptions()
#' @param pad padding of the map in degree lat-lon (only for `plot_leaflet = FALSE`).
#' @param polyline list of parameters passed to `leaflet::addPolylines()`
#' @param circle list of parameters passed to `leaflet::addCircleMarkers()`
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE)
#' })
#'
#' path <- data.frame(
#'   stap_id = 1:4,
#'   lon = c(17.05, 16.2, NA, 15.4),
#'   lat = c(48.9, 47.8, NA, 46.5),
#'   known_lon = c(17.05, NA, NA, NA),
#'   known_lat = c(48.9, NA, NA, NA),
#'   start = as.POSIXct(
#'     c("2018-01-01", "2018-01-03", "2018-01-05", "2018-01-07"),
#'     tz = "UTC"
#'   ),
#'   end = as.POSIXct(
#'     c("2018-01-02", "2018-01-04", "2018-01-06", "2018-01-08"),
#'     tz = "UTC"
#'   )
#' )
#'
#' plot_path(path)
#'
#' plot_path(path, plot_leaflet = FALSE)
#'
#' @family path
#' @seealso [plot.map()]
#' @export
plot_path <- function(
  path,
  plot_leaflet = TRUE,
  map = NULL,
  provider = "Esri.WorldTopoMap",
  provider_options = leaflet::providerTileOptions(),
  pad = 3,
  polyline = NULL,
  circle = NULL
) {
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(assertthat::has_name(path, c("lat", "lon")))

  # Calculate duration if start and end available
  if (all(c("start", "end") %in% names(path))) {
    path$duration <- stap2duration(path)
  } else {
    path$duration <- 1
  }

  # Get number of path
  if (!("j" %in% names(path))) {
    path$j <- 1
  }

  # Define point fill color
  path$point_fill <- "black"
  if (all(c("known_lat", "known_lon") %in% names(path))) {
    known_idx <- !is.na(path$known_lat) & !is.na(path$known_lon)
    path$point_fill[known_idx] <- "#D1495B"
  }
  if ("known" %in% names(path)) {
    path$point_fill[path$known] <- "#D1495B"
  }
  # Zenith color needs to be defined after known positions to override
  if ("zenith" %in% names(path)) {
    path$point_fill[!is.na(path$zenith)] <- "#F2C14E"
  }

  if (plot_leaflet) {
    return(plot_path_leaflet(
      path = path,
      map = map,
      provider = provider,
      provider_options = provider_options,
      polyline = polyline,
      circle = circle
    ))
  }

  return(plot_path_static(path, pad = pad))
}

#' @noRd
plot_path_static <- function(path, pad = 3) {
  bbox <- list(
    min_lon = min(path$lon, na.rm = TRUE) - pad,
    max_lon = max(path$lon, na.rm = TRUE) + pad,
    min_lat = min(path$lat, na.rm = TRUE) - pad,
    max_lat = max(path$lat, na.rm = TRUE) + pad
  )

  world <- ggplot2::map_data("world")
  intersecting_countries <- unique(world[
    world$long >= bbox$min_lon &
      world$long <= bbox$max_lon &
      world$lat >= bbox$min_lat &
      world$lat <= bbox$max_lat,
    "region"
  ])
  map_data_countries <- ggplot2::map_data(
    "world",
    region = intersecting_countries
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = map_data_countries,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = "#f7f7f7",
      color = "#e0e0e0",
      linewidth = 0.2
    )

  path_full <- path[!is.na(path$lat) & !is.na(path$lon), ]
  if (nrow(path_full) < nrow(path)) {
    p <- p +
      ggplot2::geom_path(
        data = path_full,
        ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$j),
        color = "black",
        linewidth = 1,
        alpha = 0.2
      )
  }

  p +
    ggplot2::geom_path(
      data = path,
      ggplot2::aes(x = .data$lon, y = .data$lat, group = .data$j),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = path_full,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat,
        size = log(.data$duration),
        fill = .data$point_fill
      ),
      color = "black",
      shape = 21
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::coord_fixed(
      ratio = 1.3,
      xlim = c(bbox$min_lon, bbox$max_lon),
      ylim = c(bbox$min_lat, bbox$max_lat)
    ) +
    ggplot2::theme_minimal()
}


#' @noRd
plot_path_leaflet <- function(
  path,
  map = NULL,
  provider = "Esri.WorldTopoMap",
  provider_options = leaflet::providerTileOptions(),
  polyline = NULL,
  circle = NULL
) {
  polyline <- merge_params(
    list(
      stroke = TRUE,
      color = "black",
      weight = 5,
      opacity = 0.7,
      dashArray = NULL
    ),
    polyline
  )

  if (is.null(map)) {
    map <- leaflet::leaflet(height = 600) |>
      leaflet::addProviderTiles(
        provider = provider,
        options = provider_options
      )
  }

  duration <- if ("duration" %in% names(path)) {
    path$duration
  } else if (all(c("start", "end") %in% names(path))) {
    stap2duration(path)
  } else {
    rep(1, nrow(path))
  }
  radius <- duration^(0.25) * 6
  if ("stap_id" %in% names(path)) {
    label <- glue::glue("#{path$stap_id}, {round(duration, 1)} days")
  } else {
    label <- glue::glue("{round(duration, 1)} days")
  }

  circle <- merge_params(
    list(
      radius = radius,
      stroke = TRUE,
      color = "white",
      weight = 2,
      opacity = 1,
      fill = if (!"interp" %in% names(path)) {
        rep(TRUE, nrow(path))
      } else {
        ifelse(is.na(path$interp), TRUE, !path$interp)
      },
      fillColor = path$point_fill,
      fillOpacity = 0.8,
      label = label
    ),
    circle
  )

  # Remove position of stap not included/not available
  path_full <- path[!is.na(path$lat), ]
  path_full <- path_full[!is.na(path_full$lon), ]

  # Get number of path
  unique_j <- unique(path$j)

  # Plot trajectory of all available point in grey
  if (nrow(path_full) < nrow(path)) {
    polyline_full <- polyline
    # polyline_full$weight <- polyline_full$weight/2
    polyline_full$opacity <- polyline_full$opacity / 2
    polyline_full$color <- "grey"

    for (j in unique_j) {
      map <- do.call(
        leaflet::addPolylines,
        c(
          list(
            map = map,
            lng = path_full$lon[path_full$j == j],
            lat = path_full$lat[path_full$j == j],
            group = path$j[path$j == j]
          ),
          polyline_full
        )
      )
    }
  }

  # Overlay with trajectory of consecutive position in black.
  for (j in unique_j) {
    map <- do.call(
      leaflet::addPolylines,
      c(
        list(
          map = map,
          lng = path$lon[path$j == j],
          lat = path$lat[path$j == j],
          group = path$j[path$j == j]
        ),
        polyline
      )
    )
  }

  path_full <- path[!is.na(path$lat), ]
  if (nrow(path_full)) {
    map <- do.call(
      leaflet::addCircleMarkers,
      c(
        list(
          map = map,
          lng = path_full$lon,
          lat = path_full$lat,
          group = path_full$j
        ),
        circle
      )
    )
  }
  return(map)
}
