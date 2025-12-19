# Utility functions for GeoPressure trainset Shiny app

get_plot_styles <- function(active_series, label_pres, label_acc = NULL, acc_has_lines = TRUE) {
  pressure_colors <- get_marker_colors(label_pres)

  # Overview line (used for range slider) should stay lightweight and unobtrusive.
  pressure_overview_line_style <- list(
    "line.color" = "rgba(0,0,0,0.18)",
    "line.width" = 1
  )

  # Detail line (visible window) can be emphasized when pressure is active.
  pressure_detail_line_style <- if (active_series == "pressure") {
    list(
      "line.color" = "rgba(0,0,0,0.75)",
      "line.width" = 1.5
    )
  } else {
    list(
      "line.color" = "rgba(0,0,0,0.12)",
      "line.width" = 1
    )
  }

  pressure_markers_style <- list(
    "marker.size" = if (active_series == "pressure") 10 else 4,
    "marker.opacity" = if (active_series == "pressure") 0.8 else 0.4,
    "marker.color" = list(pressure_colors)
  )

  result <- list(
    pressure_overview_line_style = pressure_overview_line_style,
    pressure_detail_line_style = pressure_detail_line_style,
    pressure_markers_style = pressure_markers_style
  )

  if (!is.null(label_acc)) {
    acceleration_colors <- get_marker_colors(label_acc)
    acceleration_style <- list(
      "marker.size" = if (active_series == "acceleration") 10 else 5,
      "marker.opacity" = if (active_series == "acceleration") 0.8 else 0.3,
      "marker.color" = list(acceleration_colors)
    )
    if (isTRUE(acc_has_lines)) {
      acceleration_style[["line.color"]] <- if (active_series == "acceleration") {
        "black"
      } else {
        "rgba(0,0,0,0.2)"
      }
      acceleration_style[["line.width"]] <- if (active_series == "acceleration") 2 else 1
    }
    result$acceleration_style <- acceleration_style
  }

  result
}

apply_plot_styling <- function(
  plot_proxy,
  active_series,
  label_pres,
  label_acc = NULL,
  acc_has_lines = TRUE,
  pressure_overview_trace = 0,
  pressure_markers_trace = 1,
  pressure_detail_trace = 2,
  acceleration_trace = 3
) {
  styles <- get_plot_styles(active_series, label_pres, label_acc, acc_has_lines = acc_has_lines)

  plot_proxy |>
    plotly::plotlyProxyInvoke("restyle", list(selectedpoints = NULL))

  plot_proxy |>
    plotly::plotlyProxyInvoke(
      "restyle",
      styles$pressure_overview_line_style,
      list(pressure_overview_trace)
    )

  plot_proxy |>
    plotly::plotlyProxyInvoke(
      "restyle",
      styles$pressure_markers_style,
      list(pressure_markers_trace)
    )

  plot_proxy |>
    plotly::plotlyProxyInvoke(
      "restyle",
      styles$pressure_detail_line_style,
      list(pressure_detail_trace)
    )

  if (!is.null(label_acc) && !is.null(styles$acceleration_style)) {
    plot_proxy |>
      plotly::plotlyProxyInvoke("restyle", styles$acceleration_style, list(acceleration_trace))
  }
}

# Define color mapping function for plot markers
get_marker_colors <- function(labels) {
  labels_chr <- as.character(labels)

  # Base colors for special cases
  colors <- ifelse(
    is.na(labels_chr) | labels_chr == "",
    "black", # No label = black
    ifelse(
      labels_chr == "discard",
      "grey", # Discard = grey
      ifelse(labels_chr == "flight", "red", "")
    )
  ) # Flight = red

  # Get unique additional labels (excluding NA, "", "discard", "flight")
  additional_labels <- unique(labels_chr[
    !is.na(labels_chr) & labels_chr != "" & labels_chr != "discard" & labels_chr != "flight"
  ])

  # Nice color palette for additional categories (colorbrewer-inspired)
  # Excludes red (flight) and grey (discard) colors
  category_palette <- c(
    "#1f77b4", # blue
    "#ff7f0e", # orange
    "#2ca02c", # green
    "#9467bd", # purple
    "#8c564b", # brown
    "#e377c2", # pink
    "#bcbd22", # olive
    "#17becf" # cyan
  )

  # Assign stable colors to elev_<n> based on <n> (independent of visible window)
  is_elev <- grepl("^elev_\\d+$", labels_chr)
  if (any(is_elev, na.rm = TRUE)) {
    elev_num <- suppressWarnings(as.integer(sub("^elev_", "", labels_chr[is_elev])))
    idx <- ((elev_num - 1L) %% length(category_palette)) + 1L
    colors[which(is_elev)] <- category_palette[idx]
    additional_labels <- setdiff(additional_labels, unique(labels_chr[is_elev]))
  }

  # Assign colors to additional labels
  if (length(additional_labels) > 0) {
    additional_labels <- sort(additional_labels)
    for (i in seq_along(additional_labels)) {
      label <- additional_labels[i]
      color <- category_palette[((i - 1) %% length(category_palette)) + 1]
      colors[labels_chr == label & !is.na(labels_chr)] <- color
    }
  }

  colors
}

downsample_indices <- function(n, max_points) {
  if (is.null(max_points) || is.infinite(max_points) || max_points <= 0 || n <= max_points) {
    return(seq_len(n))
  }

  # Evenly spaced indices; always include endpoints.
  idx <- unique(pmax.int(1L, pmin.int(n, as.integer(round(seq(1, n, length.out = max_points))))))
  idx <- sort.int(idx)
  if (length(idx) == 0L) {
    return(integer(0))
  }
  if (idx[[1]] != 1L) {
    idx <- c(1L, idx)
  }
  if (idx[[length(idx)]] != n) {
    idx <- c(idx, n)
  }
  idx
}

range_indices_sorted <- function(time_num, xmin, xmax) {
  n <- length(time_num)
  if (n == 0L) {
    return(integer(0))
  }

  if (is.na(xmin) || is.na(xmax)) {
    return(integer(0))
  }
  if (xmin > xmax) {
    tmp <- xmin
    xmin <- xmax
    xmax <- tmp
  }

  lo <- findInterval(xmin, time_num)
  if (lo == 0L) {
    lo <- 1L
  } else if (lo < n && time_num[[lo]] < xmin) {
    lo <- lo + 1L
  }

  hi <- findInterval(xmax, time_num, rightmost.closed = TRUE)
  if (hi == 0L) {
    return(integer(0))
  }

  if (lo > hi) {
    return(integer(0))
  }
  seq.int(lo, hi)
}
