#' Plot twilight calibration diagnostics
#'
#' This function plots the stacked calibration histogram and fitted density. If a `path` is
#' provided, it instead plots the solar zenith angle histogram derived from `tag$twilight` and the
#' path, with the calibration density overlaid.
#'
#' @param x a GeoPressureR `twl_calib` object or a `tag` object with calibration.
#' @param tag a GeoPressureR `tag` object with twilight calibration (used by
#' `plot_twl_calib_path()`).
#' @param path a GeoPressureR `path` or `pressurepath` data.frame. If provided, plot the path-based
#' calibration diagnostic instead of the stacked calibration histogram.
#' @param warning_ci Width of the symmetric confidence interval used to flag staps with twilights
#' outside the calibrated range (e.g., `0.95` for 2.5%/97.5%).
#' @param plot_plotly logical to use `plotly`.
#' @param ... unused
#'
#' @return a ggplot or ggplotly object
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     tag_set_map(
#'       extent = c(-16, 23, 0, 50),
#'       scale = 5,
#'       known = data.frame(
#'         stap_id = 1,
#'         known_lon = 17.05,
#'         known_lat = 48.9
#'       )
#'     ) |>
#'     twilight_create() |>
#'     twilight_label_read() |>
#'     geolight_map_calibrate()
#' })
#'
#' # Histogram + kernel density
#' plot_twl_calib(tag)
#'
#' # With fitted location duration of 5 days minimum
#' tag <- geolight_map_calibrate(tag, fitted_location_duration = 5)
#'
#' # Path-based diagnostic (uses tag$twilight)
#' path <- tag$stap[, c("stap_id", "start", "end")]
#' path$lon <- ifelse(is.na(tag$stap$known_lon), 0, tag$stap$known_lon)
#' path$lat <- ifelse(is.na(tag$stap$known_lat), 0, tag$stap$known_lat)
#' plot_twl_calib(tag, path = path, plot_plotly = FALSE)
#'
#' @family geolight
#' @export
plot_twl_calib <- function(
  x,
  path = NULL,
  warning_ci = 0.95,
  plot_plotly = TRUE,
  ...
) {
  if (!is.null(path)) {
    if (!inherits(x, "tag")) {
      cli::cli_abort(c(
        "x" = "When {.var path} is provided, {.var x} must be a {.cls tag} object.",
        ">" = "Use {.fun plot_twl_calib_path} directly if you have a tag."
      ))
    }
    return(plot_twl_calib_path(
      tag = x,
      path = path,
      warning_ci = warning_ci,
      plot_plotly = plot_plotly
    ))
  }

  if (inherits(x, "tag")) {
    tag_assert(x, "twl_calib")
    twl_calib <- x$param$geolight_map$twl_calib
  } else {
    twl_calib <- x
  }

  stap <- twl_calib$calib_stap
  stap$duration <- stap2duration(stap)
  stap$label <- glue::glue(
    "#{stap$stap_id} ({format(round(stap$duration), trim = TRUE)}d) {ifelse(is.na(stap$zenith), 'known', 'fitted')}"
  )
  stap_type <- ifelse(is.na(stap$zenith), "known", "fitted")
  names(stap_type) <- as.character(stap$stap_id)

  stap_label <- stap$label[match(names(twl_calib$hist_counts), stap$stap_id)]
  names(stap_label) <- names(twl_calib$hist_counts)

  hist_df <- data.frame(
    stap = rep(stap_label, each = length(twl_calib$hist_mids)),
    zenith_angle = rep(twl_calib$hist_mids, times = length(twl_calib$hist_counts)),
    count = unlist(twl_calib$hist_counts, use.names = FALSE)
  )

  line_df <- data.frame(
    x = twl_calib$x,
    y = twl_calib$y * sum(unlist(twl_calib$hist_counts, use.names = FALSE)) * twl_calib$binwidth
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = hist_df,
      ggplot2::aes(
        x = .data$zenith_angle,
        y = .data$count,
        fill = .data$stap
      ),
      width = twl_calib$binwidth
    ) +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "black",
      linewidth = 0.8
    ) +
    ggplot2::labs(
      x = "Solar zenith angle",
      y = "Count of twilights",
      fill = "Calibration stap"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      values = c(
        stats::setNames(
          grDevices::hcl.colors(sum(stap_type == "known"), "Teal"),
          stap_label[stap_type == "known"]
        ),
        stats::setNames(
          grDevices::hcl.colors(sum(stap_type == "fitted"), "YlOrRd"),
          stap_label[stap_type == "fitted"]
        )
      )
    )

  if (plot_plotly) {
    p <- p + ggplot2::theme(legend.position = "none")
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  }

  p <- p +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal"
    )
  p
}

#' @rdname plot_twl_calib
#' @export
plot_twl_calib_path <- function(
  tag,
  path,
  warning_ci = 0.9973,
  plot_plotly = TRUE
) {
  tag_assert(tag, "twl_calib")
  assertthat::assert_that(assertthat::has_name(tag, "twilight"))
  assertthat::assert_that(is.data.frame(path))
  assertthat::assert_that(all(c("lat", "lon") %in% names(path)))

  twl_calib <- tag$param$geolight_map$twl_calib
  calib_stap <- path
  calib_stap$known_lat <- calib_stap$lat
  calib_stap$known_lon <- calib_stap$lon

  twl_calib_path <- geolight_calibrate(
    twl = tag$twilight,
    calib_stap = calib_stap
  )

  # Build histogram data from calibration output
  hist_df <- data.frame(
    stap_id = rep(names(twl_calib_path$hist_counts), each = length(twl_calib_path$hist_mids)),
    zenith_angle = rep(twl_calib_path$hist_mids, times = length(twl_calib_path$hist_counts)),
    count = unlist(twl_calib_path$hist_counts, use.names = FALSE)
  )
  stap_levels <- sort(unique(as.numeric(hist_df$stap_id)))
  hist_df$stap_id <- factor(hist_df$stap_id, levels = as.character(stap_levels))
  hist_df$count_norm <- hist_df$count / stats::ave(hist_df$count, hist_df$stap_id, FUN = sum)

  # Compute symmetric CI bounds from calibrated density
  alpha <- (1 - warning_ci) / 2
  cdf <- stats::approxfun(
    twl_calib$x,
    cumsum(twl_calib$y) / sum(twl_calib$y),
    yleft = 0,
    yright = 1
  )
  z_grid <- twl_calib$x
  cdf_grid <- cdf(z_grid)
  z_q <- stats::approx(cdf_grid, z_grid, xout = c(alpha, 1 - alpha))$y

  # Flag staps with any twilight outside CI
  twl <- twilight_include(tag$twilight)
  twl <- twl[twl$include, ]
  z_calib <- lapply(
    seq_len(nrow(calib_stap)),
    function(i) {
      id <- twl$twilight >= calib_stap$start[i] &
        twl$twilight <= calib_stap$end[i]
      geolight_solar(
        date = twl$twilight[id],
        lat = calib_stap$known_lat[i],
        lon = calib_stap$known_lon[i]
      )
    }
  )
  names(z_calib) <- as.character(calib_stap$stap_id)
  ci_flag <- vapply(
    z_calib,
    function(z) {
      z <- as.numeric(z)
      z <- z[is.finite(z)]
      if (length(z) == 0) {
        return(FALSE)
      }
      any(z < z_q[1] | z > z_q[2])
    },
    logical(1)
  )
  hist_df$ci_flag <- ci_flag[as.character(hist_df$stap_id)]

  # Build labels and plot helpers
  calib_stap$duration <- stap2duration(calib_stap)
  label_df <- calib_stap[, c("stap_id", "duration")]
  label_df$stap_id <- as.character(label_df$stap_id)
  label_map <- stats::setNames(
    glue::glue("#{label_df$stap_id} ({format(round(label_df$duration), trim = TRUE)}d)"),
    label_df$stap_id
  )
  line_df <- data.frame(x = twl_calib$x, y = twl_calib$y)
  x_range <- range(hist_df$zenith_angle[hist_df$count > 0], finite = TRUE)

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = hist_df,
      ggplot2::aes(x = .data$zenith_angle, y = .data$count_norm, fill = .data$ci_flag),
      width = twl_calib_path$binwidth,
    ) +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "#c5a909ff",
      linewidth = 0.8
    ) +
    ggplot2::geom_vline(
      xintercept = z_q,
      linetype = "dashed",
      color = "#E6550D"
    ) +
    ggplot2::facet_wrap(
      ~stap_id,
      scales = "free_y",
      labeller = ggplot2::as_labeller(label_map)
    ) +
    ggplot2::labs(
      x = "Solar zenith angle",
      y = "Normalized count"
    ) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#E6550D", "FALSE" = "grey40")) +
    ggplot2::coord_cartesian(xlim = x_range) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_blank()
    )

  if (plot_plotly) {
    p <- p + ggplot2::theme(legend.position = "none")
    return(plotly::ggplotly(p, dynamicTicks = TRUE))
  }

  p
}
