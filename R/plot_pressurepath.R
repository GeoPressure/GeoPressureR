#' Plot a `pressurepath`
#'
#' Display a `pressurepath` data.frame as a time series or histogram
#'
#' @param pressurepath a GeoPressureR `pressurepath` data.frame.
#' @param type `"timeseries"` (default), `"histogram"`, or `"altitude"`
#' @inheritParams geopressure_map
#' @param warning_std_thr Threshold of outlier, coefficient of the [z-score](
#' https://en.wikipedia.org/wiki/Standard_score)
#' @param plot_plotly logical to use `plotly`
#'
#' @return a plot or ggplotly object.
#'
#' @examples
#' # Load the pressurepath of the most likely path computed with `geopressuretemplate()`
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   pressurepath <- load_interim("18LX", var = "pressurepath_most_likely")
#' })
#'
#' plot_pressurepath(pressurepath)
#'
#' plot_pressurepath(pressurepath, type = "histogram")
#'
#' plot_pressurepath(pressurepath, type = "altitude")
#'
#' @family pressurepath
#' @export
plot_pressurepath <- function(
  pressurepath,
  type = "timeseries",
  sd = attr(pressurepath, "sd"),
  warning_std_thr = 3,
  plot_plotly = TRUE
) {
  assertthat::assert_that(is.data.frame(pressurepath))

  type <- match.arg(
    type,
    choices = c("timeseries", "ts", "histogram", "hist", "altitude", "alt")
  )
  if (type == "ts") {
    type <- "timeseries"
  } else if (type == "hist") {
    type <- "histogram"
  } else if (type == "alt") {
    type <- "altitude"
  }

  if ("pressure_era5" %in% names(pressurepath)) {
    cli::cli_warn(c(
      "!" = "{.var pressurepath} has been created with an old version of {.pkg GeoPressureR} (<v3.2.0)",
      ">" = "For optimal performance, we suggest to re-run {.fun pressurepath_create}"
    ))
    pressurepath$surface_pressure <- pressurepath$pressure_era5
    pressurepath$surface_pressure_norm <- pressurepath$pressure_era5_norm

    id <- pressurepath$stap_id == 0
    sequence <- seq_len(nrow(pressurepath))
    pressurepath$stap_id[id] <- stats::approx(
      sequence[!id],
      pressurepath$stap_id[!id],
      sequence[id]
    )$y
  }

  assertthat::assert_that(assertthat::has_name(
    pressurepath,
    c(
      "date",
      "stap_id",
      "pressure_tag",
      "label",
      "surface_pressure",
      "altitude",
      "lat",
      "lon",
      "surface_pressure_norm"
    )
  ))

  # Per-point error, with the sd of its stap
  pp <- pressurepath_error(pressurepath, sd)
  pp <- pp[order(pp$date), ]

  if (type %in% c("timeseries", "altitude")) {
    breaks <- seq(min(pp$date), max(pp$date), by = "30 min")
    if (utils::tail(breaks, 1) < max(pp$date)) {
      breaks <- c(breaks, max(pp$date))
    }
    midpoint <- breaks[-length(breaks)] + diff(breaks) / 2
    index <- findInterval(midpoint, pp$date, all.inside = TRUE)
    sunrise <- as.numeric(pp$sunrise[index]) %% 86400
    sunset <- as.numeric(pp$sunset[index]) %% 86400
    time <- as.numeric(midpoint) %% 86400
    night <- ifelse(
      sunrise < sunset,
      time < sunrise | time >= sunset,
      time >= sunset & time < sunrise
    )
    runs <- rle(night)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1
    night <- data.frame(
      xmin = breaks[starts[which(runs$values)]],
      xmax = breaks[ends[which(runs$values)] + 1]
    )
    night_layer <- if (nrow(night)) {
      ggplot2::geom_rect(
        data = night,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax),
        ymin = -Inf,
        ymax = Inf,
        inherit.aes = FALSE,
        fill = "grey85"
      )
    }
  }

  if (type == "timeseries") {
    pp$warning <- abs(pp$error) / pp$sd >= warning_std_thr

    # convert stapelev to factor for color
    pp$stap_id <- factor(pp$stap_id)

    p <- ggplot2::ggplot(pp, ggplot2::aes(x = .data$date)) +
      night_layer +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$pressure_tag),
        color = "grey"
      ) +
      ggplot2::geom_point(
        data = pp[pp$label == "discard", ],
        ggplot2::aes(y = .data$pressure_tag),
        colour = "black"
      ) +
      ggplot2::geom_line(
        data = pp[pp$stapelev != "0|0", ],
        ggplot2::aes(y = .data$surface_pressure_norm, color = .data$stapelev)
      ) +
      ggplot2::geom_point(
        data = pp[pp$warning & pp$label != "discard" & pp$stapelev != "0|0", ],
        ggplot2::aes(y = .data$pressure_tag),
        fill = "orange",
        shape = 24,
        size = 3
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Pressure (hPa)") +
      ggplot2::theme(legend.position = "none")
  } else if (type == "histogram") {
    # Check if the empirical sd is greater than the sd used in the computation of the map

    check <- pressurepath_check(pressurepath, sd, warning_std_thr)

    # Remove error for flight
    pp <- pp[pp$stapelev %in% check$stapelev, ]

    pp$sd_exceeded <- check$sd_exceeded[match(pp$stapelev, check$stapelev)]
    pp$stapelev <- factor(pp$stapelev, levels = check$stapelev)
    pp$warning_p <- pp$sd * warning_std_thr
    pp$warning_m <- -pp$sd * warning_std_thr

    lab <- ggplot2::as_labeller(stats::setNames(
      glue::glue("{check$stapelev} - SD: {check$error_sd}"),
      check$stapelev
    ))

    p <- ggplot2::ggplot(
      pp[pp$label != "discard", ],
      ggplot2::aes(x = .data$error)
    ) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data$sd_exceeded),
        binwidth = 0.4
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = .data$warning_m),
        linetype = "dashed",
        colour = "#E6550D"
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = .data$warning_p),
        linetype = "dashed",
        colour = "#E6550D"
      ) +
      ggplot2::facet_wrap(~stapelev, scales = "free_y", labeller = lab) +
      ggplot2::scale_x_continuous(
        name = "Difference of pressure tag - ERA5 (hPa)"
      ) +
      ggplot2::scale_y_continuous(name = "Normalized histogram") +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#E6550D", "FALSE" = "black")
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_blank()
      )
  } else {
    pp_alt <- pressurepath

    # find flight
    id_flight <- pp_alt$stap_id != round(pp_alt$stap_id)
    # group flight arbitrarily using the floor of their stap_id
    pp_alt$stap_id[id_flight] <- floor(pp_alt$stap_id)[id_flight]
    # convert to factor for plotting
    pp_alt$stap_id <- factor(pp_alt$stap_id)

    p <- ggplot2::ggplot() +
      night_layer +
      ggplot2::geom_line(
        data = pp_alt,
        ggplot2::aes(
          x = .data$date,
          y = .data$altitude
        ),
        color = "grey",
        linewidth = 0.2
      ) +
      ggplot2::geom_line(
        data = pp_alt[!id_flight, ],
        ggplot2::aes(
          x = .data$date,
          y = .data$altitude,
          group = .data$stap_id,
          color = .data$stap_id
        )
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Altitude (m)") +
      ggplot2::theme(legend.position = "none")

    if (any(id_flight)) {
      p <- p +
        ggplot2::geom_line(
          data = pp_alt[id_flight, ],
          ggplot2::aes(
            x = .data$date,
            y = .data$altitude,
            group = .data$stap_id
          ),
          color = "black"
        )
    }
  }

  if (plot_plotly) {
    out <- plotly::ggplotly(p, dynamicTicks = TRUE)
    if (type != "histogram" && nrow(night) > 0) {
      y_range <- out$x$layout$yaxis$range
      out$x$data[[1]]$y[out$x$data[[1]]$y == -Inf] <- y_range[1]
      out$x$data[[1]]$y[out$x$data[[1]]$y == Inf] <- y_range[2]
    }
    return(out)
  } else {
    return(p)
  }
}

#' Check the pressure error of a `pressurepath` per elevation level
#'
#' @description
#' Return one row per stationary period and elevation level (stapelev) with the numbers behind
#' `plot_pressurepath(type = "histogram")`: the error between tag pressure and the normalised ERA5
#' surface pressure (`pressure_tag - surface_pressure_norm`), its empirical standard deviation
#' compared with the `sd` used for the pressure map, its offset and its outliers.
#'
#' It also works on the output of [`geopressure_timeseries()`] for one stationary period, which gives
#' the fit of the pressure at a single position.
#'
#' Discarded points are excluded from the error; flights are not summarised.
#'
#' @param pressurepath a GeoPressureR `pressurepath` data.frame (see [`pressurepath_create()`]) or
#' the output of [`geopressure_timeseries()`] with `pressure`.
#' @param sd standard deviation of the pressure error (hPa), one value or one per stationary period.
#' Default is the `sd` used by [`geopressure_map()`], or 1 if not available.
#' @inheritParams plot_pressurepath
#'
#' @return A data.frame with one row per stapelev:
#' - `stapelev`: `"{stap_id}|{elevation level}"`.
#' - `stap_id`.
#' - `n`: number of points used, `n_discard`: number of discarded points.
#' - `error_sd`: empirical standard deviation of the error (hPa).
#' - `error_offset`: mean ERA5 surface pressure minus mean tag pressure (hPa).
#' - `sd`: standard deviation used for the pressure map (hPa).
#' - `sd_exceeded`: `error_sd > sd`.
#' - `n_outlier`: number of points with `|error| >= warning_std_thr * sd`.
#' - `error_max`: largest absolute error (hPa).
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   pressurepath <- load_interim("18LX", var = "pressurepath_most_likely")
#' })
#'
#' pressurepath_check(pressurepath)
#'
#' @family pressurepath
#' @keywords internal
#' @export
pressurepath_check <- function(
  pressurepath,
  sd = attr(pressurepath, "sd"),
  warning_std_thr = 3
) {
  assertthat::assert_that(is.data.frame(pressurepath))
  assertthat::assert_that(assertthat::has_name(
    pressurepath,
    c("date", "stap_id", "pressure_tag", "label", "surface_pressure", "surface_pressure_norm")
  ))
  assertthat::assert_that(is.null(sd) || is.numeric(sd))
  assertthat::assert_that(assertthat::is.number(warning_std_thr))

  # Error of ground points, grouped by stapelev in stap and elevation order
  pp <- pressurepath_error(pressurepath, sd)
  pp <- pp[pp$stap_id == round(pp$stap_id) & pp$stap_id != 0, ]
  elev <- as.numeric(sub(".*\\|", "", pp$stapelev))
  pp$stapelev <- factor(pp$stapelev, unique(pp$stapelev[order(pp$stap_id, elev)]))

  # Summary per stapelev, rounded as displayed in the histogram
  out <- do.call(
    rbind,
    lapply(split(pp, pp$stapelev), \(g) {
      e <- g$error[!is.na(g$error)]
      data.frame(
        stapelev = as.character(g$stapelev[1]),
        stap_id = g$stap_id[1],
        n = length(e),
        n_discard = sum(g$label == "discard"),
        error_sd = round(stats::sd(e), 2),
        error_offset = round(mean(g$surface_pressure - g$surface_pressure_norm), 2),
        sd = g$sd[1],
        n_outlier = sum(abs(e) >= warning_std_thr * g$sd[1]),
        error_max = if (length(e)) max(abs(e)) else NA_real_
      )
    })
  )
  out$sd_exceeded <- out$error_sd > out$sd
  rownames(out) <- NULL
  out[c(
    "stapelev",
    "stap_id",
    "n",
    "n_discard",
    "error_sd",
    "error_offset",
    "sd",
    "sd_exceeded",
    "n_outlier",
    "error_max"
  )]
}

#' Per-point pressure error with its stapelev and the sd of its stap
#'
#' Flights are grouped as stap 0 and use the sd of stap 1. A single `sd` applies to every stap.
#' @noRd
pressurepath_error <- function(pressurepath, sd) {
  if (is.null(sd)) {
    sd <- 1
  }
  ground <- pressurepath$stap_id == round(pressurepath$stap_id)
  pressurepath$stapelev <- paste(
    ifelse(ground, pressurepath$stap_id, 0),
    label2elev(pressurepath$label),
    sep = "|"
  )
  pressurepath$error <- pressurepath$pressure_tag - pressurepath$surface_pressure_norm
  pressurepath$error[pressurepath$label == "discard"] <- NA
  pressurepath$sd <- if (length(sd) == 1) sd else sd[ifelse(ground, pressurepath$stap_id, 1)]
  pressurepath
}
