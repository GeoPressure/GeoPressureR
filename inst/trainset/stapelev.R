# Deterministic elevation proposals, independent of the Shiny workspace.

stapelev_best_offset <- function(x, range) {
  x <- sort(x[is.finite(x)])
  if (!length(x)) {
    return(NA_real_)
  }
  end <- findInterval(x + 2 * range, x)
  start <- which.max(end - seq_along(x) + 1L)
  round(mean(x[c(start, end[start])]), 1)
}

# Solar elevation is in degrees; points exactly at the threshold belong to day.
stapelev_daylight_keep <- function(solar_elevation, discard = "none", solar_angle = 0) {
  switch(
    discard,
    none = rep(TRUE, length(solar_elevation)),
    day = solar_elevation < solar_angle,
    night = solar_elevation >= solar_angle
  )
}

stapelev_assign_offsets <- function(
  proposal,
  offsets,
  windows,
  range,
  continuity,
  eligible = rep(TRUE, nrow(proposal)),
  gap = NULL
) {
  proposal$offset <- NA_real_
  proposal$error <- NA_real_
  proposal$label[proposal$label != "flight"] <- "discard"
  if (!length(offsets)) {
    return(proposal)
  }

  # Stable identity resolves ties independently of checkbox or row order.
  if (!is.null(names(offsets))) {
    order <- order(names(offsets))
    offsets <- offsets[order]
    windows <- windows[order, , drop = FALSE]
    if (length(range) > 1L) {
      range <- range[order]
    }
    if (length(continuity) > 1L) {
      continuity <- continuity[order]
    }
    if (length(gap) > 1L) {
      gap <- gap[order]
    }
    if (is.matrix(eligible)) eligible <- eligible[, order, drop = FALSE]
  }
  range <- rep_len(range, length(offsets))
  eligible <- matrix(eligible, nrow = nrow(proposal), ncol = length(offsets))
  date_num <- as.numeric(proposal$date)
  in_window <- outer(date_num, windows[, 1], ">=") & outer(date_num, windows[, 2], "<=")
  in_window[,
    !is.finite(windows[, 1]) | !is.finite(windows[, 2]) | windows[, 1] >= windows[, 2]
  ] <- FALSE
  deviation <- outer(proposal$residual, offsets, "-")
  distance <- abs(deviation)
  distance[!in_window | !eligible | sweep(distance, 2, range, ">") | !is.finite(distance)] <- Inf
  nearest <- max.col(-distance, ties.method = "first")
  group <- integer(nrow(proposal))
  valid <- proposal$label != "flight" &
    is.finite(distance[cbind(seq_len(nrow(proposal)), nearest)])
  group[valid] <- nearest[valid]

  continuity <- rep_len(continuity, length(offsets))
  if (any(continuity > 0) && any(group > 0L)) {
    date_diff <- diff(sort(unique(date_num)))
    dt <- if (length(date_diff)) stats::median(date_diff) else 0
    keep <- logical(nrow(proposal))
    gap_limit <- if (is.null(gap)) rep(dt, length(offsets)) else rep_len(gap, length(offsets)) * 60
    for (g in seq_along(offsets)) {
      if (continuity[g] <= 0) {
        keep[group == g] <- TRUE
        next
      }
      excluded_run <- cumsum(
        !eligible[, g] | proposal$label == "flight" | (group > 0L & group != g)
      )
      i <- which(group == g)
      if (!length(i)) {
        next
      }
      # Excluded solar periods are hard boundaries, even for a single sample.
      start <- c(1L, which(diff(date_num[i]) > dt + gap_limit[g] | diff(excluded_run[i]) > 0L) + 1L)
      end <- c(start[-1L] - 1L, length(i))
      duration <- (date_num[i[end]] - date_num[i[start]] + dt) / 3600
      keep[i] <- rep(duration >= continuity[g], end - start + 1L)
    }
    group[!keep] <- 0L
  }

  level_labels <- names(offsets)
  if (is.null(level_labels)) {
    level_labels <- c("", paste0("elev_", seq_along(offsets)[-1L] - 1L))
  }
  proposal$label[group > 0L] <- level_labels[group[group > 0L]]
  proposal$offset[group > 0L] <- offsets[group[group > 0L]]
  proposal$error[group > 0L] <- deviation[cbind(which(group > 0L), group[group > 0L])]
  proposal
}

# Compress day/night into intervals with interpolated threshold crossings.
stapelev_solar_strip <- function(date, solar_elevation, solar_angle = 0) {
  time <- as.numeric(date)
  day <- solar_elevation >= solar_angle
  i <- which(diff(day) != 0L)
  crossing <- time[i] +
    (solar_angle - solar_elevation[i]) /
      (solar_elevation[i + 1L] - solar_elevation[i]) *
      (time[i + 1L] - time[i])
  edges <- unique(c(time[1], crossing, time[length(time)]))
  middle <- (head(edges, -1L) + tail(edges, -1L)) / 2
  day <- stats::approx(time, solar_elevation, xout = middle, rule = 2)$y >= solar_angle
  date <- as.POSIXct(edges, origin = "1970-01-01", tz = "UTC")
  list(
    x = date,
    z = matrix(as.integer(day), nrow = 1),
    text = matrix(
      glue::glue(
        "{ifelse(day, 'Day', 'Night')} · threshold {solar_angle}°<br>",
        "{format(head(date, -1L), '%d %b %H:%M', tz = 'UTC')}–",
        "{format(tail(date, -1L), '%d %b %H:%M', tz = 'UTC')} UTC"
      ),
      nrow = 1
    )
  )
}

stapelev_eligible <- function(proposal, solar_elevation, settings) {
  count <- length(settings$offsets)
  count <- max(1L, count)
  discard <- rep_len(settings$discard_solar, count)
  angle <- rep_len(settings$solar_angle, count)
  eligible <- vapply(
    seq_len(count),
    function(i) {
      keep <- stapelev_daylight_keep(solar_elevation, discard[i], angle[i])
      keep
    },
    logical(nrow(proposal))
  )
  if (count == 1L) {
    eligible <- eligible[, 1]
  }

  eligible
}

# Synchronous local refinement: fixed levels compete but never move.
# The same starting values, windows and selected identities give the same fit.
stapelev_fit_offsets <- function(proposal, settings, eligible, selected) {
  converged <- FALSE
  support <- integer(length(settings$offsets))
  for (iteration in seq_len(50)) {
    classified <- stapelev_assign_offsets(
      proposal,
      settings$offsets,
      settings$windows,
      settings$range,
      0,
      eligible,
      settings$gap
    )
    updated <- settings$offsets
    for (i in seq_along(updated)) {
      use <- classified$label == names(updated)[i]
      support[i] <- sum(use)
      if (names(updated)[i] %in% selected && support[i] >= 3L) {
        updated[i] <- round(stats::median(proposal$residual[use]), 1)
      }
    }
    if (identical(updated, settings$offsets)) {
      converged <- TRUE
      break
    }
    settings$offsets <- updated
  }
  list(offsets = settings$offsets, support = support, converged = converged, iterations = iteration)
}

# Explicit initialization: fixed count, deterministic sequential coverage search.
# Existing offsets are never read; windows are supplied by the user.
stapelev_initialize_offsets <- function(proposal, windows, range, eligible) {
  offsets <- numeric()
  eligible <- matrix(eligible, nrow = nrow(proposal), ncol = nrow(windows))
  time <- as.numeric(proposal$date)
  for (i in seq_len(nrow(windows))) {
    candidate <- eligible[, i] &
      proposal$label != "flight" &
      time >= windows[i, 1] &
      time <= windows[i, 2] &
      is.finite(proposal$residual)
    if (length(offsets)) {
      covered <- stapelev_assign_offsets(
        proposal,
        offsets,
        windows[seq_along(offsets), , drop = FALSE],
        range,
        0
      )
      candidate <- candidate & covered$label == "discard"
    }
    offset <- stapelev_best_offset(proposal$residual[candidate], range)
    if (!is.finite(offset)) {
      return(NULL)
    }
    offsets <- c(offsets, offset)
  }
  offsets
}
