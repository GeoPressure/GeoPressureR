#' Create stationary periods from twilight midpoints
#'
#' @description
#' Construct stationary periods from labelled sunrise and sunset times. Each boundary is the
#' midpoint of an assumed movement period: night midpoints when movement occurs at night, or day
#' midpoints when movement occurs during the day. The resulting `stap` intervals therefore cover
#' the periods between successive movements. If a `stap_long` is provided (as a data.frame or a CSV
#' path), its intervals are kept as-is and the remaining gaps are filled using twilight midpoints.
#'
#' @param tag a GeoPressureR `tag` object with `twilight`.
#' @param stap_long optional long stationary periods, supplied as a `stap` data.frame with columns
#' `start` and `end`, a CSV path with those
#' columns (POSIXct-compatible strings), or a tag `id` resolved by `read_stap()`.
#' @param stap0 `r lifecycle::badge("deprecated")` Renamed to `stap_long`.
#' @param movement_period When the bird is assumed to be moving: `"night"`
#' (default) assumes the bird is moving during the night, so sunrise-to-sunset twilights are
#' combined into the same stationary period, while `"day"` assumes the bird is moving during the
#' day, so sunset-to-sunrise twilights are combined into the same stationary period.
#' @param twl_grouping `r lifecycle::badge("deprecated")` Renamed to `movement_period`.
#' @param max_twl_gap maximum allowed gap between consecutive twilights (in hours) before
#' erroring, indicating missing twilights.
#' @param quiet logical to hide messages.
#'
#' @return Updated `tag` with a twilight-based `stap` and optional `stap0`. The `stap` includes a
#' logical column `stap0` set to `TRUE` when intervals come from the provided `stap0` input.
#'
#' @examples
#' # Create and label the sunrise and sunset times used to define periods.
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read()
#'
#'   # Assume the bird moves at night. Preserve the long stationary periods
#'   # stored in data/stap-label/18LX.csv, while filling the remaining time
#'   # with daily periods bounded by night midpoints.
#'   tag <- tag_stap_daily(
#'     tag,
#'     stap_long = tag$param$id,
#'     movement_period = "night"
#'   )
#' })
#'
#' head(tag$stap)
#' @family tag
#' @export
tag_stap_daily <- function(
  tag,
  stap_long = NULL,
  movement_period = "night",
  max_twl_gap = 23.5,
  quiet = FALSE,
  twl_grouping = lifecycle::deprecated(),
  stap0 = lifecycle::deprecated()
) {
  if (lifecycle::is_present(stap0)) {
    lifecycle::deprecate_warn(
      "3.6.0",
      "tag_stap_daily(stap0)",
      "tag_stap_daily(stap_long)"
    )
    stap_long <- stap0
  }

  if (lifecycle::is_present(twl_grouping)) {
    lifecycle::deprecate_warn(
      "3.6.0",
      "tag_stap_daily(twl_grouping)",
      "tag_stap_daily(movement_period)"
    )
    movement_period <- twl_grouping
  }

  # Check tag and twilight data
  tag_assert(tag, "twilight")
  if (tag_assert(tag, "setmap", "logical")) {
    cli::cli_abort(c(
      "x" = "{.fun tag_set_map} has already been run on this {.var tag}.",
      ">" = "It is best practice to start from your raw data again using {.fun tag_create}."
    ))
  }
  if (tag_assert(tag, "stap", "logical") && !quiet) {
    cli::cli_warn(
      "The {.var tag} object already has a {.field stap} defined which will be overwriten."
    )
  }
  twl <- tag$twilight
  twl <- twl[!is.na(twl$twilight), ]
  twl <- twl[order(twl$twilight), ]
  n_twl <- nrow(twl)
  if (n_twl < 4) {
    cli::cli_abort("Not enough twilight data to build midpoint-based stationary periods.")
  }
  rise <- twl$rise
  if (anyNA(rise)) {
    cli::cli_abort("{.field rise} must not contain NA values.")
  }
  if (any(rise[-n_twl] == rise[-1])) {
    cli::cli_abort("{.field rise} must alternate TRUE/FALSE with no repeats.")
  }

  # Check for missing twilights
  if (!is.numeric(max_twl_gap) || length(max_twl_gap) != 1) {
    cli::cli_abort("{.var max_twl_gap} must be a single numeric value.")
  }
  twl_gap <- as.numeric(diff(twl$twilight), units = "secs")
  if (length(twl_gap) > 0 && any(twl_gap > max_twl_gap * 60 * 60)) {
    max_gap <- max(twl_gap)
    cli::cli_abort(c(
      "x" = "Missing twilight suspected: found gaps larger than {max_twl_gap}h.",
      "i" = "Largest gap: {format_minutes(max_gap / 60)}.",
      "i" = "Check for missing twilights or increase {.var max_twl_gap}."
    ))
  }

  movement_period <- match.arg(
    movement_period,
    choices = c("night", "day")
  )

  # Find index of first twilights per pairs
  pair_idx <- which(rise[-n_twl] == (movement_period == "day"))

  # Calculate midpoints between twilight pairs
  boundaries <- twl$twilight[pair_idx] +
    (twl$twilight[pair_idx + 1] - twl$twilight[pair_idx]) / 2
  if (length(boundaries) < 2 && is.null(stap_long)) {
    cli::cli_abort("Not enough {movement_period} twilight pairs to build stationary periods.")
  }

  # Integrate long stationary periods if provided
  stap_long_df <- NULL
  if (!is.null(stap_long)) {
    if (is.list(stap_long) && !is.data.frame(stap_long)) {
      stap_long$known_lon <- as.numeric(lapply(stap_long$known_lon, function(x) {
        if (is.null(x)) NA else x
      }))
      stap_long$known_lat <- as.numeric(lapply(stap_long$known_lat, function(x) {
        if (is.null(x)) NA else x
      }))
      stap_long <- as.data.frame(stap_long)
    }

    stap_long_df <- read_stap(stap_long)
    keep <- rep(TRUE, length(boundaries))
    for (i in seq_len(nrow(stap_long_df))) {
      keep <- keep & !(boundaries > stap_long_df$start[i] & boundaries < stap_long_df$end[i])
    }
    boundaries <- sort(unique(c(boundaries[keep], stap_long_df$start, stap_long_df$end)))
  }

  boundaries <- sort(unique(boundaries))
  if (length(boundaries) < 2) {
    cli::cli_abort("Not enough twilights to build stationary periods.")
  }
  if (any(diff(boundaries) <= 0)) {
    cli::cli_abort("Twilight boundaries must be strictly increasing.")
  }

  # Build stap data.frame
  stap <- data.frame(
    stap_id = seq_len(length(boundaries) - 1),
    start = boundaries[-length(boundaries)],
    end = boundaries[-1],
    stap0 = FALSE
  )
  if (!is.null(stap_long_df)) {
    key <- paste(stap$start, stap$end)
    key_long <- paste(stap_long_df$start, stap_long_df$end)
    if (!all(key_long %in% key)) {
      cli::cli_abort("{.var stap_long} intervals do not align with twilight boundaries.")
    }
    stap$stap0 <- key %in% key_long
    known_cols <- intersect(c("known_lat", "known_lon"), names(stap_long_df))
    if (length(known_cols) > 0) {
      idx <- match(key_long, key)
      stap[known_cols] <- NA_real_
      stap[idx, known_cols] <- stap_long_df[known_cols]
    }
  }

  # Assign stap to tag
  tag$stap <- stap

  # Assign stap_id to twilight data
  tag$twilight$stap_id <- find_stap(tag$stap, tag$twilight$twilight)

  # Store parameters used
  tag$param$tag_stap_daily <- list(
    movement_period = movement_period,
    max_twl_gap = max_twl_gap
  )

  return(tag)
}
