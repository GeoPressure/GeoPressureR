#' Create stationary periods from twilight midpoints
#'
#' @description
#' Build a `stap` table using midpoints between consecutive twilight pairs so the period lengths
#' follow the twilight drift. If a `stap0` is provided (as a data.frame or a CSV path), its
#' intervals are kept as-is and the remaining gaps are filled using twilight midpoints.
#'
#' @param tag a GeoPressureR `tag` object with `twilight`.
#' @param stap0 optional `stap` data.frame with columns `start` and `end`, a CSV path with those
#' columns (POSIXct-compatible strings), or a tag `id` resolved by `read_stap()`.
#' @param twl_grouping Which period is assumed to be when the bird is moving: `"night"`
#' (default) assumes the bird is moving during the night, so sunrise-to-sunset twilights are
#' combined into the same stationary period, while `"day"` assumes the bird is moving during the
#' day, so sunset-to-sunrise twilights are combined into the same stationary period.
#' @param max_twl_gap_hours maximum allowed gap between consecutive twilights (in hours) before
#' erroring, indicating missing twilights.
#' @param quiet logical to hide messages.
#'
#' @return Updated `tag` with a twilight-based `stap` and optional `stap0`. The `stap` includes a
#' logical column `stap0` set to `TRUE` when intervals come from the provided `stap0` input.
#'
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label(quiet = TRUE) |>
#'     twilight_create() |>
#'     twilight_label_read()
#' })
#' tag <- tag_stap_daily(tag, twl_grouping = "night")
#' @family tag
#' @export
tag_stap_daily <- function(
  tag,
  stap0 = NULL,
  twl_grouping = "night",
  max_twl_gap_hours = 23.5,
  quiet = FALSE
) {
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
  if (!is.numeric(max_twl_gap_hours) || length(max_twl_gap_hours) != 1) {
    cli::cli_abort("{.var max_twl_gap_hours} must be a single numeric value.")
  }
  twl_gap <- as.numeric(diff(twl$twilight), units = "secs")
  if (length(twl_gap) > 0 && any(twl_gap > max_twl_gap_hours * 60 * 60)) {
    max_gap <- max(twl_gap)
    cli::cli_abort(c(
      "x" = "Missing twilight suspected: found gaps larger than {max_twl_gap_hours}h.",
      "i" = "Largest gap: {format_minutes(max_gap / 60)}.",
      "i" = "Check for missing twilights or increase {.var max_twl_gap_hours}."
    ))
  }

  twl_grouping <- match.arg(
    twl_grouping,
    choices = c("night", "day")
  )

  # Find index of first twilights per pairs
  pair_idx <- which(rise[-n_twl] == (twl_grouping == "day"))

  # Calculate midpoints between twilight pairs
  boundaries <- twl$twilight[pair_idx] +
    (twl$twilight[pair_idx + 1] - twl$twilight[pair_idx]) / 2
  if (length(boundaries) < 2 && is.null(stap0)) {
    cli::cli_abort("Not enough {twl_grouping} twilight pairs to build stationary periods.")
  }

  # Integrate stap0 if provided
  stap0_df <- NULL
  if (!is.null(stap0)) {
    if (is.list(stap0) && !is.data.frame(stap0)) {
      stap0$known_lon <- as.numeric(lapply(stap0$known_lon, function(x) {
        if (is.null(x)) NA else x
      }))
      stap0$known_lat <- as.numeric(lapply(stap0$known_lat, function(x) {
        if (is.null(x)) NA else x
      }))
      stap0 <- as.data.frame(stap0)
    }

    stap0_df <- read_stap(stap0)
    keep <- rep(TRUE, length(boundaries))
    for (i in seq_len(nrow(stap0_df))) {
      keep <- keep & !(boundaries > stap0_df$start[i] & boundaries < stap0_df$end[i])
    }
    boundaries <- boundaries[keep]
    boundaries <- sort(unique(c(boundaries, stap0_df$start, stap0_df$end)))
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
  if (!is.null(stap0_df)) {
    key <- paste(stap$start, stap$end)
    key0 <- paste(stap0_df$start, stap0_df$end)
    if (!all(key0 %in% key)) {
      cli::cli_abort("{.var stap0} intervals do not align with twilight boundaries.")
    }
    stap$stap0 <- key %in% key0
    known_cols <- intersect(c("known_lat", "known_lon"), names(stap0_df))
    if (length(known_cols) > 0) {
      idx <- match(key0, key)
      stap[known_cols] <- NA_real_
      stap[idx, known_cols] <- stap0_df[known_cols]
    }
  }

  # Assign stap to tag
  tag$stap <- stap

  # Assign stap_id to twilight data
  tag$twilight$stap_id <- find_stap(tag$stap, tag$twilight$twilight)

  # Store parameters used
  tag$param$tag_stap_daily <- list(
    twl_grouping = twl_grouping,
    max_twl_gap_hours = max_twl_gap_hours
  )

  return(tag)
}
