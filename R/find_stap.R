#' Find the stationary period corresponding to a date
#'
#' When `stap` contains a `stap_id` column, values are returned on that scale. Otherwise,
#' `find_stap()` returns row-based indices `1:nrow(stap)`. For dates in flight gaps between two
#' consecutive stationary periods, the returned value is linearly interpolated between the two
#' neighbouring stationary periods, yielding decimal `stap_id`.
#'
#' @param stap a data.frame with columns `start` and `end` defining stationary periods, and
#' optionally `stap_id`.
#' @param date a POSIXct vector of datetimes to map to stationary periods.
#' @return Numeric vector of `stap_id` values (fractional values for in-flight gaps).
#' @keywords internal
#' @export
find_stap <- function(stap, date) {
  start_num <- as.numeric(stap$start)
  end_num <- as.numeric(stap$end)
  date_num <- as.numeric(date)
  stap_ref <- if ("stap_id" %in% names(stap)) as.numeric(stap$stap_id) else seq_len(nrow(stap))

  # Assign each date to the last stap start that is <= date.
  idx <- findInterval(date_num, start_num, rightmost.closed = TRUE)
  idx[idx < 1] <- 1
  idx[idx > nrow(stap)] <- nrow(stap)

  # Default: within a stap interval, stap_id is the corresponding stap reference.
  stap_id <- stap_ref[idx]
  # For dates in the flight gap (after end[i] and before start[i+1]),
  # interpolate linearly to get fractional stap_id between consecutive periods.
  in_gap <- which(date_num > end_num[idx] & idx < nrow(stap))
  if (length(in_gap) > 0) {
    gap_idx <- idx[in_gap]
    gap_len <- start_num[gap_idx + 1] - end_num[gap_idx]
    stap_id[in_gap] <- stap_ref[gap_idx] +
      (date_num[in_gap] - end_num[gap_idx]) / gap_len * (stap_ref[gap_idx + 1] - stap_ref[gap_idx])
  }

  # Check that all dates have a stap_id
  assertthat::assert_that(!anyNA(stap_id))

  stap_id
}
