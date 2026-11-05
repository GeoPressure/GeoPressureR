#' Compute duration of stationary periods
#'
#' @description
#' This function returns the duration between `stap$start` and `stap$end`.
#'
#' The function can be used with any data.frame containing `start` and `end` as POSIXct (e.g.,
#' `flight` or `path`).
#'
#' @param stap A `stap` data.frame.
#' @param stap_path `r lifecycle::badge("deprecated")` Renamed to `stap`.
#' @param units character string. Units in which the results are desired. Can be abbreviated.
#' See `difftime()`
#' @param return_numeric logical to return the duration as a numeric rather than with a duration
#' format.
#'
#' @return Vector of duration
#'
#' @examples
#' # Create fake stap
#' stap <- data.frame(
#'   start = seq(as.POSIXct("1990-01-01"), as.POSIXct("1991-01-01"), length.out = 13)
#' )
#' stap$end <- stap$start + 60 * 60 * 24 * 30
#' stap$stap_id <- seq_len(nrow(stap))
#'
#' stap2duration(stap)
#'
#' stap2duration(stap, units = "mins", return_numeric = FALSE)
#'
#' @export
stap2duration <- function(
  stap,
  units = "days",
  return_numeric = TRUE,
  stap_path = lifecycle::deprecated()
) {
  if (lifecycle::is_present(stap_path)) {
    lifecycle::deprecate_warn("3.8.0", "stap2duration(stap_path)", "stap2duration(stap)")
    stap <- stap_path
  }
  assertthat::assert_that(is.data.frame(stap))
  assertthat::assert_that(assertthat::has_name(stap, c("start", "end")))
  dur <- difftime(stap$end, stap$start, units = units)
  if (return_numeric) {
    dur <- as.numeric(dur)
  }
  return(dur)
}
