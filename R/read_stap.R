#' Read or validate stap data
#'
#' Reads a CSV file defining coarse stationary periods (`stap0`) or validates a data.frame,
#' including date parsing, ordering, and overlap checks.
#'
#' @param x a GeoPressureR `tag` object, a `stap` data.frame, or a CSV path.
#' If a `tag` is provided, the default file path is
#' `./data/stap-label/{tag$param$id}.csv`.
#' @param required_cols character vector of required columns (default: `c("start", "end")`).
#'
#' @return A data.frame containing at least the required columns, sorted by `start`.
#' @examples
#' # From a data.frame
#' stap_df <- data.frame(
#'   start = as.POSIXct(c("2020-01-01", "2020-01-03"), tz = "UTC"),
#'   end = as.POSIXct(c("2020-01-02", "2020-01-04"), tz = "UTC")
#' )
#' read_stap(stap_df)
#'
#' # From a CSV path
#' # read_stap("./data/stap-label/18LX.csv")
#' @family tag
#' @export
read_stap <- function(x, required_cols = c("start", "end")) {
  assertthat::assert_that(is.character(required_cols), length(required_cols) > 0)

  stap_df <- NULL
  file_path <- NULL

  if (inherits(x, "tag")) {
    file_path <- glue::glue("./data/stap-label/{x$param$id}.csv")
  } else if (is.character(x)) {
    file_path <- x
  } else if (is.data.frame(x)) {
    stap_df <- x
  } else {
    cli::cli_abort("{.var stap} must be a data.frame, a CSV path, or a GeoPressureR tag.")
  }

  if (is.null(stap_df)) {
    if (!file.exists(file_path)) {
      cli::cli_abort("Stap file not found: {.file {file_path}}.")
    }
    stap_df <- utils::read.csv(file_path, stringsAsFactors = FALSE)
  }

  # Check required columns
  if (!all(required_cols %in% names(stap_df))) {
    cli::cli_abort("{.var stap} must contain columns: {.val {required_cols}}.")
  }

  # Parse start and end as POSIXct
  stap_df$start <- as.POSIXct(stap_df$start, tz = "UTC")
  stap_df$end <- as.POSIXct(stap_df$end, tz = "UTC")
  if (anyNA(stap_df$start) || anyNA(stap_df$end)) {
    cli::cli_abort("{.var stap} `start` or `end` could not be parsed as POSIXct.")
  }
  if (any(stap_df$end <= stap_df$start)) {
    cli::cli_abort("{.var stap} must have {.field end} after {.field start}.")
  }

  # Order by start
  stap_df <- stap_df[order(stap_df$start), , drop = FALSE]

  # Check for overlapping intervals
  if (nrow(stap_df) > 1) {
    overlap <- stap_df$start[-1] < stap_df$end[-nrow(stap_df)]
    if (any(overlap)) {
      cli::cli_abort("{.var stap} intervals overlap.")
    }
  }

  stap_df
}
