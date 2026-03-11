#' Create stationary periods from a tag label
#'
#' @description
#' This function computes the stationary periods from the pressure and/or acceleration label data.
#' In most case, this function should be run directly after `tag_label_read()` in order to update
#' `tag` to correspond to the new label file.
#'
#' A stationary period is a period during which the bird is considered static relative to the
#' spatial resolution of interest (~10-50km). They are defined by being separated by a flight of any
#' duration (label `"flight"`). The `stap_id` is an integer value for stationary periods and decimal
#' value for flight. The `stap_id` is added as a new column to each sensor data.
#'
#' If an acceleration data.frame is present and contains a column `label`, the stationary period
#' will be computed from it, otherwise, it uses the pressure data.frame.
#'
#'
#' @inheritParams tag_label
#' @param quiet logical to display warning message.
#' @return `tag` is return with (1) a new data.frame of stationary periods `tag$stap` and (2) a new
#'  column `stap_id` for each sensor data.
#' @examples
#' withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
#'   tag <- tag_create("18LX", quiet = TRUE) |>
#'     tag_label_read()
#'
#'   tag <- tag_label_stap(tag)
#'
#'   str(tag)
#'
#'   str(tag$stap)
#' })
#' @family tag_label
#' @seealso [GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/tag-object.html#compute-stationary-periods)
#' @export
tag_label_stap <- function(
  tag,
  quiet = FALSE,
  warning_flight_duration = lifecycle::deprecated(),
  warning_stap_duration = lifecycle::deprecated()
) {
  if (lifecycle::is_present(warning_flight_duration)) {
    lifecycle::deprecate_warn(
      "3.5.0",
      "tag_label_stap(warning_flight_duration)"
    )
  }
  if (lifecycle::is_present(warning_stap_duration)) {
    lifecycle::deprecate_warn(
      "3.5.0",
      "tag_label_stap(warning_stap_duration)"
    )
  }
  if (tag_assert(tag, "setmap", "logical")) {
    cli::cli_abort(c(
      "x" = "{.fun tag_set_map} has already been run on this {.var tag}.",
      ">" = "It is best practice to start from your raw data again using {.fun tag_create}."
    ))
  }

  # Perform test
  tag_assert(tag, "label")

  # Build activity from acceleration when available
  if (
    assertthat::has_name(tag, "acceleration") &&
      assertthat::has_name(tag$acceleration, "label") &&
      nrow(tag$acceleration) > 0
  ) {
    sensor <- tag$acceleration[, c("date", "label")]

    # Append pressure label when acceleration ends
    if (assertthat::has_name(tag, "pressure")) {
      # Only use pressure data fire the first or after the last acceleration timestamp
      out <- tag$pressure$date < min(sensor$date, na.rm = TRUE) |
        tag$pressure$date > max(sensor$date, na.rm = TRUE)
      pres_outside_acc <- tag$pressure[out, ]

      if (nrow(pres_outside_acc) > 0) {
        # combine both data
        sensor <- rbind(sensor, pres_outside_acc[, c("date", "label")])
        # order by date
        sensor <- sensor[order(sensor$date), ]
      }
      if ("flight" %in% tag$pressure$label[!out]) {
        if (!quiet) {
          cli::cli_warn(c(
            "!" = "The label {.val flight} is detected on {.field pressure} while
        {.field acceleration} is also present.",
            "i" = "The stationary periods will be estimated from {.field acceleration} data and the
        {.val flight} label from pressure will be ignored. ",
            ">" = "It is best practise to remove {.val flight} label in {.field pressure} data when
        {.field acceleration} is available."
          ))
        }
      }
    }
  } else {
    sensor <- tag$pressure[, c("date", "label")]
  }

  # Create a table of activities (migration or stationary)
  tmp <- c(1, cumsum(diff(as.numeric(sensor$label == "flight")) == 1) + 1)
  tmp[sensor$label == "flight"] <- NA

  # As we label "in flight" pressure/acceleration, the taking-off and landing happened before and
  # after the labelling respectively. To account for this. We estimate that the bird took off
  # between the previous and first flight label, and landed between the last flight label and next
  # one. We use the temporal resolution to account for this.
  sensor$dt_prev <- c(as.difftime(0, units = "mins"), diff(sensor$date)) / 2
  sensor$dt_next <- c(diff(sensor$date), as.difftime(0, units = "mins")) / 2

  # construct stationary period table
  tag$stap <- data.frame(
    stap_id = unique(tmp[!is.na(tmp)]),
    start = do.call(c, lapply(split(sensor$date, tmp), min)) -
      do.call(c, lapply(split(sensor$dt_prev, tmp), function(x) x[1])),
    end = do.call("c", lapply(split(sensor$date, tmp), max)) +
      do.call(c, lapply(split(sensor$dt_next, tmp), function(x) x[1]))
  )

  # Assign to each sensor the stationary period to which it belong to.
  for (sensor_df in c(
    "pressure",
    "acceleration",
    "light",
    "twilight",
    "magnetic"
  )) {
    if (assertthat::has_name(tag, sensor_df)) {
      assertthat::assert_that(is.data.frame(tag[[sensor_df]]))
      if ("date" %in% names(tag[[sensor_df]])) {
        date <- tag[[sensor_df]]$date
      } else if ("twilight" %in% names(tag[[sensor_df]])) {
        date <- tag[[sensor_df]]$twilight
      } else {
        cli::cli_abort(c(
          "{.field {sensor_df}} needs to have a column {.field date} or {.field twilight}."
        ))
      }
      tag[[sensor_df]]$stap_id <- find_stap(tag$stap, date)
    }
  }

  return(tag)
}

#' Find the stationary period corresponding to a date
#'
#' @param stap a data.frame with columns `start` and `end` defining stationary periods.
#' @param date a POSIXct vector of datetimes to map to stationary periods.
#' @return Numeric vector of `stap_id` indices (fractional values for in-flight gaps).
#' @keywords internal
#' @export
find_stap <- function(stap, date) {
  start_num <- as.numeric(stap$start)
  end_num <- as.numeric(stap$end)
  date_num <- as.numeric(date)

  # Assign each date to the last stap start that is <= date.
  idx <- findInterval(date_num, start_num, rightmost.closed = TRUE)
  idx[idx < 1] <- 1
  idx[idx > nrow(stap)] <- nrow(stap)

  # Default: within a stap interval, stap_id is the integer index.
  stap_id <- idx
  # For dates in the flight gap (after end[i] and before start[i+1]),
  # interpolate linearly to get fractional stap_id between i and i+1.
  in_gap <- date_num > end_num[idx] & idx < nrow(stap)
  if (any(in_gap)) {
    gap_len <- start_num[idx[in_gap] + 1] - end_num[idx[in_gap]]
    stap_id[in_gap] <- idx[in_gap] +
      (date_num[in_gap] - end_num[idx[in_gap]]) / gap_len
  }

  # Check that all date have a stap_id
  assertthat::assert_that(!anyNA(stap_id))

  stap_id
}
