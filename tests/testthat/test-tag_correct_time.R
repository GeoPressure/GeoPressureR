library(testthat)
library(GeoPressureR)

test_that("clock correction preserves sensor data and applies one shared reference", {
  reference <- as.POSIXct("2020-01-01", tz = "UTC")
  sensor <- data.frame(date = reference + c(-1, 0, 1) * 365.25 * 86400, value = 1000)
  attr(sensor, "source") <- "raw"
  attr(sensor$date, "clock") <- "RTC"
  magnetic <- sensor
  magnetic$value <- NULL
  for (column in c(
    "magnetic_x",
    "magnetic_y",
    "magnetic_z",
    "acceleration_x",
    "acceleration_y",
    "acceleration_z"
  )) {
    magnetic[[column]] <- seq_len(3)
  }
  tag <- tag_create(
    "clock",
    manufacturer = "tabular",
    pressure_file = sensor,
    light_file = sensor,
    acceleration_file = sensor,
    temperature_external_file = sensor,
    temperature_internal_file = sensor,
    magnetic_file = magnetic,
    quiet = TRUE
  )
  corrected <- tag_correct_time(tag, time_drift = 1, reference = reference, time_shift = 2)
  for (name in setdiff(names(tag), "param")) {
    expect_equal(
      as.numeric(corrected[[name]]$date) - as.numeric(tag[[name]]$date),
      c(3600, 7200, 10800)
    )
    expect_identical(attributes(corrected[[name]]), attributes(tag[[name]]))
    expect_identical(attributes(corrected[[name]]$date), attributes(tag[[name]]$date))
    expect_identical(
      corrected[[name]][setdiff(names(tag[[name]]), "date")],
      tag[[name]][setdiff(names(tag[[name]]), "date")]
    )
  }
  expect_identical(tag$pressure, sensor)
  expect_identical(corrected$param$tag_create, tag$param$tag_create)
  expect_equal(corrected$param$tag_correct_time$reference, reference)
  expect_equal(corrected$param$tag_correct_time$time_unit, "year")
})

test_that("sensor selection and different recording durations do not change the drift rate", {
  reference <- as.POSIXct("2020-01-01", tz = "UTC")
  sensor <- data.frame(date = reference + (0:2) * 30 * 86400, value = 1000)
  tag <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor[2:3, ],
    acceleration_file = sensor[1:2, ],
    quiet = TRUE
  )
  corrected <- tag_correct_time(
    tag,
    time_drift = -1 / 60,
    reference = reference,
    time_unit = "month",
    sensors = c("pressure", "light", "magnetic")
  )
  expect_equal(
    as.numeric(corrected$pressure$date - tag$pressure$date, units = "secs"),
    c(0, -60, -120)
  )
  expect_equal(corrected$light$date, corrected$pressure$date[2:3])
  expect_identical(corrected$acceleration, tag$acceleration)
  expect_equal(corrected$param$tag_correct_time$sensors, c("pressure", "light"))
})

test_that("constant correction follows tag_create time_shift and supports light-only tags", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:2) * 300, value = 1)
  tag <- tag_create(
    "clock",
    manufacturer = "tabular",
    light_file = sensor,
    assert_pressure = FALSE,
    quiet = TRUE
  )
  corrected <- tag_correct_time(tag, time_drift = 0, reference = sensor$date[1], time_shift = 2)
  shifted <- tag_create(
    "clock",
    manufacturer = "tabular",
    light_file = sensor,
    assert_pressure = FALSE,
    time_shift = 2,
    quiet = TRUE
  )
  expect_identical(corrected$light, shifted$light)
  expect_identical(tag_correct_time(tag, 0, sensor$date[1])$light, tag$light)
})

test_that("corrected CSVs retain fractional seconds and can enter the normal workflow", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:48) * 1800, value = 1000)
  tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE)
  corrected <- tag_correct_time(tag, time_drift = 12 / 60, reference = sensor$date[1])
  csv <- corrected$pressure
  names(csv)[1] <- "datetime"
  csv$datetime <- format(csv$datetime, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")
  directory <- tempfile("corrected-raw-tag-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  utils::write.csv(csv, file.path(directory, "pressure.csv"), row.names = FALSE)
  reread <- tag_create("clock", directory = directory, quiet = TRUE)
  expect_lt(
    max(abs(as.numeric(reread$pressure$date) - as.numeric(corrected$pressure$date))),
    1.1e-6
  )
  expect_equal(reread$pressure$value, sensor$value)
  expect_equal(reread$param$tag_create$time_shift, 0)
})

test_that("clock correction rejects stale results and rates that reverse time", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:2) * 1800, value = 1000)
  tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE)
  tag$pressure$label <- "stationary"
  expect_error(tag_correct_time(tag, 1, sensor$date[1]), "unlabelled")
  tag$pressure$label <- NULL
  tag$twilight <- data.frame(twilight = sensor$date[1], rise = TRUE)
  expect_error(tag_correct_time(tag, 1, sensor$date[1]), "unlabelled")
  tag$twilight <- NULL
  expect_error(tag_correct_time(tag, -30 * 24, sensor$date[1], time_unit = "month"))
  expect_error(tag_correct_time(tag, c(1, 2), sensor$date[1]))
})

test_that("corrected light and pressure use full-day and whole-hour grids, including plots", {
  reference <- as.POSIXct("2020-01-01", tz = "UTC")
  light <- data.frame(
    date = reference + (0:863) * 300,
    value = rep(c(rep(0, 72), rep(1, 144), rep(0, 72)), 3)
  )
  pressure <- data.frame(date = reference + (0:143) * 1800, value = 1000 + sin((0:143) / 12))
  tag <- tag_create("clock", pressure_file = pressure, light_file = light, quiet = TRUE)
  baseline <- ts2mat(tag$light)
  baseline_twl <- twilight_create(tag, twl_offset = 0)
  tag$pressure$label <- "stationary"
  tag$pressure$stap_id <- 1
  tag$stap <- data.frame(stap_id = 1, known_lat = NA_real_)
  baseline_pressure <- geopressure_map_preprocess(tag)
  for (rate in c(-12 / 60, 12 / 60)) {
    raw <- tag
    raw$pressure$label <- raw$pressure$stap_id <- raw$stap <- NULL
    corrected <- tag_correct_time(raw, rate, reference)
    mat <- suppressWarnings(ts2mat(corrected$light))
    expect_identical(dim(mat$value), dim(baseline$value))
    expect_equal(mat$res, 300)
    expect_equal(diff(mat$date[1, ]), rep(86400, 2))
    twilight <- suppressWarnings(twilight_create(corrected, twl_offset = 0))
    expect_equal(nrow(twilight$twilight), nrow(baseline_twl$twilight))
    expect_lt(
      max(abs(as.numeric(twilight$twilight$twilight) - as.numeric(baseline_twl$twilight$twilight))),
      300
    )
    expect_no_error(suppressWarnings(ggplot2::ggplot_build(plot_tag_twilight(
      corrected,
      twl_offset = 0,
      plot_plotly = FALSE
    ))))
    corrected$pressure$label <- "stationary"
    corrected$pressure$stap_id <- 1
    corrected$stap <- tag$stap
    processed <- suppressWarnings(geopressure_map_preprocess(corrected))
    expect_true(all(as.numeric(processed$date) %% 3600 == 0))
    expect_true(all(diff(as.numeric(processed$date)) == 3600))
    expect_identical(names(processed), names(baseline_pressure))
    expect_no_error(suppressWarnings(ggplot2::ggplot_build(plot_tag_pressure(
      corrected,
      plot_plotly = FALSE
    ))))
  }
})
