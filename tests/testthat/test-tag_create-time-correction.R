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
  corrected <- tag_create(
    "clock",
    manufacturer = "tabular",
    pressure_file = sensor,
    light_file = sensor,
    acceleration_file = sensor,
    temperature_external_file = sensor,
    temperature_internal_file = sensor,
    magnetic_file = magnetic,
    time_drift = 1,
    time_shift = 2,
    time_reference = reference,
    quiet = TRUE
  )
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
  expect_equal(corrected$param$tag_create$time_reference, reference)
  expect_equal(corrected$param$tag_create$time_drift, 1)
  expect_equal(corrected$param$tag_create$time_shift, 2)
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
  corrected <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor[2:3, ],
    acceleration_file = sensor[1:2, ],
    time_drift = list(pressure = -1 / 60 * 365.25 / 30, light = -1 / 60 * 365.25 / 30),
    time_shift = list(pressure = 2, light = -1),
    quiet = TRUE
  )
  expect_equal(
    as.numeric(corrected$pressure$date - tag$pressure$date, units = "secs"),
    c(7200, 7140, 7080)
  )
  expect_equal(corrected$light$date + 3 * 3600, corrected$pressure$date[2:3])
  expect_identical(corrected$acceleration, tag$acceleration)
  expect_equal(corrected$param$tag_create$time_reference, reference)
  expect_equal(corrected$param$tag_create$time_shift, list(pressure = 2, light = -1))
})

test_that("each sensor can have its own shift and drift rate", {
  sensor <- data.frame(
    date = as.POSIXct("2020-01-01", tz = "UTC") + (0:2) * 365.25 * 86400,
    value = 1000
  )
  tag <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor,
    acceleration_file = sensor,
    time_shift = list(pressure = 2, light = -1),
    time_drift = list(pressure = 1, light = -0.5),
    quiet = TRUE
  )
  expect_equal(as.numeric(tag$pressure$date - sensor$date, units = "secs"), c(7200, 10800, 14400))
  expect_equal(as.numeric(tag$light$date - sensor$date, units = "secs"), c(-3600, -5400, -7200))
  expect_identical(tag$acceleration, sensor)
})

test_that("default reference uses the earliest measurement across all sensors", {
  reference <- as.POSIXct("2020-01-01", tz = "UTC")
  sensor <- data.frame(date = reference + (0:2) * 365.25 * 86400, value = 1000)
  tag <- tag_create("clock", pressure_file = sensor, light_file = sensor[2:3, ], quiet = TRUE)
  corrected <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor[2:3, ],
    time_drift = 1,
    quiet = TRUE
  )
  expect_equal(corrected$param$tag_create$time_reference, reference)
  expect_equal(corrected$pressure$date[1], tag$pressure$date[1])
  expect_equal(
    as.numeric(corrected$pressure$date - tag$pressure$date, units = "secs"),
    c(0, 3600, 7200)
  )
  expect_equal(corrected$light$date, corrected$pressure$date[2:3])
  selected <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor[2:3, ],
    time_drift = list(light = 1),
    quiet = TRUE
  )
  expect_identical(selected$pressure, tag$pressure)
  expect_equal(selected$light$date, corrected$light$date)
  expect_equal(selected$param$tag_create$time_reference, reference)
  explicit <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor[2:3, ],
    time_drift = 1,
    time_reference = sensor$date[2],
    quiet = TRUE
  )
  expect_equal(explicit$pressure$date[2], tag$pressure$date[2])
  expect_equal(
    as.numeric(explicit$pressure$date - tag$pressure$date, units = "secs"),
    c(-3600, 0, 3600)
  )
})

test_that("clock correction respects tag_create time_shift and supports light-only tags", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:2) * 300, value = 1)
  tag <- tag_create(
    "clock",
    manufacturer = "tabular",
    light_file = sensor,
    assert_pressure = FALSE,
    quiet = TRUE
  )
  shifted <- tag_create(
    "clock",
    manufacturer = "tabular",
    light_file = sensor,
    assert_pressure = FALSE,
    time_shift = 2,
    quiet = TRUE
  )
  corrected <- tag_create(
    "clock",
    manufacturer = "tabular",
    light_file = sensor,
    assert_pressure = FALSE,
    time_shift = 2,
    time_drift = 1,
    quiet = TRUE
  )
  expect_equal(corrected$light$date[1], shifted$light$date[1])
  expect_equal(corrected$param$tag_create$time_reference, sensor$date[1])
  expect_lt(
    abs(
      as.numeric(corrected$light$date[3] - shifted$light$date[3], units = "secs") -
        600 / (365.25 * 24)
    ),
    0.000501
  )
  expect_null(tag$param$tag_create$time_reference)
})

test_that("corrected CSVs retain fractional seconds and can enter the normal workflow", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:48) * 1800, value = 1000)
  tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE)
  corrected <- tag_create("clock", pressure_file = sensor, time_drift = 12 / 60, quiet = TRUE)
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

test_that("clock corrections apply before cropping and record the original reference", {
  reference <- as.POSIXct("2020-01-01", tz = "UTC")
  sensor <- data.frame(date = reference + (0:2) * 365.25 * 86400, value = 1000)
  corrected <- tag_create(
    "clock",
    pressure_file = sensor,
    time_shift = 2,
    time_drift = 1,
    crop_start = sensor$date[2] + 2.5 * 3600,
    quiet = TRUE
  )
  expect_equal(corrected$param$tag_create$time_reference, reference)
  expect_equal(corrected$pressure$date, sensor$date[2:3] + c(3, 4) * 3600)
  defaults <- param_create("clock", default = TRUE)$tag_create
  expect_equal(defaults$time_drift, 0)
  expect_null(defaults$time_reference)
})

test_that("fractional clock corrections do not mask genuine irregular sampling", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:48) * 1800, value = 1000)
  corrected <- expect_no_warning(tag_create("clock", pressure_file = sensor, time_drift = 12 / 60))
  expect_no_warning(ts2mat(corrected$pressure))
  corrected$pressure$label <- "stationary"
  corrected$pressure$stap_id <- 1
  corrected$stap <- data.frame(stap_id = 1, known_lat = NA_real_)
  expect_no_warning(geopressure_map_preprocess(corrected))
  sensor$date[25:49] <- sensor$date[25:49] + 1
  expect_warning(irregular <- tag_create("clock", pressure_file = sensor), "Irregular time spacing")
  expect_warning(ts2mat(irregular$pressure), "not constant")
  irregular$pressure$label <- "stationary"
  irregular$pressure$stap_id <- 1
  irregular$stap <- corrected$stap
  expect_warning(geopressure_map_preprocess(irregular), "not on a regular interval")
})

test_that("clock correction validates rate structure at tag_create entry", {
  sensor <- data.frame(date = as.POSIXct("2020-01-01", tz = "UTC") + (0:2) * 1800, value = 1000)
  expect_error(tag_create("clock", pressure_file = sensor, time_drift = -365.25 * 24))
  expect_error(tag_create("clock", pressure_file = sensor, time_drift = c(1, 2)))
  unused <- tag_create(
    "clock",
    pressure_file = sensor,
    time_drift = list(humidity = 1),
    quiet = TRUE
  )
  expect_identical(unused$pressure, sensor)
  expect_error(tag_create("clock", pressure_file = sensor, time_drift = list(light = c(1, 2))))
  expect_error(tag_create("clock", pressure_file = sensor, time_drift = Inf))
  expect_error(tag_create("clock", pressure_file = sensor, time_reference = NA_character_))
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
    corrected <- tag_create(
      "clock",
      pressure_file = pressure,
      light_file = light,
      time_drift = rate,
      quiet = TRUE
    )
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


test_that("new sensor tables receive clock correction and cropping automatically", {
  reference <- as.POSIXct("2020-01-01", tz = "UTC")
  sensor <- data.frame(date = reference + (1:2) * 365.25 * 86400, value = 1000)
  tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE)
  tag$humidity <- data.frame(date = reference + (0:1) * 365.25 * 86400, value = c(40, 50))
  corrected <- tag_create_time_correct(tag, time_shift = 2, time_drift = 1, time_reference = NULL)
  expect_equal(corrected$param$tag_create$time_reference, reference)
  expect_equal(corrected$humidity$date, tag$humidity$date + c(2, 3) * 3600)
  expect_equal(corrected$pressure$date, tag$pressure$date + c(3, 4) * 3600)
  expect_identical(corrected$humidity$value, tag$humidity$value)
  selected <- tag_create_time_correct(
    tag,
    time_shift = list(humidity = 2),
    time_drift = list(humidity = -0.5),
    time_reference = NULL
  )
  expect_equal(selected$humidity$date, tag$humidity$date + c(2, 1.5) * 3600)
  expect_identical(selected$pressure, tag$pressure)
  cropped <- tag_create_crop(
    corrected,
    crop_start = sensor$date[1] + 2.5 * 3600,
    crop_end = NULL,
    quiet = TRUE
  )
  expect_equal(nrow(cropped$humidity), 1)
  expect_equal(cropped$humidity$date, corrected$humidity$date[2])
  expect_equal(nrow(cropped$pressure), 2)
})

test_that("drift-corrected pressure and acceleration labels survive CSV round trips", {
  sensor <- data.frame(
    date = as.POSIXct("2025-01-01", tz = "UTC") + (0:48) * 1800,
    value = 1000
  )
  tag <- tag_create(
    "clock",
    pressure_file = sensor,
    acceleration_file = sensor,
    time_drift = list(pressure = -12 / 60, acceleration = 6 / 60),
    quiet = TRUE
  )
  tag$pressure$label <- rep(c("flight", "discard", "elev_100"), length.out = nrow(sensor))
  tag$acceleration$label <- rev(tag$pressure$label)
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  tag_label_write(tag, file, quiet = TRUE)
  csv <- utils::read.csv(file)
  expect_true(all(grepl("\\.[0-9]{3}Z$", csv$timestamp)))
  reloaded <- expect_no_warning(tag_label_read(tag, file))
  expect_identical(reloaded$pressure, tag$pressure)
  expect_identical(reloaded$acceleration, tag$acceleration)

  # Genuine timestamp mismatches must still be reported.
  csv$timestamp[1] <- "2024-12-31T23:59:59.000Z"
  utils::write.csv(csv, file, row.names = FALSE)
  expect_warning(tag_label_read(tag, file), "missing 1 timesteps")
})

test_that("label timestamps retain milliseconds across second boundaries", {
  sensor <- data.frame(
    date = as.POSIXct("2025-01-01", tz = "UTC") + c(0.0004, 0.9996, 2.1234),
    value = 1000
  )
  tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE)
  tag$pressure$label <- c("flight", "discard", "elev_100")
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  tag_label_write(tag, file, quiet = TRUE)
  expect_identical(
    utils::read.csv(file)$timestamp,
    c(
      "2025-01-01T00:00:00.000Z",
      "2025-01-01T00:00:01.000Z",
      "2025-01-01T00:00:02.123Z"
    )
  )
  reloaded <- expect_no_warning(tag_label_read(tag, file))
  expect_identical(reloaded$pressure, tag$pressure)

  # Existing whole-second label files remain readable.
  tag$pressure$date <- as.POSIXct("2025-01-01", tz = "UTC") + 0:2
  tag_label_write(tag, file, quiet = TRUE)
  csv <- utils::read.csv(file)
  csv$timestamp <- sub(".000Z", "Z", csv$timestamp, fixed = TRUE)
  utils::write.csv(csv, file, row.names = FALSE)
  reloaded <- expect_no_warning(tag_label_read(tag, file))
  expect_identical(reloaded$pressure, tag$pressure)
})

test_that("tag creation rounds every sensor to milliseconds before cropping", {
  sensor <- data.frame(
    date = as.POSIXct("2025-01-01", tz = "UTC") + c(0.0004, 0.9996, 2.1234),
    value = 1000
  )
  attr(sensor$date, "clock") <- "RTC"
  magnetic <- sensor
  for (column in c(
    "acceleration_x",
    "acceleration_y",
    "acceleration_z",
    "magnetic_x",
    "magnetic_y",
    "magnetic_z"
  )) {
    magnetic[[column]] <- sensor$value
  }
  tag <- tag_create(
    "clock",
    pressure_file = sensor,
    light_file = sensor,
    acceleration_file = sensor,
    temperature_external_file = sensor,
    temperature_internal_file = sensor,
    magnetic_file = magnetic,
    quiet = TRUE
  )
  for (name in setdiff(names(tag), "param")) {
    expect_identical(as.numeric(tag[[name]]$date), round(as.numeric(sensor$date) * 1000) / 1000)
    expect_identical(attributes(tag[[name]]$date), attributes(sensor$date))
    expect_identical(tag[[name]]$value, sensor$value)
  }
  cropped <- tag_create(
    "clock",
    pressure_file = sensor,
    crop_start = "2025-01-01 00:00:01",
    crop_end = "2025-01-01 00:00:02",
    quiet = TRUE
  )
  expect_identical(
    as.numeric(cropped$pressure$date),
    round(as.numeric(sensor$date[2]) * 1000) / 1000
  )
})

test_that("derived light grids and twilight labels retain millisecond timestamps", {
  light <- data.frame(
    date = as.POSIXct("2025-01-01", tz = "UTC") + (0:900) * 299 + 0.123,
    value = rep(c(rep(0, 144), rep(10, 144)), length.out = 901)
  )
  tag <- tag_create(
    "clock",
    manufacturer = "tabular",
    light_file = light,
    assert_pressure = FALSE,
    quiet = TRUE
  ) |>
    twilight_create(twl_offset = 0)
  mat <- ts2mat(tag$light)
  expect_identical(mat$date, round(mat$date * 1000) / 1000)
  tag$twilight$label <- rep(c("discard", ""), length.out = nrow(tag$twilight))
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  twilight_label_write(tag, file, quiet = TRUE)
  reloaded <- expect_no_warning(twilight_label_read(tag, file))
  expect_identical(reloaded$twilight, tag$twilight)
})

test_that("API responses restore millisecond dates before joining pressure measurements", {
  date <- as.POSIXct("2017-06-20", tz = "UTC") + (0:3) * 3600 + c(0.001, 0.123, 0.456, 0.999)
  pressure <- data.frame(date = date, value = 1000, label = "", stap_id = 1)
  httr2::local_mocked_responses(list(
    httr2::response(
      headers = list(`content-type` = "application/json"),
      body = charToRaw(
        '{"data":{"url":"https://example.com/pressure.csv","distInter":0,"lat":46,"lon":6}}'
      )
    ),
    httr2::response(
      body = charToRaw(glue::glue(
        "time,pressure,altitude\n{glue::glue_collapse(glue::glue('{trunc(as.numeric(date))},96000,100'), sep = '\n')}"
      ))
    )
  ))
  out <- geopressure_timeseries_api(lat = 46, lon = 6, pressure = pressure, quiet = TRUE)
  expect_identical(out$date, date)
  expect_true(all(out$surface_pressure == 960))
  expect_true(all(out$surface_pressure_norm == 1000))

  httr2::local_mocked_responses(list(httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(
      list(
        data = list(
          time = trunc(as.numeric(date)),
          surface_pressure = rep(96000, 4)
        )
      ),
      auto_unbox = TRUE
    ))
  )))
  path <- data.frame(stap_id = 1, lat = 46, lon = 6)
  tag <- tag_create("clock", pressure_file = pressure, quiet = TRUE)
  tag$stap <- data.frame(stap_id = 1, start = date[1], end = date[4])
  out <- pressurepath_create_api(
    tag,
    path,
    variable = "surface_pressure",
    solar_dep = NULL,
    era5_dataset = "single-levels",
    quiet = TRUE
  )
  expect_identical(out$date, date)
  expect_true(all(out$surface_pressure == 960))

  expect_identical(
    geopressure_api_restore_date(date, trunc(as.numeric(date[c(1, 3)]))),
    date[c(1, 3)]
  )
  sub_second <- date[1] + c(0, 0.1, 0.2)
  expect_identical(
    geopressure_api_restore_date(sub_second, trunc(as.numeric(sub_second))),
    sub_second
  )
  expect_error(
    geopressure_api_restore_date(sub_second, trunc(as.numeric(sub_second[1]))),
    "unambiguously"
  )
})

test_that("TRAINSET CSV input uses millisecond rounding at tag creation", {
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  csv <- data.frame(
    series = "pressure",
    timestamp = c(
      "2025-01-01T00:00:00.0004Z",
      "2025-01-01T00:30:00.1234Z",
      "2025-01-01T01:00:00.9996Z"
    ),
    value = 1000,
    label = ""
  )
  utils::write.csv(csv, file, row.names = FALSE)
  tag <- csv2tag(file)
  expected <- as.POSIXct("2025-01-01", tz = "UTC") + c(0, 1800.123, 3601)
  expect_identical(tag$pressure$date, expected)
  tag_label_write(tag, file, quiet = TRUE)
  reloaded <- expect_no_warning(tag_label_read(tag, file))
  expect_identical(reloaded$pressure$label, tag$pressure$label)
  expect_identical(reloaded$pressure$date, expected)
})

test_that("built-in TRAINSET saves millisecond pressure and acceleration labels", {
  app_dir <- system.file("trainset", package = "GeoPressureR")
  withr::local_dir(app_dir)
  library(shiny)
  library(plotly)
  library(bslib)
  sensor <- data.frame(date = as.POSIXct("2025-01-01", tz = "UTC") + (0:48) * 1800, value = 1000)
  tag <- tag_create(
    "clock",
    pressure_file = sensor,
    acceleration_file = sensor,
    time_drift = list(pressure = -0.2, acceleration = 0.1),
    quiet = TRUE
  )
  tag$pressure$label <- rep(c("", "flight", "discard"), length.out = 49)
  tag$acceleration$label <- rev(tag$pressure$label)
  app <- new.env(parent = globalenv())
  app$tag <- tag
  sys.source("utils.R", app)
  sys.source("server.R", app)
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  shiny::testServer(app$server, {
    write_labels_csv(file)
    reloaded <- expect_no_warning(tag_label_read(tag, file))
    expect_identical(reloaded$pressure, tag$pressure)
    expect_identical(reloaded$acceleration, tag$acceleration)
    payload <- jsonlite::fromJSON(output$ts_plot, simplifyVector = FALSE)
    chart_time <- unlist(payload$x$data[[1]]$x)
    expect_identical(as.POSIXct(chart_time, format = "%FT%H:%M:%OS", tz = "UTC"), tag$pressure$date)
    expect_true(all(grepl("\\.[0-9]{3}Z$", chart_time)))
    session$setInputs(label_select = "discard")
    apply_labels_to_points(
      NULL,
      selection_range = list(
        xmin = chart_time[2],
        xmax = chart_time[2],
        ymin = 999,
        ymax = 1001
      )
    )
    expect_identical(reactive_label_pres()[2], "discard")
  })
})

test_that("astronomical twilight labels also use millisecond timestamps", {
  sensor <- data.frame(date = as.POSIXct("2025-01-01", tz = "UTC") + (0:48) * 1800, value = 1000)
  tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE)
  path <- data.frame(date = sensor$date[c(1, 49)], lat = 46, lon = 6, stap_id = 1)
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file))
  for (solar_dep in c(0, 6)) {
    twl <- path2twilight(path, solar_dep = solar_dep, return_long = FALSE)
    for (column in c("sunrise", "sunset")) {
      expect_identical(as.numeric(twl[[column]]), round(as.numeric(twl[[column]]) * 1000) / 1000)
    }
    tag$twilight <- path2twilight(path, solar_dep = solar_dep)
    tag$twilight$label <- "discard"
    tag$param$twilight_create$twl_offset <- 0
    twilight_label_write(tag, file, quiet = TRUE)
    reloaded <- expect_no_warning(twilight_label_read(tag, file))
    expect_identical(reloaded$twilight, tag$twilight)
  }
})

test_that("tag creation warns only when rounding creates timestamp collisions", {
  sensor <- data.frame(
    date = as.POSIXct("2025-01-01", tz = "UTC") + c(0.0001, 0.0002, 1.1234),
    value = c(1000, 1001, 1002)
  )
  expect_warning(
    tag <- tag_create("clock", pressure_file = sensor, quiet = TRUE),
    "created 1 additional duplicate timestamp for pressure"
  )
  expect_equal(nrow(tag$pressure), nrow(sensor))
  expect_identical(tag$pressure$value, sensor$value)
  expect_identical(tag$pressure$date[1], tag$pressure$date[2])
  sensor$date[2] <- sensor$date[1]
  expect_no_warning(tag_create("clock", pressure_file = sensor, quiet = TRUE))
  sensor$date[2] <- sensor$date[1] + 0.002
  expect_no_warning(tag_create("clock", pressure_file = sensor, quiet = TRUE))
})
