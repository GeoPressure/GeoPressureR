library(testthat)
library(GeoPressureR)

# Set working directory
test_with_extdata()

test_that("tag_create() | manufacturer", {
  expect_no_error(tag_create(id = "18LX", quiet = TRUE))
  expect_no_error(tag_create(id = "CB621", quiet = TRUE))
  expect_no_error(tag_create(id = "X19D", quiet = TRUE))
  expect_no_error(tag_create(
    id = "C003654",
    manufacturer = "prestag",
    quiet = TRUE
  ))

  pres <- data.frame(
    date = as.POSIXct(
      c(
        "2017-06-20 00:00:00 UTC",
        "2017-06-20 01:00:00 UTC",
        "2017-06-20 02:00:00 UTC",
        "2017-06-20 03:00:00 UTC"
      ),
      tz = "UTC"
    ),
    value = c(1000, 1000, 1000, 1000)
  )
  expect_no_error(tag_create(id = "dummy", pressure_file = pres, quiet = TRUE))
})

test_that("tag_create() | default", {
  # with crop
  tag <- tag_create(
    id = "18LX",
    crop_start = "2017-06-20",
    crop_end = "2018-05-02",
    quiet = TRUE
  )

  # Check that the return tag is correct (suppress CLI output)
  expect_no_error(invisible(capture.output(print(tag), type = "message")))
  expect_no_error(tag_assert(tag, "pressure"))
  expect_no_error(tag_assert(tag, "light"))
  expect_no_error(tag_assert(tag, "acceleration"))

  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)
  expect_gt(nrow(tag$acceleration), 0)

  expect_error(expect_warning(suppressMessages(tag_create(
    id = "18LX",
    pressure_file = "wrong_file"
  ))))
  expect_warning(suppressMessages(tag_create(id = "18LX", light_file = "wrong_file")))

  # Check crop: invalid range errors
  expect_error(
    tag_create(
      id = "18LX",
      crop_start = "2019-06-20",
      crop_end = "2018-05-02",
      quiet = TRUE
    ),
    "must be strictly earlier"
  )

  # Check crop: valid range but no remaining data errors
  expect_error(
    tag_create(
      id = "18LX",
      crop_start = "2019-06-20",
      crop_end = "2019-06-21",
      quiet = TRUE
    ),
    "No data left after cropping"
  )
})

test_that("tag_create() | Migrate Technology", {
  tag <- tag_create(
    id = "CB621",
    pressure_file = "*.deg",
    light_file = "*.lux",
    acceleration_file = NA,
    quiet = TRUE
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
  expect_no_error(invisible(capture.output(print(tag), type = "message")))
  expect_gt(nrow(tag$pressure), 0)
  expect_gt(nrow(tag$light), 0)

  tag <- tag_create(
    id = "CB621",
    acceleration_file = NA,
    quiet = TRUE
    # crop_start = "2017-06-20", crop_end = "2018-05-02"
  )
})


test_that("tag_create() | no acceleration", {
  expect_no_error({
    tag <- tag_create(
      id = "18LX",
      acceleration_file = NA,
      light_file = NA,
      quiet = TRUE
    )
  })
  expect_no_error(invisible(capture.output(print(tag), type = "message")))

  expect_no_error({
    tag <- tag_label_read(
      tag,
      file = "./data/tag-label/18LX-labeled-no_acc.csv"
    )
  })
  expect_no_error(invisible(capture.output(print(tag), type = "message")))
  expect_no_error(tag_label_stap(tag, quiet = TRUE))
})

test_that("tag_create() | BAS light-only 19006", {
  tag <- tag_create(
    id = "19006",
    assert_pressure = FALSE,
    quiet = TRUE
  )

  expect_true("light" %in% names(tag))
  expect_gt(nrow(tag$light), 0)
  expect_false("pressure" %in% names(tag))
  expect_equal(tag$param$tag_create$manufacturer, "bas")
  expect_match(tag$param$tag_create$light_file, "19006\\.lig$")
})

test_that("tag_create() | SOI magnetic-only 14DM", {
  tag <- tag_create(
    id = "14DM",
    manufacturer = "soi",
    pressure_file = NA,
    light_file = NA,
    acceleration_file = NA,
    temperature_external_file = NA,
    temperature_internal_file = NA,
    assert_pressure = FALSE,
    quiet = TRUE
  )

  expect_true("magnetic" %in% names(tag))
  expect_gt(nrow(tag$magnetic), 0)
  expect_true(all(
    c(
      "date",
      "magnetic_x",
      "magnetic_y",
      "magnetic_z",
      "acceleration_x",
      "acceleration_y",
      "acceleration_z"
    ) %in%
      names(tag$magnetic)
  ))
  expect_equal(tag$param$tag_create$manufacturer, "soi")
  expect_match(tag$param$tag_create$magnetic_file, "14DM_20160630\\.magnetic$")
  expect_true(all(c("left", "backward", "down") == tag$param$mag_axis))
})

test_that("tag_create() | tabular full input and pressure bounds warning", {
  d <- as.POSIXct(c("2017-06-20 00:00:00", "2017-06-20 01:00:00"), tz = "UTC")
  pressure <- data.frame(date = d, value = c(1000, 1200))
  light <- data.frame(date = d, value = c(1, 2))
  acceleration <- data.frame(date = d, value = c(10, 20))
  temperature_external <- data.frame(date = d, value = c(15, 16))
  temperature_internal <- data.frame(date = d, value = c(35, 36))
  magnetic <- data.frame(
    date = d,
    acceleration_x = c(0.1, 0.2),
    acceleration_y = c(0.3, 0.4),
    acceleration_z = c(0.5, 0.6),
    magnetic_x = c(1, 2),
    magnetic_y = c(3, 4),
    magnetic_z = c(5, 6)
  )

  expect_warning(
    tag <- tag_create(
      id = "dummy_df",
      manufacturer = "tabular",
      pressure_file = pressure,
      light_file = light,
      acceleration_file = acceleration,
      temperature_external_file = temperature_external,
      temperature_internal_file = temperature_internal,
      magnetic_file = magnetic,
      quiet = TRUE
    ),
    "Pressure observation should be between"
  )

  expect_true(all(
    c(
      "pressure",
      "light",
      "acceleration",
      "temperature_external",
      "temperature_internal",
      "magnetic"
    ) %in%
      names(tag)
  ))
  expect_false("temperature" %in% names(tag))
  expect_equal(tag$param$tag_create$manufacturer, "tabular")
})

test_that("tag_create() | tabular csv and in-memory input", {
  dt <- as.POSIXct(
    c("2017-06-20 00:00:00", "2017-06-20 01:00:00"),
    tz = "UTC"
  )
  csv <- data.frame(
    datetime = format(dt, "%Y-%m-%dT%H:%M"),
    value = c(1000, 1001)
  )
  file_pressure <- tempfile(fileext = ".csv")
  utils::write.csv(csv, file_pressure, row.names = FALSE)

  tag <- tag_create(
    id = "dummy_tab",
    manufacturer = "tabular",
    pressure_file = file_pressure,
    quiet = TRUE
  )
  expect_true("pressure" %in% names(tag))
  expect_equal(tag$param$tag_create$pressure_file, file_pressure)

  pressure_table <- data.frame(
    date = dt,
    value = c(1000, 1001)
  )
  tag_in_memory <- tag_create(
    id = "dummy_tab2",
    manufacturer = "tabular",
    pressure_file = pressure_table,
    quiet = TRUE
  )
  expect_true("pressure" %in% names(tag_in_memory))
  expect_equal(tag_in_memory$param$tag_create$manufacturer, "tabular")
  expect_equal(tag_in_memory$param$tag_create$pressure_file, "in_memory")
})

test_that("tag_create_soi() | airtemperature fallback and optional sensors", {
  dir_tmp <- tempfile("gpr-soi-")
  dir.create(dir_tmp, recursive = TRUE)
  on.exit(unlink(dir_tmp, recursive = TRUE), add = TRUE)
  copied <- file.copy(
    list.files("./data/raw-tag/18LX", full.names = TRUE),
    dir_tmp
  )
  expect_true(all(copied))

  file_temp <- list.files(dir_tmp, pattern = "\\.temperature$", full.names = TRUE)
  file_air <- sub("\\.temperature$", ".airtemperature", file_temp)
  expect_true(file.rename(file_temp, file_air))

  tag <- tag_create(
    id = "18LX",
    manufacturer = "soi",
    directory = dir_tmp,
    quiet = TRUE
  )
  expect_true("temperature_external" %in% names(tag))
  expect_match(tag$param$tag_create$temperature_external_file, "\\.airtemperature$")

  tag_no_opt <- tag_create(
    id = "18LX",
    manufacturer = "soi",
    directory = dir_tmp,
    light_file = NA,
    acceleration_file = NA,
    temperature_external_file = NA,
    temperature_internal_file = NA,
    magnetic_file = NA,
    quiet = TRUE
  )
  expect_true("pressure" %in% names(tag_no_opt))
  expect_false(any(
    c(
      "light",
      "acceleration",
      "temperature_external",
      "temperature_internal",
      "magnetic"
    ) %in%
      names(tag_no_opt)
  ))
})

test_that("param_create() | default", {
  expect_no_error(param_create(id = "18LX", extent = c(0, 0, 1, 1)))
  expect_no_error(param_create(id = "18LX", default = TRUE))
})
