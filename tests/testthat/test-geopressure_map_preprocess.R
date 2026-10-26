test_that("pressure smoothing never replaces unsupported gaps with zero pressure", {
  tag <- structure(
    list(
      param = list(),
      pressure = data.frame(
        date = as.POSIXct("2020-01-01", tz = "UTC") + (0:24) * 3600,
        value = 1000,
        label = "",
        stap_id = 1
      ),
      stap = data.frame(stap_id = 1, known_lat = NA_real_)
    ),
    class = "tag"
  )
  expect_equal(geopressure_map_preprocess(tag)$value, rep(1000, 25))

  tag$pressure$value[12] <- NA_real_
  expect_equal(geopressure_map_preprocess(tag)$value, rep(1000, 25))

  tag$pressure$value[6:20] <- NA_real_
  expect_error(geopressure_map_preprocess(tag), "anyNA")
})

test_that("short stationary periods retain their single hourly reading", {
  tag <- structure(
    list(
      param = list(),
      pressure = data.frame(
        date = as.POSIXct("2020-01-01 17:40:00", tz = "UTC") + (0:7) * 300,
        value = 1000 + 0:7,
        label = "",
        stap_id = 1
      ),
      stap = data.frame(stap_id = 1, known_lat = NA_real_)
    ),
    class = "tag"
  )

  expect_warning(processed <- geopressure_map_preprocess(tag), "less than 3 datapoints")
  expect_equal(processed$date, as.POSIXct("2020-01-01 18:00:00", tz = "UTC"))
  expect_equal(processed$value, 1004)
})
