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
