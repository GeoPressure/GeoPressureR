# pressurepath_finalize() is the single place that converts surface pressure to hPa, fixes the
# column order and records provenance, so both backends produce an indistinguishable object.

fake_input <- function(variable = c("altitude", "surface_pressure", "temperature_2m")) {
  pp <- data.frame(
    stap_id = rep(1L, 4),
    label = rep("", 4),
    date = as.POSIXct("2020-07-15 12:00:00", tz = "UTC") + 3600 * (0:3),
    pressure_tag = c(950, 951, 952, 953),
    lon = rep(12.5, 4),
    lat = rep(43.5, 4),
    temperature_2m = rep(290, 4),
    altitude = c(500, 505, 510, 515),
    surface_pressure = c(95000, 95100, 95200, 95300) # Pa, as every ERA5 source delivers it
  )
  attr(pp, "variable") <- variable
  pp
}

fake_tag <- structure(
  list(param = list(id = "XX01", geopressure_map = list(sd = 1))),
  class = c("tag", "list")
)
fake_path <- structure(data.frame(stap_id = 1L, lat = 43.5, lon = 12.5), type = "most_likely")

finalize <- function(pp, variable = attr(pp, "variable"), ...) {
  pressurepath_finalize(
    pp,
    fake_tag,
    fake_path,
    preprocess = FALSE,
    solar_dep = NULL,
    variable = variable,
    ...
  )
}

test_that("surface pressure is converted to hPa exactly once", {
  out <- finalize(fake_input())
  expect_equal(out$surface_pressure, c(950, 951, 952, 953))
  # the normalisation must be computed in hPa, alongside pressure_tag
  expect_equal(
    out$surface_pressure_norm,
    out$surface_pressure - mean(out$surface_pressure) + mean(out$pressure_tag)
  )
})

test_that("columns follow a fixed order regardless of input order", {
  pp <- fake_input()
  shuffled <- pp[c(
    "surface_pressure",
    "lat",
    "altitude",
    "date",
    "pressure_tag",
    "lon",
    "temperature_2m",
    "stap_id",
    "label"
  )]
  attr(shuffled, "variable") <- attr(pp, "variable")
  expect_equal(
    names(finalize(shuffled)),
    c(
      "date",
      "stap_id",
      "pressure_tag",
      "label",
      "lat",
      "lon",
      "altitude",
      "surface_pressure",
      "temperature_2m",
      "surface_pressure_norm"
    )
  )
})

test_that("variables appear in the order they were requested", {
  pp <- fake_input()
  out <- finalize(pp, variable = c("temperature_2m", "surface_pressure", "altitude"))
  expect_equal(
    names(out)[7:9],
    c("temperature_2m", "surface_pressure", "altitude")
  )
})

test_that("unknown columns are kept, never dropped", {
  pp <- fake_input()
  pp$j <- 1L
  pp$anything_else <- "kept"
  out <- finalize(pp)
  expect_true(all(c("j", "anything_else") %in% names(out)))
  # and they sit after the requested variables, before the derived ones
  expect_lt(match("altitude", names(out)), match("j", names(out)))
  expect_lt(match("j", names(out)), match("surface_pressure_norm", names(out)))
})

test_that("provenance is recorded on the result", {
  out <- finalize(fake_input(), era5_dataset = "land", source = "arco")
  expect_equal(attr(out, "era5_dataset"), "land")
  expect_equal(attr(out, "source"), "arco")
  expect_equal(attr(out, "variable"), c("altitude", "surface_pressure", "temperature_2m"))
  # the attributes that were already there must survive the reordering
  expect_equal(attr(out, "id"), "XX01")
  expect_equal(attr(out, "sd"), 1)
  expect_equal(attr(out, "type"), "most_likely")
  expect_false(attr(out, "preprocess"))
})

test_that("a path without surface pressure gets no normalisation", {
  pp <- fake_input(variable = "altitude")
  pp$surface_pressure <- NULL
  pp$temperature_2m <- NULL
  out <- finalize(pp)
  expect_false("surface_pressure_norm" %in% names(out))
  expect_equal(names(out), c("date", "stap_id", "pressure_tag", "label", "lat", "lon", "altitude"))
})
