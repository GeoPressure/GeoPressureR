test_that("geopressuretemplate() passes wind parameters without provenance fields", {
  file <- withr::local_tempfile(fileext = ".RData")
  defaults <- param_create("test", default = TRUE)
  local_mocked_bindings(
    param_create = function(...) defaults,
    geopressuretemplate_tag = function(id, file, ...) {
      tag <- list(param = list(id = id), map_pressure = TRUE, pressure = 1000)
      param <- tag$param
      save(tag, param, file = file)
    },
    tag_assert = function(tag) invisible(NULL),
    graph_create = function(tag, ...) list(param = tag$param),
    graph_add_wind = function(
      graph,
      pressure,
      quiet,
      thr_as,
      file,
      rounding_interval,
      interp_spatial_linear
    ) {
      graph$param$wind <- list(pressure = pressure, thr_as = thr_as)
      graph
    },
    bird_create = function(...) list(),
    graph_set_movement = function(graph, bird, ...) {
      graph$param$movement <- TRUE
      graph
    },
    graph_marginal = function(graph, ...) graph$param$movement,
    geopressuretemplate_pressurepath = function(...) invisible(NULL),
    .package = "GeoPressureR"
  )

  geopressuretemplate(
    "test",
    file = file,
    quiet = TRUE,
    config = list(
      geopressuretemplate = list(likelihood = "map_pressure", outputs = "marginal"),
      graph_set_movement = list(type = "as")
    ),
    graph_add_wind = list(thr_as = 85)
  )

  saved <- load(file)
  expect_true("marginal" %in% saved)
  expect_true(marginal)
  expect_equal(param$wind, list(pressure = 1000, thr_as = 85))
})
