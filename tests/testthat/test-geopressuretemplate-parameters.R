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

test_that("template parameter blocks match their function arguments", {
  defaults <- param_create("test", default = TRUE)
  supplied <- list(
    tag_create = c("id", "quiet"),
    tag_label = c("tag", "quiet"),
    tag_set_map = "tag",
    twilight_create = "tag",
    twilight_label_read = "tag",
    geolight_map = c("tag", "quiet"),
    geopressure_map = c("tag", "quiet"),
    graph_create = c("tag", "quiet"),
    bird_create = character(),
    graph_set_movement = c("graph", "bird"),
    pressurepath_create = c("tag", "path", "quiet")
  )

  for (block in names(supplied)) {
    arguments <- names(defaults[[block]])
    accepted <- names(formals(get(block, asNamespace("GeoPressureR"))))
    expect_equal(setdiff(arguments, accepted), character(), info = block)
    expect_equal(intersect(arguments, supplied[[block]]), character(), info = block)
  }

  wind <- names(defaults$graph_add_wind)
  accepted <- union(
    names(formals(graph_add_wind)),
    names(formals(get("add_wind_graph_edge", asNamespace("GeoPressureR"))))
  )
  expect_length(setdiff(wind, c(accepted, "pressure_source")), 0)
  expect_length(intersect(wind, c("graph", "pressure", "quiet")), 0)
  expect_setequal(names(defaults$graph_simulation), "nj")
})

test_that("graph validation uses merged template parameters", {
  config <- list(
    geopressuretemplate = list(likelihood = "map_pressure", outputs = "marginal"),
    graph_simulation = list(nj = 10)
  )

  expect_error(
    geopressuretemplate_config(
      "test",
      config = config,
      assert_graph = TRUE,
      geopressuretemplate = list(outputs = "simulation"),
      graph_simulation = list(nj = 0)
    ),
    "nj"
  )

  config$geopressuretemplate$outputs <- "simulation"
  config$graph_simulation$nj <- 0
  expect_no_error(
    geopressuretemplate_config(
      "test",
      config = config,
      assert_graph = TRUE,
      geopressuretemplate = list(outputs = "marginal")
    )
  )
})
