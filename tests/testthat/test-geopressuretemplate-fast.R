test_that("graph template saves every requested output", {
  file <- withr::local_tempfile(fileext = ".RData")
  tag <- list(param = list(id = "test"), map_pressure = TRUE)
  param <- tag$param
  save(tag, param, file = file)

  local_mocked_bindings(
    tag_assert = function(...) invisible(NULL),
    geopressuretemplate_config = function(...) {
      list(
        geopressuretemplate = list(
          likelihood = "map_pressure",
          outputs = c("marginal", "most_likely", "simulation")
        ),
        graph_create = list(),
        graph_set_movement = list(type = "gs"),
        graph_simulation = list(nj = 2)
      )
    },
    graph_create = function(tag, ...) list(param = tag$param),
    graph_set_movement = function(graph, ...) graph,
    graph_marginal = function(...) "marginal",
    graph_most_likely = function(...) "most_likely",
    graph_simulation = function(...) {
      structure("simulation", param = list(graph_simulation = list(nj = 2)))
    },
    path2edge = function(path, ...) path,
    .package = "GeoPressureR"
  )

  geopressuretemplate_graph("test", file = file, quiet = TRUE, config = list())
  expect_setequal(
    load(file),
    c(
      "tag",
      "param",
      "marginal",
      "path_most_likely",
      "edge_most_likely",
      "path_simulation",
      "edge_simulation"
    )
  )
  expect_equal(marginal, "marginal")
  expect_equal(path_most_likely, "most_likely")
  expect_equal(as.character(path_simulation), "simulation")
  expect_equal(param$graph_simulation$nj, 2)
})

test_that("pressurepath template uses the matching path for each output", {
  file <- withr::local_tempfile(fileext = ".RData")
  tag <- list(param = list(id = "test"))
  param <- tag$param
  path_most_likely <- 1L
  path_geopressureviz <- 2L
  path_tag <- 3L
  path_simulation <- 4L
  save(tag, param, path_most_likely, path_geopressureviz, path_tag, path_simulation, file = file)

  local_mocked_bindings(
    geopressuretemplate_config = function(...) {
      list(
        geopressuretemplate = list(
          pressurepath = c("most_likely", "geopressureviz", "tag", "simulation")
        ),
        pressurepath_create = list(variable = "surface_pressure")
      )
    },
    tag_assert = function(...) invisible(NULL),
    pressurepath_create = function(tag, path, quiet, variable) path,
    .package = "GeoPressureR"
  )

  geopressuretemplate_pressurepath("test", file = file, quiet = TRUE, config = list())
  load(file)
  expect_equal(pressurepath_most_likely, 1L)
  expect_equal(pressurepath_geopressureviz, 2L)
  expect_equal(pressurepath_tag, 3L)
  expect_equal(pressurepath_simulation, 4L)
})
