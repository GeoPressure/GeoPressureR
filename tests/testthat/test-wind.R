library(testthat)
library(GeoPressureR)

skip_if_test_level_below("full")

# Set working directory
test_with_extdata()

tag <- geopressuretemplate_tag("18LX", quiet = TRUE)

test_that("tag_download_wind() | download all flights", {
  tag_download_wind(
    tag,
    variable = c("u_component_of_wind", "v_component_of_wind", "temperature")
  )
  expect_true(all(file.exists(glue::glue(
    "./data/wind/18LX/18LX_{utils::head(tag$stap$stap_id, -1)}.nc"
  ))))
  expect_no_error(edge_add_wind_check(tag))
})

graph <- graph_create(tag, quiet = TRUE)

test_that("graph_add_wind() | default", {
  graph_w <- graph_add_wind(graph, pressure = tag$pressure, quiet = TRUE)
  expect_length(graph_w$ws, length(graph$s))
  expect_true(all(is.finite(graph_w$ws)))
})

test_that("graph_add_wind() | rounding_interval and spatial interpolation", {
  graph_1 <- graph_add_wind(graph, pressure = tag$pressure, rounding_interval = 1, quiet = TRUE)
  graph_60 <- graph_add_wind(graph, pressure = tag$pressure, rounding_interval = 60, quiet = TRUE)
  expect_lt(stats::median(abs(graph_60$ws - graph_1$ws)), 5)

  graph_nn <- graph_add_wind(
    graph,
    pressure = tag$pressure,
    rounding_interval = 60,
    interp_spatial_linear = FALSE,
    quiet = TRUE
  )
  expect_lt(stats::median(abs(graph_nn$ws - graph_60$ws)), 5)
})

test_that("edge_add_wind() | along most likely path", {
  graph_w <- graph_add_wind(graph, pressure = tag$pressure, quiet = TRUE)
  graph_w <- graph_set_movement(graph_w, bird = bird_create("Acrocephalus arundinaceus"))
  path_most_likely <- graph_most_likely(graph_w, quiet = TRUE)
  edge_most_likely <- path2edge(path_most_likely, graph_w)

  edge <- edge_add_wind(
    graph_w,
    edge_s = edge_most_likely$s,
    edge_t = edge_most_likely$t,
    pressure = tag$pressure,
    variable = c("u_component_of_wind", "v_component_of_wind", "temperature"),
    quiet = TRUE
  )
  expect_true(all(c("edge_id", "val", "var") %in% names(edge)))
  expect_setequal(unique(edge$var), c("u_component_of_wind", "v_component_of_wind", "temperature"))
})
