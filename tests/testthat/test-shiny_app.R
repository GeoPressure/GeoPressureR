library(testthat)
library(GeoPressureR)

test_with_extdata()

start_bg_app <- function(fun, timeout = 15) {
  setTimeLimit(elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  suppressMessages(fun())
}

expect_bg_app_starts <- function(fun, timeout = 15) {
  p <- NULL
  on.exit(
    {
      if (inherits(p, "process") && isTRUE(p$is_alive())) {
        p$kill()
      }
    },
    add = TRUE
  )

  p <- NULL
  expect_no_error({
    p <- start_bg_app(fun, timeout = timeout)
  })
  expect_true(inherits(p, "process"))
  expect_true(isTRUE(p$is_alive()))
  Sys.sleep(1)
  expect_true(isTRUE(p$is_alive()))
}

make_shiny_test_data <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    tag_labelled <- tag_create("18LX", quiet = TRUE) |>
      tag_label(quiet = TRUE)

    tag_twilight <- tag_labelled |>
      twilight_create() |>
      twilight_label_read()

    tag_map <- tag_labelled |>
      tag_set_map(
        extent = c(-16, 23, 0, 50),
        scale = 4
      ) |>
      geopressure_map(quiet = TRUE)

    cache <<- list(
      tag_labelled = tag_labelled,
      tag_twilight = tag_twilight,
      tag_map = tag_map
    )
    cache
  }
})

test_that("trainset starts in background and stays responsive", {
  skip_on_cran()
  d <- make_shiny_test_data()
  expect_bg_app_starts(
    function() trainset(d$tag_labelled, run_bg = TRUE, launch_browser = FALSE),
    timeout = 15
  )
})

test_that("geolightviz starts in background and stays responsive", {
  skip_on_cran()
  d <- make_shiny_test_data()
  expect_bg_app_starts(
    function() geolightviz(d$tag_twilight, run_bg = TRUE, launch_browser = FALSE),
    timeout = 15
  )
})

test_that("geopressureviz starts in background and stays responsive", {
  skip_on_cran()
  d <- make_shiny_test_data()
  expect_bg_app_starts(
    function() geopressureviz(d$tag_map, run_bg = TRUE, launch_browser = FALSE),
    timeout = 15
  )
})
