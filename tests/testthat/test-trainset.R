library(testthat)
library(GeoPressureR)

test_with_extdata()

test_that("trainset() | input validation", {
  # Invalid input type
  expect_error(trainset(list()), "must be a")

  # Invalid file/id
  expect_error(trainset("non_existent_file.RData"), "Cannot find data files")
})

test_that("trainset() | foreground execution returns tag", {
  tag <- tag_create("18LX", quiet = TRUE)

  captured_options <- new.env()

  local_mocked_bindings(
    shinyOptions = function(...) {
      args <- list(...)
      for (n in names(args)) {
        captured_options[[n]] <- args[[n]]
      }
    },
    runApp = function(...) "app_ran",
    .package = "shiny"
  )

  result <- trainset(tag, run_bg = FALSE, launch_browser = FALSE)

  # Returns the tag invisibly
  expect_true(inherits(result, "tag"))

  # Shiny options receive tag and label_dir
  expect_true(!is.null(captured_options$tag))
  expect_true(!is.null(captured_options$label_dir))
  expect_true(inherits(captured_options$tag, "tag"))
})

test_that("trainset() | background execution", {
  tag <- tag_create("18LX", quiet = TRUE)

  local_mocked_bindings(
    r_bg = function(...) {
      structure(
        list(
          is_alive = function() FALSE,
          poll_io = function(...) {},
          read_error = function() "",
          read_output = function() "Listening on http://127.0.0.1:1234"
        ),
        class = "r_process"
      )
    },
    .package = "callr"
  )

  result <- trainset(tag, run_bg = TRUE, launch_browser = FALSE)
  expect_true(inherits(result, "r_process"))
})
