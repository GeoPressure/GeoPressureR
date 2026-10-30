test_that("actograms preserve flight markers with non-finite acceleration gaps", {
  withr::local_seed(1)
  tag <- structure(
    list(
      param = list(),
      acceleration = data.frame(
        date = as.POSIXct("2020-01-01", tz = "UTC") + (0:71) * 3600,
        value = rep(c(1, 2, 90, 100), 18),
        label = rep("", 72)
      )
    ),
    class = "tag"
  )
  tag$acceleration$value[5:8] <- c(NA, NaN, Inf, -Inf)
  tag$acceleration$label[12:16] <- "flight"
  original <- tag

  p <- plot(tag, type = "actogram", twl_offset = 0, plot_plotly = FALSE)
  expect_equal(sum(p$layers[[1]]$data$acceleration == 101), 5)
  expect_true(all(is.finite(p$layers[[1]]$data$acceleration)))
  expect_equal(p$scales$scales[[1]]$limits, c(-1, 101))
  built <- ggplot2::ggplot_build(p)
  expect_equal(sum(built$data[[1]]$fill == "#FF0000"), 5)
  expect_s3_class(plot(tag, type = "actogram", twl_offset = 0), "plotly")
  expect_identical(tag, original)
})
