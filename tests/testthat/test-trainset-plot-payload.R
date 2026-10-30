test_that("TRAINSET compacts plot metadata without changing plotted or selected points", {
  skip_if_not_installed("plotly")
  app <- new.env(parent = globalenv())
  sys.source(system.file("trainset", "utils.R", package = "GeoPressureR"), app)
  date <- as.POSIXct("2026-01-01", tz = "UTC") + (0:99) * 600
  p <- plotly::plot_ly(
    x = trainset_format_time(date),
    y = seq_along(date),
    customdata = seq_along(date),
    text = rep(c("", "discard"), 50),
    type = "scattergl",
    mode = "markers",
    hovertemplate = "%{x}: %{text}",
    hoverinfo = "text"
  ) |>
    plotly::layout(xaxis = list(type = "date"))
  before <- p$preRenderHook(p)
  compact <- app$trainset_compact_plot(p)
  after <- compact$preRenderHook(compact)
  for (field in c("x", "y", "customdata", "text", "marker", "type", "mode")) {
    expect_identical(after$x$data[[1]][[field]], before$x$data[[1]][[field]])
  }
  expect_identical(
    as.character(rep(after$x$data[[1]]$hovertemplate, length(date))),
    as.character(before$x$data[[1]]$hovertemplate)
  )
  expect_null(after$x$layout$xaxis$categoryarray)
  expect_identical(after$x$layout$xaxis$type, "date")
  # Per-point hover content must remain per-point.
  p <- plotly::plot_ly(
    x = date,
    y = seq_along(date),
    type = "scatter",
    mode = "markers",
    hovertemplate = as.character(seq_along(date))
  )
  compact <- app$trainset_compact_plot(p)
  expect_length(compact$preRenderHook(compact)$x$data[[1]]$hovertemplate, length(date))
})
