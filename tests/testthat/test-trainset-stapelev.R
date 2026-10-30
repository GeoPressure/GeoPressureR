test_that("TRAINSET initialization ignores starting offsets and fails atomically", {
  app <- new.env(parent = globalenv())
  sys.source(system.file("trainset", "stapelev.R", package = "GeoPressureR"), app)
  p <- data.frame(
    date = as.POSIXct("2026-01-01", tz = "UTC") + (0:23) * 600,
    residual = rep(c(0, 5), each = 12),
    label = "discard"
  )
  windows <- matrix(rep(range(as.numeric(p$date)), 2), ncol = 2, byrow = TRUE)
  expect_identical(app$stapelev_initialize_offsets(p, windows, 1, rep(TRUE, 24)), c(0, 5))
  expect_null(app$stapelev_initialize_offsets(p, windows, 10, rep(TRUE, 24)))
  p$label[1:12] <- "flight"
  expect_identical(
    app$stapelev_initialize_offsets(p, windows[1, , drop = FALSE], 1, rep(TRUE, 24)),
    5
  )
})

test_that("TRAINSET applies each level's tolerance, solar mask and continuity", {
  app <- new.env(parent = globalenv())
  sys.source(system.file("trainset", "stapelev.R", package = "GeoPressureR"), app)
  p <- data.frame(
    date = as.POSIXct("2026-01-01", tz = "UTC") + (0:23) * 600,
    residual = rep(c(0, 5), each = 12),
    label = "discard"
  )
  windows <- matrix(rep(range(as.numeric(p$date)), 2), ncol = 2, byrow = TRUE)
  offsets <- stats::setNames(c(0, 5), c("", "elev_1"))
  a <- app$stapelev_assign_offsets(p, offsets, windows, c(1, 1), c(3, 0), gap = c(10, 0))
  expect_true(all(a$label[1:12] == "discard"))
  expect_true(all(a$label[13:24] == "elev_1"))
  b <- app$stapelev_assign_offsets(
    p,
    rev(offsets),
    windows[2:1, ],
    c(1, 1),
    c(0, 3),
    gap = c(0, 10)
  )
  expect_identical(a, b)
  eligible <- cbind(rep(FALSE, 24), rep(TRUE, 24))
  b <- app$stapelev_assign_offsets(p, offsets, windows, c(1, 1), 0, eligible)
  expect_true(all(b$label[1:12] == "discard"))
  # A closer level outside its own tolerance cannot block a valid wider band.
  p$residual[] <- 1
  b <- app$stapelev_assign_offsets(
    p,
    stats::setNames(c(0, 4), names(offsets)),
    windows,
    c(0.5, 4),
    0
  )
  expect_true(all(b$label == "elev_1"))
})

test_that("TRAINSET bridges short interruptions without a hidden percentage limit", {
  app <- new.env(parent = globalenv())
  sys.source(system.file("trainset", "stapelev.R", package = "GeoPressureR"), app)
  p <- data.frame(
    date = as.POSIXct("2026-01-01", tz = "UTC") + (0:23) * 600,
    residual = 0,
    label = "discard"
  )
  spikes <- seq(3, 21, 3)
  p$residual[spikes] <- 10
  window <- matrix(range(as.numeric(p$date)), nrow = 1)
  offsets <- stats::setNames(0, "")
  a <- app$stapelev_assign_offsets(p, offsets, window, 1, 3, gap = 10)
  expect_equal(sum(a$label == ""), 17)
  expect_true(all(a$label[spikes] == "discard"))
  b <- app$stapelev_assign_offsets(p, offsets, window, 1, 3, gap = 0)
  expect_true(all(b$label == "discard"))
  eligible <- rep(TRUE, 24)
  eligible[12] <- FALSE
  b <- app$stapelev_assign_offsets(p, offsets, window, 1, 3, eligible, gap = 60)
  expect_true(all(b$label == "discard"))
})
