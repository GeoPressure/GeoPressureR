#' GPT3 temperature lapse rate
#'
#' Lapse rate (K/m) of the GPT3 empirical troposphere model (Landskron & Böhm 2018,
#' <https://doi.org/10.1007/s00190-017-1066-2>) at each position and date: a mean, annual and
#' semi-annual cycle on a 1° grid, bilinear between the four nearest grid centres as in the
#' reference implementation (`gpt3_1_fast.m`), and nearest along latitude in the polar rows. The
#' grid is downloaded once from TU Wien and only its lapse-rate coefficients are cached.
#'
#' @param lon,lat Longitude and latitude (°), recycled with `date`.
#' @param date `POSIXct` date.
#' @param cache_dir Directory of the cached coefficients.
#' @noRd
gpt3_lapse_rate <- function(
  lon,
  lat,
  date,
  cache_dir = tools::R_user_dir("GeoPressureR", "cache")
) {
  file <- file.path(cache_dir, "gpt3_1_lapse_rate.rds")
  if (!file.exists(file)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    grd <- tempfile(fileext = ".grd")
    httr2::request("https://vmf.geo.tuwien.ac.at/codes/gpt3_1.grd") |>
      httr2::req_perform(path = grd)
    # Columns 18-22: mean, annual and semi-annual harmonics of dT/dh in K/km
    saveRDS(
      as.matrix(utils::read.table(
        grd,
        skip = 1,
        colClasses = rep(c("NULL", "numeric", "NULL"), c(17, 5, 42))
      )) /
        1000,
      file
    )
    unlink(grd)
  }
  coef <- readRDS(file)
  n <- max(length(lon), length(lat), length(date))
  # Fractional row and column of the grid centres, from 89.5° N and 0.5° E
  i <- pmin(pmax(89.5 - rep_len(lat, n), 0), 179)
  j <- rep_len(lon, n) %% 360 - 0.5
  i0 <- floor(i)
  j0 <- floor(j)
  di <- i - i0
  dj <- j - j0
  i1 <- pmin(i0 + 1, 179)
  j1 <- (j0 + 1) %% 360
  j0 <- j0 %% 360
  doy <- 2 * pi * rep_len(as.POSIXlt(date, tz = "UTC")$yday + 1, n) / 365.25
  h <- cbind(1, cos(doy), sin(doy), cos(2 * doy), sin(2 * doy))
  v <- \(i, j) rowSums(coef[i * 360 + j + 1, , drop = FALSE] * h)
  (1 - di) *
    (1 - dj) *
    v(i0, j0) +
    di * (1 - dj) * v(i1, j0) +
    (1 - di) * dj * v(i0, j1) +
    di * dj * v(i1, j1)
}
