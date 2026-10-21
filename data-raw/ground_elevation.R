# Ground elevation statistics on the GeoPressureR map grids and on the ERA5 0.25° grid
#
# Builds the global rasters published as GitHub release assets under the tag
# `ground-elevation-v1`. Each raster holds, for every grid cell, the minimum, the 10th to 90th
# percentiles (step 10) and the maximum of the Copernicus GLO-90 DEM pixels inside the cell.
#
# Why: a pressure path gives the bird's altitude above sea level; the ground below it is needed
# to express it above ground and to check that it is physically possible (see
# GeoPressure/GeoPressureAPI#33 for the issues with the former Earth Engine layers).
#
# Source
# - Copernicus DEM GLO-90 (DGED, 3 arc-second), cloud-optimised GeoTIFFs on the AWS open data
#   registry: https://registry.opendata.aws/copernicus-dem/ (s3://copernicus-dem-90m).
#   © DLR e.V. 2010-2014 and © Airbus Defence and Space GmbH 2014-2018, provided under COPERNICUS
#   by the European Union and ESA; all rights reserved. Free licence:
#   https://spacedata.copernicus.eu/documents/20123/121286/CSCDA_ESA_Mission-specific+Annex_31_Oct_22.pdf
# - Tile list: https://copernicus-dem-90m.s3.amazonaws.com/tileList.txt (26,475 1° tiles). Tiles
#   absent from the list are open ocean.
# - At 0.2° cells, GLO-30 and GLO-90 percentiles agree within ~1 m and minima within ~3 m
#   (median), so GLO-90 is used: 9 times less data for the same statistics.
#
# Outputs (one Cloud Optimised GeoTIFF each, INT16, metres above the EGM2008 geoid, 11 bands
# named min, p10, ..., p90, max)
# - `glo90-ground-scale{s}.tif` for s in 1, 2, 4, 5, 10 pixels per degree: the map grid of
#   `tag_set_map(scale = s)`, cell edges on multiples of 1/s, extent -180, 180, -90, 90.
# - `glo90-ground-era5-0.25.tif`: cells centred on the ERA5 single-levels grid (multiples of
#   0.25°), extent -180.125, 179.875, -90.125, 90.125, i.e. the same 1440 x 721 grid as the
#   ERA5 geopotential read by `era5_surface_elevation()`.
#
# Conventions
# - A DEM pixel belongs to the cell containing its centre. GLO pixel centres fall exactly on cell
#   edges, so cells are closed on their west and north edge: lon in [W, E), lat in (S, N]. Every
#   map-grid cell therefore holds pixels from a single 1° tile, and every cell of a scale gets the
#   same number of pixels.
# - Sea pixels are 0 in GLO tiles, and missing (ocean) tiles count as 0, so coastal cells include
#   the sea surface, as the ground a bird can fly over.
# - Percentiles use `quantile(type = 7)` on pixel values, unweighted by pixel area. GLO-90 widens
#   pixels in longitude above 50° latitude; ERA5 cells straddling 50, 60, 70 or 80° mix both
#   pixel sizes.
#
# Running
# - Requires only R with terra, httr2, glue and cli. `parallel::mclapply()` forks, so on
#   Windows set `cores <- 1`.
# - Keep the computer usable: run at background priority, which on Apple silicon also keeps the
#   workers on efficiency cores and lowers their disk priority:
#     taskpolicy -b Rscript data-raw/ground_elevation.R            # macOS
#     nice -n 19 ionice -c 3 Rscript data-raw/ground_elevation.R   # Linux
#   A tile takes ~1 s of CPU, so `cores = 4` already keeps up with a ~100 Mbit/s download.
# - Resumable: per-tile results are saved in `work_dir` as soon as computed and skipped on rerun;
#   raw tiles are deleted once no longer needed (at most two 1° latitude bands on disk, ~4 GB).
# - Full run: ~130 GB downloaded, 1-3 h depending on bandwidth. Set `region` to test on a small
#   area first; its outputs only hold that area (the rest is 0).
#
# Publishing (after a full run, `region <- NULL`)
#   gh release create ground-elevation-v1 --repo GeoPressure/GeoPressureR \
#     --title "Ground elevation v1" --notes "Built with data-raw/ground_elevation.R" \
#     "$work_dir"/glo90-ground-*.tif

region <- NULL # c(W, E, S, N) in integer degrees, e.g. c(5, 10, 44, 48); NULL for the globe
work_dir <- file.path(tools::R_user_dir("GeoPressureR", "cache"), "ground-elevation-build")
cores <- 4 # parallel workers; more mainly competes with other programs for little gain
scales <- c(1, 2, 4, 5, 10)
probs <- c(0, seq(0.1, 0.9, 0.1), 1)
band_names <- c("min", glue::glue("p{seq(10, 90, 10)}"), "max")
data_version <- "ground-elevation-v1"
base_url <- "https://copernicus-dem-90m.s3.amazonaws.com"

dir.create(file.path(work_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(work_dir, "tile"), showWarnings = FALSE)

# Tile list, kept with the build so the run is reproducible
tile_list_file <- file.path(work_dir, "tileList.txt")
if (!file.exists(tile_list_file)) {
  httr2::request(glue::glue("{base_url}/tileList.txt")) |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |>
    httr2::req_perform(path = tile_list_file)
}
tile_name <- readLines(tile_list_file)
tile_lat <- as.integer(sub(".*_([NS])(\\d{2})_00_.*", "\\2", tile_name)) *
  ifelse(grepl("_S\\d{2}_00_", tile_name), -1L, 1L)
tile_lon <- as.integer(sub(".*_([EW])(\\d{3})_00_DEM$", "\\2", tile_name)) *
  ifelse(grepl("_W\\d{3}_00_DEM$", tile_name), -1L, 1L)
tile_key <- paste(tile_lon, tile_lat)

raw_file <- \(lo, la) {
  file.path(work_dir, "raw", glue::glue("{tile_name[match(paste(lo, la), tile_key)]}.tif"))
}

# GLO-90 longitude spacing widens with latitude; used for missing (ocean) tiles
tile_ncol <- \(la) {
  c(1200, 800, 600, 400, 240)[findInterval(min(abs(la), abs(la + 1)), c(0, 50, 60, 70, 80))]
}

# DEM of a 1° tile as a matrix (row 1 = north), zeros for ocean tiles
read_tile <- function(lo, la) {
  lo <- (lo + 180) %% 360 - 180
  if (!(paste(lo, la) %in% tile_key)) {
    return(matrix(0, 1200, tile_ncol(la)))
  }
  terra::as.matrix(terra::rast(raw_file(lo, la)), wide = TRUE)
}

# Quantiles of `v` within groups `g` (0-based), vectorised over groups (type 7)
group_quantile <- function(v, g, n_group) {
  o <- order(g, v)
  v <- v[o]
  cnt <- tabulate(g + 1L, n_group)
  start <- c(0L, cumsum(cnt)[-n_group])
  h <- outer(cnt - 1, probs) + 1
  lo <- floor(h)
  vl <- v[start + lo]
  q <- round(vl + (h - lo) * (v[start + pmin(lo + 1, cnt)] - vl))
  storage.mode(q) <- "integer"
  q
}

# Map-grid statistics of one tile: array [s rows from north, s cols from west, band] per scale
tile_stat <- function(lo, la) {
  m <- read_tile(lo, la)
  i <- rep(seq_len(nrow(m)) - 1L, ncol(m))
  j <- rep(seq_len(ncol(m)) - 1L, each = nrow(m))
  stats::setNames(
    lapply(scales, \(s) {
      g <- (i %/% (nrow(m) / s)) * s + j %/% (ncol(m) / s)
      q <- group_quantile(as.vector(m), g, s * s)
      aperm(array(q, c(s, s, length(probs))), c(2, 1, 3))
    }),
    scales
  )
}

# ERA5 statistics of the 4 x 4 cells centred on lon lo + (0, .25, .5, .75) and
# lat la + (.75, .5, .25, 0); they straddle tiles (lo - 1, lo) x (la - 1, la)
era5_stat <- function(lo, la) {
  parts <- list(
    list(lo - 1, la, 151:1200, "west"),
    list(lo, la, 151:1200, "east"),
    list(lo - 1, la - 1, 1:150, "west"),
    list(lo, la - 1, 1:150, "east")
  )
  vg <- lapply(parts, \(p) {
    if (p[[2]] < -90 || p[[2]] > 89) {
      return(NULL)
    }
    m <- read_tile(p[[1]], p[[2]])
    n <- ncol(m)
    col <- if (p[[4]] == "west") seq(0.875 * n + 1, n) else seq_len(0.875 * n)
    r <- if (p[[2]] == la) (p[[3]] - 151) %/% 300 else rep(3L, length(p[[3]]))
    c <- if (p[[4]] == "west") rep(0L, length(col)) else (col - 1 + 0.125 * n) %/% (0.25 * n)
    list(
      v = as.vector(m[p[[3]], col]),
      g = rep(r, length(col)) * 4L + rep(c, each = length(p[[3]]))
    )
  })
  vg <- vg[!vapply(vg, is.null, logical(1))]
  q <- group_quantile(unlist(lapply(vg, `[[`, "v")), unlist(lapply(vg, `[[`, "g")), 16)
  aperm(array(q, c(4, 4, length(probs))), c(2, 1, 3))
}

# Download the given tiles in parallel, skipping those already on disk
download_tile <- function(lo, la) {
  f <- raw_file(lo, la)
  todo <- !file.exists(f)
  if (!any(todo)) {
    return(invisible())
  }
  reqs <- lapply(basename(f[todo]), \(n) {
    httr2::request(glue::glue("{base_url}/{sub('.tif$', '', n)}/{n}")) |>
      httr2::req_retry(max_tries = 5, retry_on_failure = TRUE)
  })
  part <- glue::glue("{f[todo]}.part")
  resp <- httr2::req_perform_parallel(reqs, paths = part, on_error = "continue", progress = FALSE)
  ok <- vapply(resp, inherits, logical(1), "httr2_response")
  file.rename(part[ok], f[todo][ok])
  if (!all(ok)) {
    cli::cli_abort(
      "Failed to download {.file {basename(f[todo][!ok])}}; rerun the script to resume."
    )
  }
}

out_file <- \(type, lo, la) file.path(work_dir, "tile", glue::glue("{type}_{lo}_{la}.rds"))

# Write via a temporary file so an interrupted run never leaves a truncated result behind
save_out <- function(x, file) {
  saveRDS(x, glue::glue("{file}.tmp"))
  file.rename(glue::glue("{file}.tmp"), file)
}

# Process latitude bands south to north. Band `la` computes the map-grid statistics of its tiles
# and the ERA5 cells centred on lat la to la + 0.75, which also need band la - 1.
lon_range <- if (is.null(region)) c(-180, 179) else c(region[1], region[2] - 1)
lat_range <- if (is.null(region)) c(-90, 90) else c(region[3], region[4] - 1)
for (la in seq(lat_range[1], lat_range[2])) {
  lon <- seq(lon_range[1], lon_range[2])
  map_todo <- lon[paste(lon, la) %in% tile_key & !file.exists(out_file("map", lon, la))]
  # ERA5 chunks touching at least one land tile
  land <- \(lo, l) paste((lo + 180) %% 360 - 180, l) %in% tile_key
  era5_lon <- lon[land(lon, la) | land(lon - 1, la) | land(lon, la - 1) | land(lon - 1, la - 1)]
  era5_todo <- era5_lon[!file.exists(out_file("era5", era5_lon, la))]

  if (length(map_todo) + length(era5_todo) > 0) {
    cli::cli_progress_step(
      "Lat {la}: {length(map_todo)} tile{?s}, {length(era5_todo)} ERA5 chunk{?s}"
    )
    need <- unique(rbind(
      data.frame(lo = map_todo, la = rep(la, length(map_todo))),
      expand.grid(lo = c(era5_todo, era5_todo - 1), la = c(la - 1, la))
    ))
    need$lo <- (need$lo + 180) %% 360 - 180
    need <- need[paste(need$lo, need$la) %in% tile_key, ]
    download_tile(need$lo, need$la)

    parallel::mclapply(
      map_todo,
      \(lo) save_out(tile_stat(lo, la), out_file("map", lo, la)),
      mc.cores = cores
    )
    parallel::mclapply(
      era5_todo,
      \(lo) save_out(era5_stat(lo, la), out_file("era5", lo, la)),
      mc.cores = cores
    )
    missing <- c(
      map_todo[!file.exists(out_file("map", map_todo, la))],
      era5_todo[!file.exists(out_file("era5", era5_todo, la))]
    )
    if (length(missing) > 0) {
      cli::cli_abort("Lat {la}: no output for lon {missing}; rerun the script to resume.")
    }
  }
  # Band la - 1 is no longer needed by any later band
  unlink(list.files(
    file.path(work_dir, "raw"),
    glue::glue("_{if (la - 1 < 0) 'S' else 'N'}{sprintf('%02d', abs(la - 1))}_00_"),
    full.names = TRUE
  ))
}

unlink(file.path(work_dir, "raw"), recursive = TRUE)

# Assemble the global rasters
write_stat <- function(value, ext, file) {
  r <- terra::rast(
    nrows = dim(value)[1],
    ncols = dim(value)[2],
    nlyrs = length(probs),
    ext = terra::ext(ext),
    crs = "EPSG:4326"
  )
  terra::values(r) <- matrix(aperm(value, c(2, 1, 3)), ncol = length(probs))
  names(r) <- band_names
  terra::metags(r) <- c(
    source = "Copernicus DEM GLO-90 (s3://copernicus-dem-90m)",
    statistic = "min, percentiles 10-90 (quantile type 7) and max of DEM pixels within each cell",
    unit = "m above EGM2008 geoid",
    version = data_version,
    script = "https://github.com/GeoPressure/GeoPressureR/blob/main/data-raw/ground_elevation.R",
    created = format(Sys.Date())
  )
  terra::writeRaster(
    r,
    file,
    filetype = "COG",
    datatype = "INT2S",
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
  )
}

for (s in scales) {
  cli::cli_progress_step("Write map grid scale {s}")
  value <- array(0L, c(180 * s, 360 * s, length(probs)))
  for (f in list.files(file.path(work_dir, "tile"), "^map_", full.names = TRUE)) {
    ll <- as.integer(strsplit(sub("^map_(.*)\\.rds$", "\\1", basename(f)), "_")[[1]])
    value[(89 - ll[2]) * s + seq_len(s), (ll[1] + 180) * s + seq_len(s), ] <- readRDS(
      f
    )[[as.character(s)]]
  }
  write_stat(
    value,
    c(-180, 180, -90, 90),
    file.path(work_dir, glue::glue("glo90-ground-scale{s}.tif"))
  )
}

cli::cli_progress_step("Write ERA5 0.25° grid")
value <- array(0L, c(721, 1440, length(probs)))
for (f in list.files(file.path(work_dir, "tile"), "^era5_", full.names = TRUE)) {
  ll <- as.integer(strsplit(sub("^era5_(.*)\\.rds$", "\\1", basename(f)), "_")[[1]])
  row <- (89 - ll[2]) * 4 + 2:5 # cells centred on lat la + 0.75, ..., la
  col <- (ll[1] + 180) * 4 + 1:4
  keep <- row >= 1 & row <= 721
  value[row[keep], col, ] <- readRDS(f)[keep, , , drop = FALSE]
}
write_stat(
  value,
  c(-180.125, 179.875, -90.125, 90.125),
  file.path(work_dir, "glo90-ground-era5-0.25.tif")
)
