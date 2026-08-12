# Local cache shared by GeoPressureViz and Trainset processes

# @noRd
pressure_query_cache_dir <- function() {
  getOption(
    "GeoPressureR.pressure_query_cache_dir",
    file.path(tools::R_user_dir("GeoPressureR", "cache"), "pressure-query")
  )
}

# @noRd
pressure_query_cache_write <- function(bundle) {
  bundle$version <- 1L
  bundle$cached_at <- as.POSIXct(Sys.time(), tz = "UTC")

  tag_dir <- file.path(
    pressure_query_cache_dir(),
    gsub("[^[:alnum:]_-]", "_", bundle$tag_id)
  )
  dir.create(tag_dir, recursive = TRUE, showWarnings = FALSE)

  query_time <- format(bundle$requested_at, "%Y%m%dT%H%M%OS6Z", tz = "UTC")
  query_time <- gsub("[^[:alnum:]]", "", query_time)
  file <- file.path(
    tag_dir,
    glue::glue(
      "{query_time}_lat_{sprintf('%+.5f', bundle$requested_lat)}_lon_{sprintf('%+.5f', bundle$requested_lon)}.rds"
    )
  )
  saveRDS(bundle, file)
  file
}

# @noRd
pressure_query_cache_read <- function(tag_id, dates) {
  tag_dir <- file.path(
    pressure_query_cache_dir(),
    gsub("[^[:alnum:]_-]", "_", tag_id)
  )
  files <- list.files(tag_dir, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) {
    return(list())
  }

  bundles <- lapply(files, readRDS)
  keep <- vapply(bundles, function(x) {
    identical(x$version, 1L) &&
      identical(x$tag_id, tag_id) &&
      identical(as.numeric(x$date), as.numeric(dates))
  }, logical(1))
  bundles <- bundles[keep]
  bundles[order(vapply(bundles, function(x) as.numeric(x$requested_at), numeric(1)))]
}
