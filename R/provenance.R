# Provenance: which code and data produced a result. Every step records the same fields, in
# `param$provenance$<step>` for a `tag` or `graph`, and in `attr(x, "provenance")` for other
# outputs (timeseries, pressurepath). `api` is the list of the unique `provenance` objects returned
# by GeoPressureAPI, kept as received (NULL for other sources and for older API versions).

# @noRd
provenance_create <- function(source, era5_dataset = NULL, altitude_formula = NULL, api = NULL) {
  list(
    GeoPressureR = as.character(utils::packageVersion("GeoPressureR")),
    date = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source = source,
    era5_dataset = era5_dataset,
    altitude_formula = altitude_formula,
    api = api
  )
}

# Combine the provenance of several requests of one step, keeping the unique API objects
# @noRd
provenance_merge <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) {
    return(NULL)
  }
  out <- x[[1]]
  out["api"] <- list(unique(unlist(lapply(x, \(p) p$api), recursive = FALSE)))
  out
}

# Short label of a GeoPressureAPI provenance object: the commit, or the source hash without one
# @noRd
provenance_api_label <- function(api) {
  code <- if (is.null(api$code)) api else api$code
  label <- if (is.null(code$commit)) code$sourceHash else code$commit
  if (is.null(label)) "unknown" else as.character(label)
}

# GeoPressureAPI version of a failed request, from its `X-GeoPressure-Version` header, as a bullet
# for the error message. NULL when the header is missing or not JSON (older API versions).
# @noRd
provenance_api_error <- function(resp) {
  header <- httr2::resp_header(resp, "X-GeoPressure-Version")
  if (is.null(header) || !jsonlite::validate(header)) {
    return(NULL)
  }
  c(
    "i" = glue::glue("GeoPressureAPI version: {provenance_api_label(jsonlite::parse_json(header))}")
  )
}
