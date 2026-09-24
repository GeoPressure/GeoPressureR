#' List the ERA5 variables a backend and product can return
#'
#' The variables available to [pressurepath_create()] depend on **both** the backend and the ERA5
#' product, and the sets genuinely differ. GeoPressureAPI serves whatever bands the underlying
#' Earth Engine collection carries: 292 for ERA5 single levels against 70 for ERA5-Land, with only
#' 44 in common. `era5_dataset = "both"` mosaics ERA5-Land on top of ERA5 but returns ERA5's band
#' namespace, so its set equals `"single-levels"` and ERA5-Land-only bands are dropped. The ARCO
#' backend reads ECMWF's zarr stores directly and currently maps only surface pressure and 2 m
#' temperature, so it can return `"surface_pressure"` and the derived `"altitude"`.
#'
#' Requesting a variable the chosen product does not have used to fail silently: GeoPressureAPI
#' returned `200 success` with every array empty — not just the missing one — which arrived as a
#' `pressurepath` whose ERA5 columns were all `NA`. [pressurepath_create()] now validates the
#' request up front with this list.
#'
#' @param source Backend: `"api"` or `"arco"`.
#' @param era5_dataset ERA5 product: `"single-levels"`, `"land"`, or `"both"`.
#'
#' @return A character vector of variable names accepted by `variable`, including `"altitude"`.
#'
#' @examples
#' length(pressurepath_variable_available("api", "single-levels"))
#' length(pressurepath_variable_available("api", "land"))
#' pressurepath_variable_available("arco", "land")
#'
#' @family pressurepath
#' @export
pressurepath_variable_available <- function(
  source = c("api", "arco"),
  era5_dataset = c("single-levels", "land", "both")
) {
  source <- match.arg(source)
  era5_dataset <- match.arg(era5_dataset)
  # ARCO derives its list from the store map that drives the reader, so the two cannot drift.
  available <- if (source == "arco") {
    era5_arco_variable(era5_dataset)
  } else {
    pressurepath_variable[[source]][[era5_dataset]]
  }
  # `altitude` is derived from surface pressure, temperature and geopotential rather than read as
  # a band, and every backend/product combination can produce it.
  sort(unique(c("altitude", available)))
}

#' Validate requested variables against the resolved backend and product
#'
#' @param variable Character vector of requested variables.
#' @param source Resolved backend, `"api"` or `"arco"`.
#' @param era5_dataset Resolved ERA5 product.
#' @return Invisibly `variable`, or an error naming the configuration that would work.
#' @noRd
pressurepath_variable_check <- function(variable, source, era5_dataset) {
  available <- pressurepath_variable_available(source, era5_dataset)
  unavailable <- setdiff(variable, available)
  if (length(unavailable) == 0) {
    return(invisible(variable))
  }

  sources <- c("api", "arco")
  datasets <- c("single-levels", "land", "both")
  known <- unique(unlist(lapply(
    sources,
    function(s) unlist(lapply(datasets, function(d) pressurepath_variable_available(s, d)))
  )))

  bullets <- vapply(
    unavailable,
    function(v) {
      # Prefer a fix that only changes era5_dataset, then one that changes source.
      same_source <- datasets[vapply(
        datasets,
        function(d) v %in% pressurepath_variable_available(source, d),
        logical(1)
      )]
      if (length(same_source) > 0) {
        return(cli::format_inline(
          "{.val {v}}: available with {.code era5_dataset = {.str {same_source[1]}}}."
        ))
      }
      other_source <- setdiff(sources, source)
      if (v %in% unlist(lapply(other_source, pressurepath_variable_available, era5_dataset))) {
        return(cli::format_inline(
          "{.val {v}}: available with {.code source = {.str {other_source[1]}}}."
        ))
      }
      if (v %in% known) {
        return(cli::format_inline("{.val {v}}: not available from this backend."))
      }
      near <- known[utils::adist(v, known) <= 3]
      if (length(near) > 0) {
        return(cli::format_inline("{.val {v}}: unknown variable. Did you mean {.val {near[1]}}?"))
      }
      cli::format_inline("{.val {v}}: unknown ERA5 variable.")
    },
    character(1)
  )

  cli::cli_abort(c(
    "{cli::qty(length(unavailable))}Variable{?s} {.val {unavailable}} {?is/are} not available \\
     with {.code source = {.str {source}}} and {.code era5_dataset = {.str {era5_dataset}}}.",
    stats::setNames(bullets, rep("x", length(bullets))),
    "i" = "See {.fn pressurepath_variable_available} for the full list \\
           ({length(available)} variable{?s} for this configuration)."
  ))
}
