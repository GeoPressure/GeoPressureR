#' @importFrom ggplot2 .data
NULL

#' @importMethodsFrom terra rast
NULL

# setOldClass("map") allows S4 dispatch on S3 map objects.
#' @importFrom methods setOldClass
methods::setOldClass("map")

#' @importFrom methods setMethod
if (methods::isGeneric("rast") && exists("rast.map", mode = "function")) {
  try(methods::setMethod("rast", "map", rast.map), silent = TRUE)
}

#' @noRd
.onLoad <- function(libname, pkgname) {
  data_dir <- system.file("extdata/internal", package = pkgname)
  pkg_env <- asNamespace(pkgname)
  assign(
    "pressurepath_variable",
    readRDS(file.path(data_dir, "pressurepath_variable.rds")),
    envir = pkg_env
  )
  assign("avonet", readRDS(file.path(data_dir, "avonet.rds")), envir = pkg_env)
}
