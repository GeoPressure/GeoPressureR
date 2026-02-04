# Load internal datasets into namespace
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
