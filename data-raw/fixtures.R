# Precomputed 18LX workflow outputs shared by examples, offline tests and the pkgdown site.
#
# Runs the full workflow on the trimmed 18LX tag of inst/extdata (requires GeoPressureAPI access)
# and writes inst/extdata/data/interim/18LX.RData (tag with pressure and light likelihood maps,
# marginal, most likely and simulated paths, and the most likely pressurepath).
# Re-run after any change to the structure of these objects.
pkgload::load_all()

withr::with_dir("inst/extdata", {
  geopressuretemplate("18LX", tag_set_map = list(scale = 2), quiet = TRUE)
})
