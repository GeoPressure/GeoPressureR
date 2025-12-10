library(testthat)
library(GeoPressureR)

# Set working directory to access package data
test_with_extdata()

tag <- tag_create("18LX")
tag <- tag_label(tag)
expect_error(geopressureviz(tag))
tag <- tag_set_map(
  tag,
  extent = c(-16, 23, 0, 50),
  scale = 2,
  known = data.frame(
    stap_id = 1,
    known_lon = 17.05,
    known_lat = 48.9
  )
)
expect_error(geopressureviz(tag))

# Pressure
tag <- geopressure_map_mismatch(tag)
geopressureviz(tag)

tag <- geopressure_map_likelihood(tag)
geopressureviz(tag)

# With light
tag <- twilight_create(tag)
tag <- twilight_label_read(tag)
tag <- geolight_map(tag)
geopressureviz(tag)
