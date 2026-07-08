# Shared fixtures for the test suite.
#
# The bundled `isoscape` / `isoscape_sd` datasets are stored as xyz data.frames;
# every test that needs a raster builds one from them via terra so the suite has
# no external data dependency.

example_isoscape <- function() {
  terra::rast(isocat::isoscape, type = "xyz")
}

example_isoscape_sd <- function() {
  terra::rast(isocat::isoscape_sd, type = "xyz")
}

# A small multi-layer assignment SpatRaster reused across tests.
example_assignment <- function(ids = c("A", "B", "C"),
                               values = c(-100, -80, -50),
                               sd_indv = 5) {
  isotopeAssignmentModel(
    ID = ids,
    isotopeValue = values,
    SD_indv = rep(sd_indv, length(ids)),
    precip_raster = example_isoscape(),
    precip_SD_raster = example_isoscape_sd()
  )
}
