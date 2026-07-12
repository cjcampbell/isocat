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

# NA-containing versions, exercising NA handling in the normalization and
# similarity code. The bundled isoscape is a complete grid (which hid a family of
# missing-na.rm bugs), so a masked study region is simulated by setting cells NA.
example_isoscape_na <- function(mask_n = 200) {
  r <- example_isoscape()
  r[seq_len(mask_n)] <- NA
  r
}

example_assignment_na <- function(ids = c("A", "B", "C"),
                                  values = c(-100, -80, -50),
                                  sd_indv = 5, mask_n = 200) {
  iso    <- example_isoscape()
  iso_sd <- example_isoscape_sd()
  iso[seq_len(mask_n)]    <- NA
  iso_sd[seq_len(mask_n)] <- NA
  isotopeAssignmentModel(
    ID = ids, isotopeValue = values, SD_indv = rep(sd_indv, length(ids)),
    precip_raster = iso, precip_SD_raster = iso_sd
  )
}
