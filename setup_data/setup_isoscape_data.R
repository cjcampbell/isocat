# Code for making reproducible, small example isoscape data.
#
# The source IsoMAP rasters are geographic (WGS84 lon/lat) with no CRS tag. We tag the
# CRS, project onto a SQUARE equal-area (Albers) grid, and record the CRS as a `crs`
# attribute on each exported data.frame so downstream code can rebuild a CRS-aware raster
# with
#   rast(x, type = "xyz", crs = attr(x, "crs"))
# terra's xyz round-trip drops the CRS, so it must travel as an attribute and be
# re-applied on rebuild. The equal-area projection leaves NA cells at the corners/edges,
# which are retained (na.rm = FALSE) so the grid stays exactly n_side x n_side.

library(terra)

iso_crs   <- "EPSG:4326"                  # WGS84 lon/lat: the IsoMAP source projection
my_extent <- ext(-110, -95, 35, 45)       # lon/lat region to crop before projecting
n_side    <- 150                          # square output grid: n_side x n_side cells
aea_crs   <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"

# Square equal-area template: project the lon/lat region to Albers, then take the centred
# bounding square (side = the larger projected dimension) at n_side x n_side square cells.
pe       <- as.vector(ext(project(rast(my_extent, resolution = 0.1, crs = iso_crs), aea_crs)))
cx       <- mean(pe[1:2]); cy <- mean(pe[3:4])
half     <- max(diff(pe[1:2]), diff(pe[3:4])) / 2
template <- rast(ext(cx - half, cx + half, cy - half, cy + half),
                 ncol = n_side, nrow = n_side, crs = aea_crs)

# Read a source raster, tag its CRS, crop, project onto the square grid, return an xyz
# data.frame carrying the (Albers) CRS.
prepRast <- function(isoscape_file_location, extension) {
  r <- rast(file.path(isoscape_file_location, extension))
  crs(r) <- iso_crs
  r <- project(crop(r, my_extent), template)
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  attr(df, "crs") <- aea_crs
  df
}

# Same, for a multi-month stack (prefix e.g. "d2h" or "d2h_se").
prepMonthly <- function(prefix, months = 7:9) {
  r <- rast(lapply(months, function(m)
    rast(file.path("~/isocat/setup_data/GlobalPrecip", sprintf("%s_%02d.tif", prefix, m)))))
  crs(r) <- iso_crs
  r <- project(crop(r, my_extent), template)
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  attr(df, "crs") <- aea_crs
  df
}

# growing season (main example)
isoscape    <- prepRast("~/isocat/setup_data/GlobalPrecipGS", "d2h_GS.tif")
isoscape_sd <- prepRast("~/isocat/setup_data/GlobalPrecipGS", "d2h_se_GS.tif")

# monthly (July-September)
iso_monthly    <- prepMonthly("d2h",    7:9)
iso_monthly_se <- prepMonthly("d2h_se", 7:9)

usethis::use_data(isoscape, isoscape_sd, iso_monthly, iso_monthly_se, overwrite = TRUE)
