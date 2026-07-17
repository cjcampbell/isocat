# Code for making reproducible, small example isoscape data.
#
# The source IsoMAP rasters are geographic (WGS84 lon/lat) with no CRS tag. We tag the
# CRS, project onto a SQUARE, FULLY-FILLED equal-area (Albers) grid, and record the CRS
# as a `crs` attribute on each exported data.frame so downstream code can rebuild a
# CRS-aware raster with
#   rast(x, type = "xyz", crs = attr(x, "crs"))
# terra's xyz round-trip drops the CRS, so it must travel as an attribute.
#
# Projecting a lon/lat rectangle into an equal-area projection yields a curved/tilted
# block, not a rectangle. To get a square grid of real data (no NA edges), we (1) centre
# the Albers projection on the data region so the block sits straight, then (2) crop to
# the largest centred square that lies entirely within the data before resampling.

library(terra)

iso_crs   <- "EPSG:4326"                  # WGS84 lon/lat: the IsoMAP source projection
my_extent <- ext(-110, -95, 35, 45)       # lon/lat region to crop before projecting
n_side    <- 150                          # square output grid: n_side x n_side cells
# Albers equal-area centred on the data region (lon_0 = data centre) so the projected
# block is symmetric rather than leaning.
aea_crs   <- "+proj=aea +lat_0=40 +lon_0=-102.5 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"

# Largest centred square fully inside the projected data (no NA), from a reference layer.
ref  <- rast("~/isocat/setup_data/GlobalPrecipGS/d2h_GS.tif")
crs(ref) <- iso_crs
ref  <- project(crop(ref, my_extent), aea_crs)
e    <- as.vector(ext(trim(ref)))                       # bounding box of non-NA data
cx   <- mean(e[1:2]); cy <- mean(e[3:4])
half <- 0.5 * min(diff(e[1:2]), diff(e[3:4]))
while (terra::global(is.na(crop(ref, ext(cx - half, cx + half, cy - half, cy + half))),
                     "sum")[1, 1] > 0) {
  half <- half * 0.98                                   # shrink until the square is filled
}
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
