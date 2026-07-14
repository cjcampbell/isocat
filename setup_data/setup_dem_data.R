# Reproducible example DEM aligned to the example isoscape grid.
#
# Builds `example_dem`: elevation (m) over the same extent and grid as the bundled
# `isoscape`, shipped as an xyz data.frame (used via `rast(example_dem, type = "xyz")`).
#
# Source: ETOPO 2022 15 Arc-Second Global Relief Model (NOAA NCEI), public domain
# (CC0-1.0), DOI 10.25921/fd45-gt74. A regional subset is downloaded once here from the
# NOAA CoastWatch ERDDAP server (griddap, NetCDF), then aggregated/resampled onto the
# isoscape grid. ERDDAP access is DEV-ONLY: setup_data/ is .Rbuildignore'd, so nothing
# here is a package dependency. Run this script manually to (re)generate
# data/example_dem.rda.

library(terra)

# Template = the exact bundled isoscape grid (square, equal-area Albers), so the DEM
# aligns cell-for-cell.
load("~/isocat/data/isoscape.rda")                 # brings `isoscape` into scope
template <- rast(isoscape, type = "xyz", crs = attr(isoscape, "crs"))

# ETOPO 2022 (variable 'z', metres) regional subset via ERDDAP griddap. The [start:stride:
# stop] stride of 5 fetches ~75 arc-second data over the region (small download); it is
# then averaged down to the ~5 arc-minute isoscape grid.
erddap <- paste0(
  "https://coastwatch.pfeg.noaa.gov/erddap/griddap/ETOPO_2022_v1_15s.nc",
  "?z%5B(35.0):5:(45.0)%5D%5B(-110.0):5:(-95.0)%5D"
)
tmp <- tempfile(fileext = ".nc")
download.file(erddap, tmp, mode = "wb", method = "libcurl")

etopo <- rast(tmp)                                  # ETOPO subset, ~75 arc-second
crs(etopo) <- "EPSG:4326"
etopo <- aggregate(etopo, fact = 4, fun = "mean", na.rm = TRUE)   # coarsen before projecting
elev  <- project(etopo, template, method = "bilinear")            # onto the square Albers grid

example_dem <- as.data.frame(elev, xy = TRUE, na.rm = FALSE)      # keep the full square grid
names(example_dem) <- c("x", "y", "elevation")
attr(example_dem, "crs") <- attr(isoscape, "crs")   # ships the (Albers) CRS with the data

usethis::use_data(example_dem, overwrite = TRUE)

message(sprintf("example_dem: %d rows, elevation range %.0f-%.0f m",
                nrow(example_dem),
                min(example_dem$elevation, na.rm = TRUE),
                max(example_dem$elevation, na.rm = TRUE)))
