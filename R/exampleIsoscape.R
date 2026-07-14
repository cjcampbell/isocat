#' Example isoscape data
#'
#'
#' @format A data.frame with 22500 rows (\code{x}, \code{y}, and a \code{d2h_GS} layer) on
#'   a square 150 x 150 equal-area (Albers) grid; edge cells outside the projected region
#'   are \code{NA}. Coordinates are in metres; the CRS is carried in a \code{"crs"}
#'   attribute, re-applied when rebuilding the raster.
#'
#' @references Campbell C. J. (2018) IsoMAP job 66100, Isoscapes Modeling, Analysis and Prediction (version 1.0). The IsoMAP Project. http://isomap.org
#' @references Bowen G. J., West J.B., Miller C. C., Zhao L. and Zhang T. (2018) IsoMAP: Isoscapes Modeling, Analysis and Prediction (version 1.0). IsoMAP job 66100, Caitlin J. Campbell. Isoscapes Modeling, Analysis and Prediction (version 1.0). The IsoMAP Project. http://isomap.org
#'
#'@examples
#'iso <- rast(isoscape, type = "xyz", crs = attr(isoscape, "crs"))

"isoscape"
