#' Competitive distance weight for candidate origins
#'
#' For each candidate origin cell, the isotope-weighted probability that an equally- or
#' more-suitable location lies at least as far from the capture site:
#' \eqn{w(x) = P(d(Y) \ge d(x) \mid iso(Y) \ge iso(x))}, with \eqn{Y \sim iso}. Multiply
#' by \code{iso} and normalize to obtain a competitively reweighted origin surface (see
#' \code{\link{competitiveSurface}}). Equivalently, over the grid,
#' \deqn{w(x) = \frac{\sum_{y:\, iso(y) \ge iso(x),\, d(y) \ge d(x)} iso(y)}{\sum_{y:\, iso(y) \ge iso(x)} iso(y)}.}
#'
#' The weight is a within-suitability distance survival function in \eqn{[0, 1]}: the
#' most-suitable cell always has \eqn{w = 1} (it is never discounted, wherever it is), and
#' a cell is penalized only by locations that are both more suitable and closer to capture.
#' It uses only the \emph{ordering} of distances, so it needs no movement scale and is
#' invariant to the absolute distance scale (see \code{\link{movementKernel}} for the
#' movement-prior alternative).
#'
#' The two nested sums are evaluated on an \code{nbins} x \code{nbins} histogram of
#' (\code{iso}, \code{d}) via reverse-cumulative sums, so the cost is \eqn{O(nbins^2)}
#' rather than \eqn{O(n^2)}; \code{nbins = 200} is close to the exact definition on
#' continental grids. Higher \code{nbins} is closer to exact.
#'
#' @param iso Numeric vector of isotopic suitabilities (\eqn{\ge 0}) per cell.
#' @param d Numeric vector of distances from each cell to the capture site, the same
#'   length as \code{iso}.
#' @param nbins Resolution of the (iso, distance) binning. Defaults to 200.
#'
#' @return Numeric vector of weights in \eqn{[0, 1]}, the same length as \code{iso},
#'   \code{NA} where \code{iso} is \code{NA} or \eqn{\le 0} or \code{d} is \code{NA}.
#'
#' @seealso \code{\link{competitiveSurface}}, \code{\link{movementKernel}}
#'
#' @examples
#' # Along a line: cell 3 is most suitable and gets w = 1; a distant low-suitability
#' # cell is discounted because better, nearer alternatives exist.
#' iso <- c(0.2, 0.6, 1.0, 0.5, 0.1)
#' d   <- c(0, 1, 2, 3, 4)
#' competitiveWeight(iso, d, nbins = 50)
#'
#' @export
competitiveWeight <- function(iso, d, nbins = 200){
  if(!is.numeric(iso) || !is.numeric(d))
    stop("'iso' and 'd' must be numeric.")
  if(length(iso) != length(d))
    stop("'iso' and 'd' must have the same length.")
  if(!is.numeric(nbins) || length(nbins) != 1 || nbins < 1)
    stop("'nbins' must be a single positive number.")
  nbins <- as.integer(nbins)

  w  <- rep(NA_real_, length(iso))
  ok <- is.finite(iso) & is.finite(d) & iso > 0
  if(!any(ok)) return(w)

  a <- pmin(nbins, pmax(1L, as.integer(ceiling(iso[ok] / max(iso[ok]) * nbins))))
  b <- pmin(nbins, pmax(1L, as.integer(ceiling((d[ok] - min(d[ok])) /
                                               (diff(range(d[ok])) + 1e-9) * nbins))))
  H  <- matrix(0, nbins, nbins)
  Hv <- tapply(iso[ok], (b - 1L) * nbins + a, sum)
  H[as.integer(names(Hv))] <- Hv
  Sa <- apply(H,  2, function(col) rev(cumsum(rev(col))))        # sum over iso-bins >= a
  S2 <- t(apply(Sa, 1, function(row) rev(cumsum(rev(row)))))     # then over distance-bins >= b
  w[ok] <- S2[cbind(a, b)] / S2[, 1][a]
  w
}


#' Competitively reweighted probability-of-origin surface
#'
#' Combines a probability-of-origin surface with the individual's capture location so that
#' isotopic suitability stays the primary driver, while a candidate origin is discounted
#' only when a closer, equally- or more-suitable origin exists. The returned surface is
#' \eqn{p(x) \propto iso(x)\, w(x)}, where \eqn{w} is the competitive distance weight
#' (\code{\link{competitiveWeight}}). Unlike the half-normal \code{\link{movementKernel}}
#' it needs no movement-scale parameter and is invariant to the absolute distance scale;
#' the most-suitable cell is retained wherever it is, so genuine long-distance origins are
#' not annihilated.
#'
#' This is the recommended way to fold a capture location into the origin-summary workflow:
#' reweight the surface once here, then draw points from the reweighted surface with
#' \code{\link{sampleOriginPoints}} and summarize them unweighted with
#' \code{\link{summarizeDistribution}} (the distance discounting is already in the surface).
#' The half-normal \code{movementKernel} weighting remains available as an alternative
#' generative movement prior.
#'
#' Distances are Euclidean in the surface's map units, so the surface should be in a
#' projected (equal-area) CRS with \code{xy} in the same CRS; a warning is issued for a
#' geographic (lon/lat) surface. The weight uses only distance ordering, but the distances
#' must be metric.
#'
#' @param surface A \code{SpatRaster} of one or more probability-of-origin layers
#'   (e.g. output of \code{\link{isotopeAssignmentModel}}). Layer names are used as
#'   individual IDs. Legacy \code{Raster*} objects are coerced.
#' @param xy Capture location(s). Either a length-2 numeric \code{c(x, y)} applied to
#'   every layer, or a \code{data.frame} with columns \code{ID}, \code{x}, and \code{y}
#'   keyed to layer names. Coordinates must be in the surface's CRS.
#' @param nbins Binning resolution passed to \code{\link{competitiveWeight}}. Defaults
#'   to 200.
#' @param normalize If \code{TRUE} (default) each returned layer is scaled to sum to 1
#'   over its non-\code{NA} cells.
#'
#' @return A \code{SpatRaster} with the same geometry and layer names as \code{surface},
#'   holding the competitively reweighted surface(s). Zero-suitability cells stay 0;
#'   \code{NA} cells stay \code{NA}.
#'
#' @seealso \code{\link{competitiveWeight}}, \code{\link{sampleOriginPoints}},
#'   \code{\link{summarizeDistribution}}, \code{\link{movementKernel}}
#'
#' @importFrom methods is
#'
#' @examples
#' # The bundled isoscape is already on an equal-area grid (distances in metres).
#' myiso    <- rast(isoscape,    type = "xyz", crs = attr(isoscape,    "crs"))
#' myiso_sd <- rast(isoscape_sd, type = "xyz", crs = attr(isoscape_sd, "crs"))
#' surfaces <- isotopeAssignmentModel(
#'         ID = c("A", "B"), isotopeValue = c(-100, -80), SD_indv = c(5, 5),
#'         precip_raster = myiso, precip_SD_raster = myiso_sd, nClusters = FALSE
#'         )
#'
#' # Capture site at the centre of the study extent.
#' e    <- as.vector(terra::ext(surfaces))
#' site <- c(x = mean(e[1:2]), y = mean(e[3:4]))
#'
#' # Competitively reweight, then sample and summarize as usual.
#' psurf <- competitiveSurface(surfaces, xy = site)
#' set.seed(1)
#' pts <- sampleOriginPoints(psurf, n = 500, xy = site)
#'
#' @export
competitiveSurface <- function(surface, xy, nbins = 200, normalize = TRUE){

  if(is(surface, "Raster")) surface <- terra::rast(surface)
  if(!is(surface, "SpatRaster"))
    stop("'surface' must be a SpatRaster.")
  if(missing(xy) || is.null(xy))
    stop("'xy' (capture location) is required for competitive reweighting.")

  ids <- names(surface)
  ref <- .parseRefXY(xy, ids)

  # Distances are Euclidean in map units, so they are only meaningful on a projected
  # (equal-area) surface.
  if(isTRUE(terra::is.lonlat(surface, perhaps = TRUE, warn = FALSE)))
    warning("'surface' appears to use geographic (lon/lat) coordinates; competitive ",
            "reweighting uses Euclidean distances that are not true distances on a ",
            "lon/lat grid. Project 'surface' to an equal-area CRS before reweighting.")

  cellxy <- terra::xyFromCell(surface, seq_len(terra::ncell(surface)))
  vals   <- terra::values(surface)                                  # ncell x nlyr
  newv   <- vals

  for(k in seq_len(terra::nlyr(surface))){
    iso <- vals[, k]
    r   <- ref[ids[k], ]
    d   <- sqrt((cellxy[, 1] - r[["x"]])^2 + (cellxy[, 2] - r[["y"]])^2)
    p   <- iso * competitiveWeight(iso, d, nbins = nbins)
    p[!is.na(iso) & iso == 0] <- 0                # zero-suitability cells stay 0, not NA
    if(normalize){
      s <- sum(p, na.rm = TRUE)
      if(is.finite(s) && s > 0) p <- p / s
    }
    newv[, k] <- p
  }

  out <- surface
  terra::values(out) <- newv
  names(out) <- ids
  out
}
