#' Half-normal movement-prior kernel
#'
#' Half-normal movement-prior weight for a distance \code{d} at movement scale \code{L}:
#' \eqn{\pi(d) = \exp(-0.5 (d / L)^2)}. \code{L = Inf} returns a flat weight of 1 (no
#' movement weighting). \code{d} and \code{L} must share units. Used by
#' \code{\link{summarizeDistribution}} to weight sampled origin points by their distance
#' from a reference location.
#'
#' @param d Numeric vector of distances (in the surface's map units).
#' @param L Single numeric movement scale, in the same units as \code{d}, or \code{Inf}
#'   for a flat (unweighted) prior.
#'
#' @return Numeric vector of weights, the same length as \code{d}.
#'
#' @examples
#' movementKernel(c(0, 100, 200), L = 150)
#' movementKernel(c(0, 100, 200), L = Inf)
#'
#' @export
movementKernel <- function(d, L){
  if(!is.numeric(d))
    stop("'d' must be numeric.")
  if(!is.numeric(L) || length(L) != 1)
    stop("'L' must be a single numeric value (or Inf).")
  if(is.infinite(L)) return(rep(1, length(d)))
  if(L <= 0)
    stop("'L' must be positive, or Inf for a flat prior.")
  exp(-0.5 * (d / L)^2)
}


#' Weighted quantiles
#'
#' Internal helper. Computes weighted quantiles of \code{x} using cumulative-weight
#' plotting positions and linear interpolation. \code{NA} values in \code{x} or \code{w}
#' are dropped pairwise.
#'
#' @param x Numeric vector of values.
#' @param w Numeric vector of non-negative weights, the same length as \code{x}.
#' @param probs Numeric vector of probabilities in \eqn{[0, 1]}.
#'
#' @return Numeric vector of quantiles, the same length as \code{probs}.
#'
#' @importFrom stats approx
#' @keywords internal
#' @export
.weightedQuantile <- function(x, w, probs){
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  if(length(x) == 0) return(rep(NA_real_, length(probs)))
  if(sum(w) == 0)    return(rep(NA_real_, length(probs)))
  o  <- order(x); x <- x[o]; w <- w[o]
  cw <- (cumsum(w) - 0.5 * w) / sum(w)               # midpoint plotting positions
  stats::approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}


#' Summarize the distribution of sampled origin points
#'
#' Reduces a set of origin points (e.g. from \code{\link{sampleOriginPoints}} and
#' \code{\link{extractEnvPoints}}) to per-group, per-variable summary statistics: a
#' weighted mean, weighted standard deviation, and weighted quantiles over the points.
#'
#' Because the points are drawn with probability proportional to the origin surface, the
#' probability weighting is already contained in the sample, and the summary is a plain
#' average over points. Supplying a finite movement scale \code{L} adds the half-normal
#' \code{\link{movementKernel}} of each point's distance from the reference location as
#' the only explicit weight (a movement prior); \code{L = Inf} (the default) gives the
#' unweighted, isotope-only summary. A finite \code{L} requires a distance column,
#' produced by passing \code{xy} to \code{\link{sampleOriginPoints}}.
#'
#' @param points A \code{data.frame} of points, one row per sample, with a grouping
#'   column and the variables to summarize (e.g. output of \code{\link{extractEnvPoints}}).
#' @param vars Character vector naming the columns in \code{points} to summarize.
#' @param group Name of the grouping column (typically the individual ID). Defaults to
#'   \code{"ID"}.
#' @param L Movement scale for the half-normal weight, in the surface's map units, or
#'   \code{Inf} for an unweighted summary. Defaults to \code{Inf}.
#' @param dist Name of the distance column used when \code{L} is finite. Defaults to
#'   \code{"dist"}.
#' @param probs Numeric vector of quantile probabilities. Defaults to
#'   \code{c(0.025, 0.5, 0.975)}.
#'
#' @return A long \code{data.frame} with one row per group x variable and columns for the
#'   grouping variable, \code{variable}, \code{n} (number of non-\code{NA} points),
#'   \code{mean}, \code{sd}, and one column per requested quantile (e.g. \code{q2.5}).
#'
#' @seealso \code{\link{sampleOriginPoints}}, \code{\link{extractEnvPoints}},
#'   \code{\link{movementKernel}}
#'
#' @importFrom methods is
#'
#' @examples
#' # Sample origin points from two example surfaces and attach an env value.
#' myiso <- rast(isoscape, type = "xyz")
#' myiso_sd <- rast(isoscape_sd, type = "xyz")
#' surfaces <- isotopeAssignmentModel(
#'         ID = c("A", "B"), isotopeValue = c(-100, -80), SD_indv = c(5, 5),
#'         precip_raster = myiso, precip_SD_raster = myiso_sd, nClusters = FALSE
#'         )
#' env <- myiso; names(env) <- "d2H"
#'
#' set.seed(1)
#' pts <- sampleOriginPoints(surfaces, n = 500, xy = c(-90, 45))
#' pts <- extractEnvPoints(pts, env)
#'
#' # Unweighted (isotope-only) summary...
#' summarizeDistribution(pts, vars = "d2H")
#' # ...and movement-weighted (L in map units, here degrees).
#' summarizeDistribution(pts, vars = "d2H", L = 10)
#'
#' @export
summarizeDistribution <- function(points, vars, group = "ID", L = Inf,
                                  dist = "dist", probs = c(0.025, 0.5, 0.975)){

  if(!is.data.frame(points))
    stop("'points' must be a data.frame.")
  if(missing(vars) || !is.character(vars) || length(vars) == 0)
    stop("'vars' must name one or more columns to summarize.")
  if(!all(vars %in% names(points)))
    stop("column(s) not found in 'points': ",
         paste(setdiff(vars, names(points)), collapse = ", "), ".")
  if(!group %in% names(points))
    stop("grouping column '", group, "' not found in 'points'.")
  if(!is.numeric(probs) || any(probs < 0 | probs > 1))
    stop("'probs' must be numeric values in [0, 1].")

  weighted <- is.finite(L)
  if(weighted && !dist %in% names(points))
    stop("finite 'L' requests movement weighting but distance column '", dist,
         "' is not in 'points'; pass 'xy' to sampleOriginPoints() first.")

  qnames <- paste0("q", probs * 100)
  idx_by_group <- split(seq_len(nrow(points)), points[[group]])

  rows <- lapply(names(idx_by_group), function(g){
    idx <- idx_by_group[[g]]
    w <- if(weighted) movementKernel(points[[dist]][idx], L) else rep(1, length(idx))
    per_var <- lapply(vars, function(vn){
      x  <- points[[vn]][idx]
      ok <- !is.na(x)
      xx <- x[ok]; ww <- w[ok]
      sw <- sum(ww)
      if(length(xx) == 0 || sw == 0){
        m <- NA_real_; s <- NA_real_; q <- rep(NA_real_, length(probs))
      } else {
        m <- sum(ww * xx) / sw
        s <- sqrt(sum(ww * (xx - m)^2) / sw)          # weighted population SD
        q <- .weightedQuantile(xx, ww, probs)
      }
      out <- data.frame(g, vn, length(xx), m, s, stringsAsFactors = FALSE)
      names(out) <- c(group, "variable", "n", "mean", "sd")
      out[qnames] <- as.list(q)
      out
    })
    do.call(rbind, per_var)
  })

  do.call(rbind, rows)
}
