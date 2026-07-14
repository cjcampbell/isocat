#' Extract environmental values at sampled origin points
#'
#' Attaches one or more environmental covariates to a set of origin points (e.g. from
#' \code{\link{sampleOriginPoints}}) by extracting an environmental \code{SpatRaster}
#' at each point's coordinates. The full point-by-point distribution is retained; layer
#' values are column-bound onto the input so downstream summaries
#' (\code{\link{summarizeDistribution}}) reduce to a simple (optionally weighted)
#' average over the points.
#'
#' Values are extracted at point coordinates, so \code{env} need not share the origin
#' surface's grid, but it must share its coordinate system.
#'
#' @param points A \code{data.frame} of points with coordinate columns (e.g. output of
#'   \code{\link{sampleOriginPoints}}).
#' @param env A \code{SpatRaster} of one or more environmental covariates, in the same
#'   CRS as the coordinates in \code{points}. Legacy \code{Raster*} objects are coerced.
#' @param xy Length-2 character vector naming the coordinate columns in \code{points}.
#'   Defaults to \code{c("x", "y")}.
#'
#' @return \code{points} with one appended column per layer of \code{env}, named by the
#'   layer names. Points falling outside a layer's coverage receive \code{NA}.
#'
#' @seealso \code{\link{sampleOriginPoints}}, \code{\link{summarizeDistribution}}
#'
#' @importFrom methods is
#'
#' @examples
#' # Generate an example probability surface and sample origin points.
#' myiso <- rast(isoscape, type = "xyz")
#' myiso_sd <- rast(isoscape_sd, type = "xyz")
#' surface <- isotopeAssignmentModel(
#'         ID = "A", isotopeValue = -100, SD_indv = 5,
#'         precip_raster = myiso, precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#' set.seed(1)
#' pts <- sampleOriginPoints(surface, n = 500)
#'
#' # Use the isoscape itself as a stand-in environmental layer.
#' env <- myiso
#' names(env) <- "d2H"
#' pts <- extractEnvPoints(pts, env)
#' head(pts)
#'
#' @export
extractEnvPoints <- function(points, env, xy = c("x", "y")){

  if(is(env, "Raster")) env <- terra::rast(env)
  if(!is(env, "SpatRaster"))
    stop("'env' must be a SpatRaster.")
  if(!is.data.frame(points))
    stop("'points' must be a data.frame (e.g. from sampleOriginPoints()).")
  if(length(xy) != 2 || !all(xy %in% names(points)))
    stop("'points' must contain the coordinate columns named in 'xy' (default 'x', 'y').")

  clash <- intersect(names(env), names(points))
  if(length(clash) > 0)
    stop("env layer name(s) already present in 'points': ",
         paste(clash, collapse = ", "),
         ". Rename the env layers to avoid overwriting columns.")

  e <- terra::extract(env, as.matrix(points[, xy]))
  e <- as.data.frame(e)
  names(e) <- names(env)                       # extract on a coord matrix returns no ID column

  cbind(points, e)
}
