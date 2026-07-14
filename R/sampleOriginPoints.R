#' Sample probability-weighted origin points from a probability-of-origin surface
#'
#' Draws \code{n} points from each layer of a probability-of-origin \code{SpatRaster}
#' by sampling cells with probability proportional to their (normalized) value. The
#' returned point cloud is a Monte-Carlo representation of the origin surface: cells
#' with higher probability of origin are sampled more often, so probability weighting
#' is already baked into the sample. Points are the basis for extracting environmental
#' values (\code{\link{extractEnvPoints}}) and summarizing the distribution of plausible
#' origins (\code{\link{summarizeDistribution}}).
#'
#' If a reference location (e.g. the sampling or capture site) is supplied via
#' \code{xy}, each point is also assigned its \code{easting} and \code{northing}
#' relative to that location and its Euclidean \code{dist} to it, in the surface's
#' map units. Distances are Euclidean, so the surface should be in a projected
#' (ideally equal-area) coordinate system for them to be meaningful.
#'
#' Sampling uses \code{\link[base]{sample.int}}; call \code{set.seed()} beforehand for
#' reproducibility.
#'
#' @param surface A \code{SpatRaster} of one or more probability-of-origin layers
#'   (e.g. output of \code{\link{isotopeAssignmentModel}}). Layer names are used as
#'   individual IDs. Legacy \code{Raster*} objects are coerced.
#' @param n Number of points to sample per layer. Defaults to 1000.
#' @param minValue Cells with value greater than \code{minValue} (and non-\code{NA})
#'   are eligible for sampling. Defaults to 0.
#' @param xy Optional reference location(s). Either a length-2 numeric \code{c(x, y)}
#'   applied to every layer, or a \code{data.frame} with columns \code{ID}, \code{x},
#'   and \code{y} keyed to layer names. Coordinates must be in the surface's CRS. A
#'   warning is issued if \code{xy} is supplied for a geographic (lon/lat) surface, where
#'   the returned distances would be in degrees rather than true distances.
#'
#' @return A long \code{data.frame} with one row per sampled point and columns
#'   \code{ID}, \code{cell}, \code{x}, \code{y}, and \code{prob} (the surface value at
#'   the sampled cell). When \code{xy} is supplied, also \code{easting}, \code{northing},
#'   and \code{dist}.
#'
#' @seealso \code{\link{extractEnvPoints}}, \code{\link{summarizeDistribution}}
#'
#' @importFrom methods is
#'
#' @examples
#' # The bundled isoscape is already on an equal-area grid, so easting/northing/dist
#' # come out as true distances (metres).
#' myiso    <- rast(isoscape,    type = "xyz", crs = attr(isoscape,    "crs"))
#' myiso_sd <- rast(isoscape_sd, type = "xyz", crs = attr(isoscape_sd, "crs"))
#' surfaces <- isotopeAssignmentModel(
#'         ID = c("A", "B"),
#'         isotopeValue = c(-100, -80),
#'         SD_indv = c(5, 5),
#'         precip_raster = myiso,
#'         precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#'
#' # Sample origin points relative to the centre of the study extent.
#' e <- as.vector(terra::ext(surfaces))
#' site <- c(x = mean(e[1:2]), y = mean(e[3:4]))
#' set.seed(1)
#' pts <- sampleOriginPoints(surfaces, n = 500, xy = site)
#' head(pts)
#'
#' @export
sampleOriginPoints <- function(surface, n = 1000, minValue = 0, xy = NULL){

  if(is(surface, "Raster")) surface <- terra::rast(surface)
  if(!is(surface, "SpatRaster"))
    stop("'surface' must be a SpatRaster.")
  if(!is.numeric(n) || length(n) != 1 || n < 1)
    stop("'n' must be a single positive number.")
  if(!is.numeric(minValue) || length(minValue) != 1)
    stop("'minValue' must be a single numeric value.")

  ids <- names(surface)
  ref <- if(is.null(xy)) NULL else .parseRefXY(xy, ids)

  # Distances are Euclidean in map units, so warn if a reference location is given for a
  # geographic (lon/lat) surface, where degree distances are not true distances.
  if(!is.null(ref) && isTRUE(terra::is.lonlat(surface, perhaps = TRUE, warn = FALSE)))
    warning("'surface' appears to use geographic (lon/lat) coordinates; 'easting', ",
            "'northing', and 'dist' are Euclidean in degrees and are not true distances. ",
            "Project 'surface' to an equal-area CRS before sampling.")

  vals   <- terra::values(surface)                                  # ncell x nlyr
  cellxy <- terra::xyFromCell(surface, seq_len(terra::ncell(surface)))

  pts <- lapply(seq_len(terra::nlyr(surface)), function(k){
    v  <- vals[, k]
    ok <- which(!is.na(v) & v > minValue)
    if(length(ok) == 0)
      stop(sprintf("Layer '%s' has no cells with value > minValue.", ids[k]))
    # Index into `ok` via sample.int() to avoid sample()'s single-element gotcha.
    cell <- ok[ sample.int(length(ok), size = n, replace = TRUE, prob = v[ok]) ]
    ox <- cellxy[cell, 1]; oy <- cellxy[cell, 2]
    d <- data.frame(ID = ids[k], cell = cell, x = ox, y = oy, prob = v[cell],
                    stringsAsFactors = FALSE)
    if(!is.null(ref)){
      r <- ref[ids[k], ]
      d$easting  <- ox - r[["x"]]
      d$northing <- oy - r[["y"]]
      d$dist     <- sqrt((ox - r[["x"]])^2 + (oy - r[["y"]])^2)
    }
    d
  })

  do.call(rbind, pts)
}


#' Resolve reference-location input for sampleOriginPoints
#'
#' Internal helper. Normalizes the \code{xy} argument into a matrix of reference
#' coordinates with columns \code{x}, \code{y} and one row per ID (rownamed by ID).
#'
#' @param xy Either a length-2 numeric \code{c(x, y)} or a \code{data.frame} with
#'   columns \code{ID}, \code{x}, \code{y}.
#' @param ids Character vector of layer/individual IDs to key against.
#'
#' @return A numeric matrix with columns \code{x}, \code{y} and rownames \code{ids}.
#'
#' @keywords internal
#' @export
.parseRefXY <- function(xy, ids){
  if(is.numeric(xy) && length(xy) == 2){
    ref <- matrix(xy, nrow = length(ids), ncol = 2, byrow = TRUE,
                  dimnames = list(ids, c("x", "y")))
    return(ref)
  }
  if(is.data.frame(xy)){
    if(!all(c("ID", "x", "y") %in% names(xy)))
      stop("'xy' data.frame must have columns 'ID', 'x', and 'y'.")
    missing_ids <- setdiff(ids, xy$ID)
    if(length(missing_ids) > 0)
      stop("'xy' is missing reference locations for: ",
           paste(missing_ids, collapse = ", "), ".")
    ref <- as.matrix(xy[match(ids, xy$ID), c("x", "y")])
    rownames(ref) <- ids
    return(ref)
  }
  stop("'xy' must be a length-2 numeric c(x, y) or a data.frame with columns 'ID', 'x', 'y'.")
}
