#' Calculates Schoener's D-value between two RasterLayers.
#'
#' RasterLayers must have identical resolutions and extents. The function will
#' automatically normalize surfaces to sum to 1.
#'
#' Calculates similarity value of two RasterLayers using Schoener's D-metric.
#' @param rast1 First object of class SpatRaster
#' @param rast2 Second object of class SpatRaster
#'
#'
#' @importFrom terra rast
#' @importFrom terra global
#'
#' @examples
#' # Generate example probability surfaces.
#' myiso <- rast(isoscape, type="xyz")
#' myiso_sd <- rast(isoscape_sd, type="xyz")
#' df <- data.frame(
#'         ID = c(-100, -80, -50),
#'         isotopeValue = c(-100, -80, -50),
#'         SD_indv = rep(5, 3)
#'         )
#' assignmentModels <- isotopeAssignmentModel(
#'         ID = df$ID,
#'         isotopeValue = df$isotopeValue,
#'         SD_indv = df$SD_indv,
#'         precip_raster = myiso,
#'         precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#'
#' # Calculate Schoener's D-metric of spatial similarity between two of the
#' # example probability surfaces.
#' schoenersD(assignmentModels[[1]], assignmentModels[[2]])
#' ## 0.969156
#'
#' @export schoenersD



# Run pairwise comparisons.
schoenersD <- function(rast1, rast2){

  if(is(rast1, "Raster")) rast1 <- terra::rast(rast1)
  if(is(rast2, "Raster")) rast2 <- terra::rast(rast2)

  stopifnot(
    "Argument rast1 is not of class 'SpatRaster'" = is(rast1, "SpatRaster"),
    "Argument rast2 is not of class 'SpatRaster'" = is(rast2, "SpatRaster")
  )

  if( as.numeric(global(rast1, "sum", na.rm = TRUE)) != 1 ) { rast1 <- .normprodrast(rast1) }
  if( as.numeric(global(rast2, "sum", na.rm = TRUE)) != 1 ) { rast2 <- .normprodrast(rast2) }

  # terra::global() returns a 1-row, 1-col data.frame; coerce to a plain numeric scalar
  # so the result matches the documented value and the natural downstream idiom
  # (e.g. data.frame(D = schoenersD(a, b))) rather than a column named "sum".
  as.numeric(1 - (0.5 * global(abs(rast1 - rast2), "sum", na.rm = TRUE)))

  }








