#' Project probability-of-origin surfaces into one summary surface.
#'
#' Create a summary surface showing which RasterLayer in a Stack has the highest value at a given location.
#' For each cell in a RasterStack, this function returns the identity of the RasterLayer with the highest value at that cell.
#' This surface is intended as a visual summary of common origins, not a basis for quantitative analysis.
#'
#' @param surfaces Object of class "RasterStack", where each layer represents a probability-of-origin surface
#' @param nClust Create and apply a multi-core cluster for faster processing using `raster` and `parallel` packages. Defaults to `FALSE` (i.e., no clustering).
#'
#' @importFrom raster calc
#'
#' @examples
#' # Create and cluster example assignment surfaces.
#' myiso <- rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' assignmentModels <- isotopeAssignmentModel(
#'         ID = LETTERS[1:4],
#'         isotopeValue = seq(-120,-25,length.out = 4),
#'         SD_indv = rep(5,4),
#'         precip_raster = myiso,
#'         precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#' raster::plot(assignmentModels)
#'
#' # Project mean aggregate surfaces into space.
#' summaryMap <- projectSummaryMaxSurface(
#'         surfaces = assignmentModels,
#'         nClust = FALSE
#'         )
#' raster::plot(summaryMap)
#'
#' @export projectSummaryMaxSurface

projectSummaryMaxSurface <- function(surfaces, nClust = FALSE){

  if( class(surfaces) != "RasterStack")
    stop( "surfaces must be of class 'RasterStack'." )
  if( nClust != FALSE & class(nClust) %in% c(FALSE, "numeric", "integer") != TRUE )
    stop( "nClust class must either be FALSE, numeric, or integer." )

  which.max2 <- function(x, ...) ifelse( length(x) == sum( is.na(x) ), NA, raster::which.max(x))

  if(nClust == FALSE){
    summaryMap <- raster::calc(surfaces, which.max2)
  } else {
    raster::beginCluster(nClust)
    summaryMap <- raster::clusterR(surfaces, raster::calc, args=list(which.max2))
    raster::endCluster()
  }

  summaryMap <- raster::ratify(summaryMap)
  return(summaryMap)

}
