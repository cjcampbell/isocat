#' Project probability-of-origin surfaces into one summary surface.
#'
#' Create a summary surface showing which RasterLayer in a Stack has the highest value at a given location.
#' For each cell in a RasterStack, this function returns the identity of the RasterLayer with the highest value at that cell.
#' This surface is intended as a visual summary of common origins, not a basis for quantitative analysis.
#'
#' @param surfaces SpatRaster where each layer represents a probability-of-origin surface
#' @param nClust Depreciated. Formerly enabled multi-core processing; retained for back-compatibility. A non-`FALSE` value now issues a message and proceeds serially.
#'
#' @importFrom methods is
#'
#' @examples
#' # Create and cluster example assignment surfaces.
#' myiso <- rast(isoscape, type="xyz")
#' myiso_sd <- rast(isoscape_sd, type="xyz")
#' assignmentModels <- isotopeAssignmentModel(
#'         ID = LETTERS[1:4],
#'         isotopeValue = seq(-120,-25,length.out = 4),
#'         SD_indv = rep(5,4),
#'         precip_raster = myiso,
#'         precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#' plot(assignmentModels)
#'
#' # Project mean aggregate surfaces into space.
#' summaryMap <- projectSummaryMaxSurface(
#'         surfaces = assignmentModels,
#'         nClust = FALSE
#'         )
#' plot(summaryMap)
#'
#' @export projectSummaryMaxSurface

projectSummaryMaxSurface <- function(surfaces, nClust = FALSE){

  if(is(surfaces, "Raster")) surfaces <- terra::rast(surfaces)

  if( !is(surfaces, "SpatRaster") )
    stop( "surfaces must be of class 'SpatRaster'." )
  if( nClust != FALSE & !any(isFALSE(nClust), is(nClust, "numeric") , is(nClust, "integer")) )
    stop( "nClust class must either be FALSE, numeric, or integer." )
  if( nClust != FALSE )
    message("Within-function parallelization is depreciated. Proceeding without parallelization.")

  summaryMap <- terra::which.max(surfaces)
  summaryMap <- terra::as.factor(summaryMap)
  return(summaryMap)

}
