#' Calculate model precision at given threshold values.
#'
#' Function that counts cells (number and proportion) above given values.
#' @param rasterstack SpatRaster of probability surfaces
#' @param checkVals vector of numeric 'threshold' values against which to calculate precision
#' @param method is FALSE by default. If character vector, appends a column recording 'method' used.
#' @param nCluster Depreciated. Formerly the number of parallel clusters; retained for back-compatibility. A non-`FALSE` value now issues a message and proceeds serially.
#'
#' @return Returns a dataframe of precision values at given threshold.
#'
#' @importFrom methods is
#'
#' @examples
#' myiso <- rast(isoscape, type="xyz")
#' myiso_sd <- rast(isoscape_sd, type="xyz")
#' assignmentModels <- isotopeAssignmentModel(
#'          ID = LETTERS[1:4],
#'          isotopeValue = seq(-120, -25, length.out = 4),
#'          SD_indv = rep(5, 4),
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd
#'          )
#' getPrecisionPar(assignmentModels, checkVals = c(0.001, 0.005, 0.01))
#'
#' @export
getPrecisionPar <- function(rasterstack, checkVals, method = FALSE, nCluster = FALSE){

  if(is(rasterstack, "Raster")) rasterstack <- terra::rast(rasterstack)

  if( nCluster != FALSE )
    message("Within-function parallelization is depreciated. Proceeding without parallelization.")

  # Iterate over each layer, counting cells at or above each threshold value.
  getcells <- lapply(1:terra::nlyr(rasterstack), function(n){
    layer_vals  <- stats::na.omit( as.vector( terra::values(rasterstack[[n]]) ) )
    n_cells_tot <- length(layer_vals)
    cells_df <- plyr::ldply(checkVals, function(z){
      data.frame(z = z, cellsAbove = sum(layer_vals >= z, na.rm = TRUE))
    })
    cbind(
      cells_df,
      propAbove = cells_df$cellsAbove / n_cells_tot,
      id = names(rasterstack[[n]])
    )
  })

  myDf <- plyr::ldply(getcells, data.frame)
  if(method != FALSE & is(method, "character") ){
    myDf <- cbind(myDf, method = method)
    }
  return(myDf)
}
