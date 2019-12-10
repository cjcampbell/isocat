#' Calculate model precision at given threshold values (in parallel).
#'
#' Function that counts cells (number and proportion) above given values.
#' @param rasterstack RasterStack of probability surfaces
#' @param checkVals vector of numeric 'threshold' values against which to calculate precision
#' @param method is FALSE by default. If character vector, appends a column recording 'method' used.
#' @param nCluster is a numeric object specifying how many clusters to form and run in parallel.
#'
#' @return Returns a dataframe of precision values at given threshold.
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#'
#' @export
getPrecisionPar <- function(rasterstack, checkVals, method = FALSE, nCluster = 20){

  n <- NULL

  # Iterate for each RasterStack layer:
  cl <- parallel::makeCluster(nCluster); doParallel::registerDoParallel(cl); getcells <- foreach(
    n = 1:raster::nlayers(rasterstack),
    .verbose = TRUE,
    .packages = c("raster", "plyr")) %dopar% {
      # Calculate number of cells above each given threshold value.
      cellsAbove <- lapply(checkVals, function(z){
        data.frame(
          z = z,
          cellsAbove = sum(stats::na.omit(rasterstack[[n]][]) >= z, na.rm = TRUE))
        }
        )
      cells_df <- plyr::ldply(cellsAbove, data.frame)
      n_cells_tot <- sum(!is.na(rasterstack[[n]][]), na.rm = TRUE)

      cbind(
        cells_df,
        propAbove = cells_df$cellsAbove / n_cells_tot,
        id = names(rasterstack[[n]])
        )

    }; parallel::stopCluster()

  myDf <- plyr::ldply(getcells, data.frame)
  if(method != FALSE & class(method) == "character"){
    myDf <- cbind(myDf, method = method)
    }
  return(myDf)
}
