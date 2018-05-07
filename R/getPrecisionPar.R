#' Calculate model precision at given threshold values (in parallel).
#'
#' Function that counts cells (number and proportion) above given values.
#' @param rasterstack rasterStack of probability surfaces
#' @param checkVals vector of numeric 'threshold' values against which to calculate precision
#' @param method is FALSE by default. If character vector, appends a column recording 'method' used.
#'
#' @return Returns a dataframe of precision values at given threshold.
#'
#' @export getPrecisionPar
#'


getPrecisionPar <- function(rasterstack, checkVals, method = FALSE, nCluster = 20){

  # Iterate for each rasterstack layer:
  cl <- makeCluster(nCluster); doParallel::registerDoParallel(cl); getcells <- foreach(
    n = 1:nlayers(rasterstack), .verbose = T, .packages = c("raster", "plyr","dplyr")) %dopar% {
      # Calculate number of cells above each given threshold value.
      cellsAbove <- lapply(checkVals, function(z){
        data.frame(z = z, cellsAbove = sum(na.omit(rasterstack[[n]][]) >= z, na.rm = T)) })
      cells_df <- plyr::ldply(cellsAbove, data.frame)
      n_cells_tot <- sum(!is.na(rasterstack[[n]][]), na.rm = T)

      cbind(cells_df, propAbove = cells_df$cellsAbove / n_cells_tot, id = names(rasterstack[[n]]))

    }; endCluster()

  myDf <- plyr::ldply(getcells, data.frame)
  if(method != FALSE & class(method) == "character"){myDf <- cbind(myDf, method = method)}
  return(myDf)
}
