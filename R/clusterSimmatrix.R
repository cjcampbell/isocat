#' Hierarchical clustering analysis of similarity matrix
#'
#' Function applies hierarchical clustering analysis to similarity matrix, such as one output by 'simmatrixMaker' function. Just a wrapper for pvclust. Output is a pvclust object.
#'
#'
#' @param simmatrix symmetric similarity matrix object.
#' @param dist_mthd Distance measure to be used. Defaults to "correlation". See help(pvclust).
#' @param hclust_mthd Method of clustering. Defaults to "average". See help(pvclust).
#' @param nBoot number of bootstrap replications. Defaults to 1000. See help(pvclust).
#' @param nClusters number of clusters to run in parallel using 'doParallel'. Defaults to FALSE (non-parallel).
#' @param r Relative size of bootstrap replications.
#'
#'
#' @examples
#' \donttest{
#' # Create probability-of-origin maps to compare.
#' myiso <- rasterFromXYZ(isoscape)
#' raster::plot(myiso)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' n <- 5
#' set.seed(42)
#' df <- data.frame(
#'          ID = LETTERS[1:n],
#'          isotopeValue = sample(-120:-40, n),
#'          SD_indv = rep(5, n)
#'          )
#' assignmentModels <- isotopeAssignmentModel(
#'                         ID = df$ID,
#'                         isotopeValue = df$isotopeValue,
#'                         SD_indv = df$SD_indv,
#'                         precip_raster = myiso,
#'                         precip_SD_raster = myiso_sd,
#'                         nClusters = FALSE
#'                         )
#' raster::plot(assignmentModels)
#' # Compare maps with simmatrixMaker.
#' mymatrix <- simmatrixMaker(assignmentModels, nClusters = FALSE, csvSavePath = FALSE)
#' # Cluster similarity matrix.
#' clust_results <- clusterSimmatrix(mymatrix, dist_mthd = "correlation",
#'     hclust_mthd = "average", nBoot = 1000,  nClusters = FALSE,
#'     r = seq(.7,1.4,by=.1) )
#' clust_results
#' }
#'
#' @export
clusterSimmatrix <- function(simmatrix,
                             dist_mthd = "correlation", hclust_mthd = "average",
                             nBoot = 1000,  nClusters = FALSE, r=seq(.7,1.4,by=.1)){

  if (!requireNamespace("pvclust")) {
    stop(" The \"pvclust\" package is required for parallel processing.")
  }

  if(class(simmatrix) != "matrix") stop("Object 'x' is not of class 'Matrix'")
  if(nClusters != FALSE){

    if(!is.numeric(nClusters))
      stop("nClusters must be set to 'FALSE' or a numeric value.")

    if (!requireNamespace("parallel")) {
      stop(" The \"parallel\" package is required for parallel processing.")
    }
    if (!requireNamespace("doParallel")) {
      stop(" The \"doParallel\" package is required for parallel processing.")
    }

    cl <- parallel::makeCluster(nClusters)
    doParallel::registerDoParallel(cl)
  } else {
      cl <- FALSE
    }

  suppressWarnings(
    result_ave <- pvclust::pvclust(
      data = data.matrix(simmatrix),
      method.hclust = hclust_mthd,
      method.dist = dist_mthd,
      nboot = nBoot,
      r = seq(.7,1.4,by=.1),
      parallel = cl
    )
  )

  if(nClusters != FALSE){
    parallel::stopCluster(cl)
  }

  return(result_ave)

}
