#' Hierarchical clustering analysis of similarity matrix
#'
#' Function applies hierarchical clustering analysis to similarity matrix, such as one output by 'simmatrixMaker' function. Just a wrapper for pvclust. Output is a pvclust object.
#' @param simmatrix symmetric similarity matrix object.
#' @param dist_mthd Distance measure to be used. Defaults to "correlation". See help(pvclust).
#' @param hclust_mthd Method of clustering. Defaults to "average". See help(pvclust).
#' @param nBoot number of bootstrap replications. Defaults to 1000. See help(pvclust).
#' @param nClusters number of clusters to run in parallel using 'doParallel'. Defaults to FALSE (non-parallel).

clusterSimmatrix <- function(simmatrix, dist_mthd = "correlation", hclust_mthd = "average", nBoot = 1000,  nClusters = FALSE){

  if("pvclust" %in% rownames(installed.packages()) == FALSE) stop("This function applies library 'pvclust' for parallel processing. Please install this package.")

  if(class(simmatrix) != "matrix") stop("Object 'x' is not of class 'Matrix'")
  if(nClusters != FALSE){

    if(!is.numeric(nClusters)) stop("nClusters must be set to 'FALSE' or a numeric value.")
    if("parallel" %in% rownames(installed.packages()) == FALSE) stop("This function applies library 'parallel' for parallel processing. Please install this package.")
    if("doParallel" %in% rownames(installed.packages()) == FALSE) stop("This function applies library 'doParallel' for parallel processing. Please install this package.")

    cl <- parallel::makeCluster(nClusters)
    doParallel::registerDoParallel(cl)
  }

  result_ave <- pvclust(
    data = data.matrix(simmatrix),
    dist_mthd = dist_mthd,
    hclust_mthd = hclust_mthd,
    nboot = nboot,
    parallel = cl
  )

  if(nClusters != FALSE){
    stopCluster(cl)
  }

  return(result_ave)

}
