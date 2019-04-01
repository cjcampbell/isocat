#' Hierarchical clustering analysis of similarity matrix
#'
#' Function applies hierarchical clustering analysis to similarity matrix, such as one output by 'simmatrixMaker' function. Just a wrapper for pvclust. Output is a pvclust object.
#' @param simmatrix symmetric similarity matrix object.
#' @param dist_mthd Distance measure to be used. Defaults to "correlation". See help(pvclust).
#' @param hclust_mthd Method of clustering. Defaults to "average". See help(pvclust).
#' @param nBoot number of bootstrap replications. Defaults to 1000. See help(pvclust).
#' @param nClusters number of clusters to run in parallel using 'doParallel'. Defaults to FALSE (non-parallel).
#' @param r Relative size of bootstrap replications.
#' @export clusterSimmatrix
#'
#'

clusterSimmatrix <- function(simmatrix,
                             dist_mthd = "correlation", hclust_mthd = "average",
                             nBoot = 1000,  nClusters = FALSE, r=seq(.7,1.4,by=.1)){

  if("pvclust" %in% rownames(installed.packages()) == FALSE)
    stop("This function applies library 'pvclust' for parallel processing.
         Please install this package.")

  if(class(simmatrix) != "matrix") stop("Object 'x' is not of class 'Matrix'")
  if(nClusters != FALSE){

    if(!is.numeric(nClusters))
      stop("nClusters must be set to 'FALSE' or a numeric value.")
    if("parallel" %in% rownames(installed.packages()) == FALSE)
      stop("This function applies library 'parallel' for parallel processing.
           Please install this package.")
    if("doParallel" %in% rownames(installed.packages()) == FALSE)
      stop("This function applies library 'doParallel' for parallel processing.
           Please install this package.")

    cl <- parallel::makeCluster(nClusters)
    doParallel::registerDoParallel(cl)
  } else {
      cl <- FALSE
    }

  result_ave <- pvclust::pvclust(
    data = data.matrix(simmatrix),
    method.hclust = hclust_mthd,
    method.dist = dist_mthd,
    nboot = nBoot,
    r = seq(.7,1.4,by=.1),
    parallel = cl
  )

  if(nClusters != FALSE){
    stopCluster(cl)
  }

  return(result_ave)

}
