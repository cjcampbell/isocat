#' Create mean aggregate probability-of-origin surfaces for each cluster.
#'
#' Subset probability-of-origin surfaces by cluster assignment and find mean aggregate probability-of-origin surface for each clustered group.
#' @param indivIDs Vector of individual ID variables corresponding to surface names.
#' @param clusters Vector of cluster IDs, in an order corresponding to `indivIDs`.
#' @param surfaces Stack of probability-of-origin surfaces for all individuals. Object of class 'RasterStack.'
#' @param nClust Create and apply a multi-core cluster for faster processing using `raster` and `parallel` packages. Defaults to `FALSE` (i.e., no clustering).
#'
#' @importFrom raster "calc"
#'
#' @examples
#' \donttest{
#' # Create and cluster example assignment surfaces.
#' myiso <- rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' df <- data.frame(
#'         ID = LETTERS[1:9],
#'         isotopeValue = seq(-120,-25,length.out = 9),
#'         SD_indv = rep(5, 9)
#'         )
#' assignmentModels <- isotopeAssignmentModel(
#'          ID = df$ID,
#'          isotopeValue = df$isotopeValue,
#'          SD_indv = df$SD_indv,
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd,
#'          nClusters = FALSE
#'          )
#' mySimilarityMatrix <- simmatrixMaker(assignmentModels)
#' cS <- clusterSimmatrix(
#'          simmatrix = mySimilarityMatrix,
#'          r = seq(.7,1.4,by=.1)
#'          )
#' # Cut clusters.
#' myheight <- 0.25
#' df$cluster <- dendextend::cutree(cS$hclust, h = myheight)
#' # Create mean aggregate surfaces.r p
#' meanSurfaces <- meanAggregateClusterProbability(
#'          indivIDs = df$ID,
#'          clusters = df$cluster,
#'          surfaces = assignmentModels,
#'          nClust = FALSE
#'          )
#'}
#' @export meanAggregateClusterProbability


meanAggregateClusterProbability <- function(indivIDs, clusters, surfaces, nClust = FALSE){

  if( class(clusters) %in% c("vector", "factor", "integer") != TRUE )
    stop( "clusters must be of class 'vector' or 'factor'." )
  if( class(surfaces) != "RasterStack")
    stop( "surfaces must be of class 'RasterStack'." )
  if( nClust != FALSE & class(nClust) %in% c(FALSE, "numeric", "integer") != TRUE )
    stop( "nClust class must either be FALSE, numeric, or integer." )

  which.mean <- function(x, ...) {
    ifelse( length(x) == sum( is.na(x) ), NA, mean(x, na.rm = TRUE))
  }

  meanRasts_list <- lapply(1:length(unique(clusters)), function(z){
    clustStack <- raster::subset(surfaces, indivIDs[ clusters == z])
    if(nClust == FALSE){
      meanRasts <- raster::calc(clustStack, which.mean)
    } else {
     raster::beginCluster(nClust)
      meanRasts <- raster::clusterR(clustStack, raster::calc, args=list(which.mean))
      raster::endCluster()
    }
    return(meanRasts)
  })

  raster::stack(meanRasts_list)

  }
