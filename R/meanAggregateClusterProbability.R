#' Create mean aggregate probability-of-origin surfaces for each cluster.
#'
#' Subset probability-of-origin surfaces by cluster assignment and find mean aggregate probability-of-origin surface for each clustered group.
#' @param indivIDs Vector of individual ID variables corresponding to surface names.
#' @param clusters Vector of cluster IDs, in an order corresponding to `indivIDs`.
#' @param surfaces SpatRaster of probability-of-origin surfaces for all individuals.
#' @param nClust Depreciated. Formerly enabled multi-core processing; retained for back-compatibility. A non-`FALSE` value now issues a message and proceeds serially.
#'
#' @importFrom methods is
#'
#' @examples
#' \donttest{
#' # Create and cluster example assignment surfaces.
#' myiso <- rast(isoscape, type="xyz")
#' myiso_sd <- rast(isoscape_sd, type="xyz")
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
#' mySimilarityMatrix <- surfaceSimilarityMatrix(assignmentModels)
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

  if(is(surfaces, "Raster")) surfaces <- terra::rast(surfaces)

  if(!any(is(clusters,"vector"), is(clusters, "factor"), is(clusters, "integer")))
    stop( "clusters must be of class 'vector' or 'factor'." )
  if( !is(surfaces, "SpatRaster") )
    stop( "surfaces must be of class 'SpatRaster'." )
  if( !all(indivIDs %in% names(surfaces)) )
    stop( "All 'indivIDs' must match layer names of 'surfaces'. Missing: ",
          paste(setdiff(indivIDs, names(surfaces)), collapse = ", ") )

  if( nClust != FALSE & !any(isFALSE(nClust), is(nClust, "numeric") , is(nClust, "integer")) )
    stop( "nClust class must either be FALSE, numeric, or integer." )
  if( nClust != FALSE )
    message("Within-function parallelization is depreciated. Proceeding without parallelization.")

  # Iterate over the actual cluster labels (not seq_len(K)), so any cluster coding
  # -- integers, letters, or factors -- selects the right individuals (GitHub #5).
  clusterIDs <- unique(clusters)
  meanRasts_list <- lapply(clusterIDs, function(z){
    clustStack <- surfaces[[ indivIDs[ clusters == z] ]]
    meanRast <- terra::mean(clustStack, na.rm = TRUE)
    # terra::mean returns NaN where every layer is NA; keep the legacy NA there.
    terra::ifel(is.nan(meanRast), NA, meanRast)
  })

  out <- terra::rast(meanRasts_list)
  names(out) <- paste0("cluster_", clusterIDs)
  out

  }
