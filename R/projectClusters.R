#' Function to project clusters
#'
#' Evaluate tree and cut. Export cluster identity as a vector (or part of a dataframe).
#' Project the spatial regions best associated with each cluster.
#' This function assumes that the levels of 'assignmentRasters' and order of 'clustID' correspond to the same indivID.
#' Output is raster or rasterstack of cluster spatial projections.
#'
#' @param assignmentRasters Rasterstack of isotope assignment models.
#' @param clustID vector or integer of cluster assignments for each individual. Must match names and order of 'assignmentRasters'.
#' @param simmatrix similarity matrix. Output of 'simmatrixMaker' function. Names must match names of 'assignmentRasters'.
#' @param namedOutput Requested output. See 'Details.'
#' @param parallel Specify clusters to run in parallel using clusterR package. Defaults to FALSE.
#' @details 'namedOutput' options:
#' \itemize{
#'  \item{"numbered"}{ returns raster of projected clusters numbered by input order.}
#'  \item{"named"}{ returns raster of projected clusters named by input cluster names.}
#'  \item{"both}{ returns rasterstack of 'numbered' and 'named' rasters.}
#' }

projectClusters <- function(assignmentRasters, clustID, simmatrix, namedOutput = "both", parallel = FALSE){

  if(missing(assignmentRasters))
    stop("Object 'assignmentRasters' not found.")
  if(class(assignmentRasters) != "RasterStack")
    stop("Object 'assignmentRasters' is not of class 'RasterStack.'")
  if(class(as.vector(clustID)) != "vector" & class(as.vector(clustID)) != "integer")
    stop("clustID cannot be converted to vector format.")
  if(length(unique(clustID)) < 2)
    stop("'clustID' must describe more than one cluster")

  friendlyRasts <- lapply(unique(clustID), function(z){
    targetrasts <- names(assignmentRasters[[which(clustID == z)]])
    subsetSimmatrix <- simmatrix[which(paste0("X",rownames(simmatrix)) %in% targetrasts),
                                 which(paste0("X",colnames(simmatrix)) %in% targetrasts)]

    if(!is.matrix(subsetSimmatrix)) { # When there is only 1 individual / cluster, manually create matrix.
      subsetSimmatrix <- matrix(1, dimnames = as.list(rep(targetrasts, times = 2)))
    }

    highestMeanSimVal <- max(colMeans(subsetSimmatrix)) # Find highest mean similarity value within subsetted similarity matrix.
    nameMostSimilar <- colnames(subsetSimmatrix)[colMeans(subsetSimmatrix) == highestMeanSimVal]
    mostSimilarRast <- assignmentRasters[[ paste0("X", nameMostSimilar) ]][[1]]

    return(mostSimilarRast)
  })

  friendlyStack <- stack(friendlyRasts)
  names(friendlyStack) <- paste0("C", unique(clustID))

  which.max2 <- function(x, ...) ifelse( length(x) == sum(is.na(x) ), NA, which.max(x))

  if(parallel == FALSE){
    projectFriendly1 <- calc(friendlyStack, which.max2)
  }
  if(parallel != FALSE){
    if("snow" %in% rownames(installed.packages()) == FALSE) stop("This function applies library 'snow' for parallel processing. Please install this package.")
    # if("ClusterR" %in% rownames(installed.packages()) == FALSE) stop("This function applies library 'ClusterR' for parallel processing. Please install this package.")

    raster::beginCluster(parallel)
    projectFriendly1 <- raster::clusterR(friendlyStack, calc, args=list(which.max2))
    raster::endCluster()
  }

  projectFriendly2 <- projectFriendly1

  if (!requireNamespace("foreach", quietly = TRUE)) { stop("Package \"foreach\" needed for this function to work. Please install it.", call. = FALSE) }

  valsFriendly <- na.omit(unique(projectFriendly2@data@values))
  foreach(i = valsFriendly) %do% {
    projectFriendly2[projectFriendly2 == i] <- names(friendlyStack)[i]
  }

  if(namedOutput == "numbered"){
    result <- projectFriendly1
  }
  if(namedOutput == "named"){
    result <- projectFriendly2
  }
  if(namedOutput == "both"){
    result <- stack(projectFriendly1, projectFriendly2)
    names(result) <- c("numbered", "named")
  }

  return(result)
}

