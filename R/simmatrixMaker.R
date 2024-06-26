#' Generates similarity matrix for RasterStack
#'
#' Legacy function that runs on raster::stack. Applies pairwise comparisons of Schoener's D-metric between each RasterLayer in a RasterStack to populate a similarity matrix.
#' @param assignmentRasters Input RasterStack
#' @param nClusters Clusters to create run in parallel using 'doParallel'. Defaults to FALSE.
#' @param csvSavePath Optional savepath to write similarity matrix to csv file. Defaults to FALSE, will not create csv.
#'
#' @importFrom foreach foreach
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom methods is
#'
#'
#' @examples
#' # Create probability-of-origin maps to compare.
#' myiso <- rast(isoscape, type="xyz")
#' plot(myiso)
#' myiso_sd <- rast(isoscape_sd, type="xyz")
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
#' simmatrixMaker(assignmentModels, nClusters = FALSE, csvSavePath = FALSE)
#'
#'
#' @export
simmatrixMaker <- function(assignmentRasters, nClusters = FALSE, csvSavePath = FALSE){

  if(missing(assignmentRasters))
    stop("Object 'assignmentRasters' not found.")
  if(!is(assignmentRasters, "RasterStack") )
    stop("Object 'assignmentRasters' is not of class 'RasterStack.'")

  # Run pairwise comparisons.
  schoener <- function(rast1, rast2){ 1 - (0.5 * raster::cellStats(abs(rast1 - rast2), stat='sum')) }

  a <- names(assignmentRasters)

  t <- t(utils::combn(a,2))

  if(nClusters == FALSE){

    if (!requireNamespace("foreach", quietly = TRUE)) {
      stop("Package \"foreach\" needed for this function to work.
           Please install it.", call. = FALSE)
      }

    l <- foreach(i = 1:nrow(t), .packages="raster") %do% {
      schoener(
        assignmentRasters[[t[i,1]]],
        assignmentRasters[[t[i,2]]])
    }

    # Create and populate matrix.
    l <- unlist(l)
    x <- matrix( NA, length(a), length(a), dimnames=list(a,a) )
    combos1 <- paste0( colnames(x)[col(x)], rownames(x)[ row(x) ] )
    combos2 <- paste0( t[ ,1], t[ ,2] )
    x[ match(combos2, combos1) ] <- l

    # Force symmetry.
    for( i in 1:ncol(x) ){
      for( j in 1:nrow(x) ){
        ifelse( is.na( x[i,j] ),
               ifelse( !is.na(x[j,i] ),
                      x[i,j] <- x[j,i],
                      1),
               x[i,j])
      }}

    x[is.na(x)] <- 1
  }

  if(nClusters != FALSE){
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package \"doParallel\" needed for this function to work as called.",
           call. = FALSE)
      }

    cl <- parallel::makeCluster(nClusters)
    doParallel::registerDoParallel(cl)
    l <- foreach(i = 1:nrow(t), .packages="raster") %dopar% {
      schoener(
        assignmentRasters[[t[i,1]]],
        assignmentRasters[[t[i,2]]])
    }

    # Create and populate matrix.
    l <- unlist(l)
    x<- matrix(NA,length(a),length(a),dimnames=list(a,a))
    combos1<-paste0(colnames(x)[col(x)],rownames(x)[row(x)])
    combos2<- paste0(t[,1],t[,2])
    x[match(combos2,combos1)]<- l

    # Force symmetry.
    for(i in 1:ncol(x)){
      for(j in 1:nrow(x)){
        ifelse(is.na(x[i,j]),
               ifelse(!is.na(x[j,i]),
                      x[i,j] <- x[j,i],
                      1),
               x[i,j])
      }}

    x[is.na(x)] <- 1

    parallel::stopCluster(cl)
  }

  if(csvSavePath != FALSE){
    utils::write.csv(x, file = file.path(csvSavePath, "PairwiseComparisonMatrix.csv"))
  }
  return(x)
}


#' Generates similarity matrix for SpatRaster objects in environment.
#'
#' Applies pairwise comparisons of Schoener's D-metric for SpatRaster objects that are loaded into the environment.
#'
#' @param spatrast Input SpatRaster
#'
#' @examples
#' # Create probability-of-origin maps to compare.
#' myiso <- rast(isoscape, type="xyz")
#' plot(myiso)
#' myiso_sd <- rast(isoscape_sd, type="xyz")
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
#' simmatrix(assignmentModels)
#'
#'
#' @export
schoenersDsimmatrix <- function(spatrast){

  stopifnot(
    "input `spatrast` must be of class SpatRaster" = is(spatrast, "SpatRaster")
  )

  # Ensure appropriate and unique layer names.
  names(spatrast) <- make.names(names(spatrast))
  names(spatrast) <- make.unique(names(spatrast))

  a <- names(spatrast)
  t <- t(utils::combn(a,2))
  m <- matrix(data = NA, nrow = length(a), ncol = length(a))

  for(i in 1:nrow(t)) {
    d <- schoenersD(
      spatrast[[t[i,1]]],
      spatrast[[t[i,2]]]
    )
    m[upper.tri(m)][i] <- unlist(d)
  }
  # force symmetry.
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m[!lower.tri(m)&!upper.tri(m)] <- 1

  return(m)
}
