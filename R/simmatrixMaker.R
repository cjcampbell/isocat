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
#' \donttest{
#' # simmatrixMaker() is a legacy function for raster::stack input;
#' # for SpatRaster input use surfaceSimilarityMatrix() instead.
#' if (requireNamespace("raster", quietly = TRUE)) {
#'   myiso <- rast(isoscape, type = "xyz")
#'   myiso_sd <- rast(isoscape_sd, type = "xyz")
#'   set.seed(42)
#'   assignmentModels <- isotopeAssignmentModel(
#'     ID = LETTERS[1:5],
#'     isotopeValue = sample(-120:-40, 5),
#'     SD_indv = rep(5, 5),
#'     precip_raster = myiso,
#'     precip_SD_raster = myiso_sd
#'   )
#'   # Coerce to a RasterStack for the legacy interface.
#'   simmatrixMaker(raster::stack(assignmentModels))
#' }
#' }
#'
#' @export
simmatrixMaker <- function(assignmentRasters, nClusters = FALSE, csvSavePath = FALSE){

  if(missing(assignmentRasters))
    stop("Object 'assignmentRasters' not found.")
  if (!requireNamespace("raster", quietly = TRUE))
    stop("Package 'raster' is required for the legacy simmatrixMaker(); use surfaceSimilarityMatrix() for SpatRaster input.", call. = FALSE)
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


#' Generate a similarity matrix for the layers of a SpatRaster
#'
#' Populates a similarity matrix from pairwise Schoener's D comparisons between the
#' layers of a SpatRaster.
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
#' plot(assignmentModels)
#' # Compare maps with surfaceSimilarityMatrix.
#' surfaceSimilarityMatrix(assignmentModels)
#'
#'
#' @export
surfaceSimilarityMatrix <- function(spatrast){

  stopifnot(
    "input `spatrast` must be of class SpatRaster" = is(spatrast, "SpatRaster")
  )

  # Ensure valid, unique layer names for the matrix dimnames.
  names(spatrast) <- make.unique(make.names(names(spatrast)))
  a <- names(spatrast)
  n <- length(a)

  # Extract all layer values once (cells x layers) and normalize each surface to
  # sum to 1, matching schoenersD(). Working on the extracted matrix avoids the
  # n*(n-1)/2 terra subset/global calls that dominated the old pairwise loop.
  v <- terra::values(spatrast)
  v <- sweep(v, 2, colSums(v), "/")

  # Pairwise Schoener's D = 1 - 0.5 * sum|p_i - p_j| over cells.
  m <- matrix(1, nrow = n, ncol = n, dimnames = list(a, a))
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      d <- 1 - 0.5 * sum(abs(v[, i] - v[, j]))
      m[i, j] <- d
      m[j, i] <- d
    }
  }

  return(m)
}
