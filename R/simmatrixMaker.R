#' Generates similarity matrix for rasterStack
#'
#' Applies pairwise comparisons of scheoner's D-metric amongst each rasterLayer in a RasterStack to populate a similarity matrix.
#' @param assignmentRasters Input rasterStack
#' @param nClusters Clusters to create run in parallel using 'doParallel'. Defaults to FALSE.
#' @param csvSavePath Optional savepath to write similarity matrix to csv file. Defaults to FALSE, will not create csv.
#'
#' @import foreach
#'
#' @export
simmatrixMaker <- function(assignmentRasters, nClusters = FALSE, csvSavePath = FALSE){

  if(missing(assignmentRasters))
    stop("Object 'assignmentRasters' not found.")
  if(class(assignmentRasters) != "RasterStack")
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
    utils::write.csv(x, file = paste0(csvSavePath, "PairwiseComparisonMatrix.csv"))
  }
  return(x)
}
