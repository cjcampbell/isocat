
#' Cumulative sum below value
#'
#' Function that calculates the cumulative sum of values less than or equal to a given value.
#'
#' @param vals Object of numeric or integer class.
#'
#' @return Returns list of values representing cumulative sum of `val` values less than or equal to the input.
#'
#' @importFrom stats na.omit
#'
#' @examples
#' vals <- 1:10
#' cumsumbelow(vals)
#'
#' @export
cumsumbelow <- function(vals){
  if(class(vals) != "numeric" & class(vals) != "integer")
    stop("values entered must be numeric")
  unlist( lapply(vals, function(x,y){
      if(is.na(x)){ NA } else { sum(y[y <= x], na.rm = TRUE) } }, y = na.omit(vals)) )
}



#' Create cumulative sum probability surface
#'
#' Converts normalized probability surface (e.g. one layer output of isotopeAssignmentModel function) to cumulative sum surfaces, i.e., one where the new value of a given cell is equal to the sum of all old values less than or equal to the old value of the cell.
#'
#' @param indivraster Normalized probability surface RasterLayer
#' @param rescale Rescale between 0 and 1? Defaults to FALSE.
#' @param rename Character value to append to raster name (e.g. "_odds"). Defaults to FALSE.
#'
#' @return Returns RasterLayer rescaled to Cumulative Sum values.
#'
#' @aliases cumsum_surface
#'
#' @seealso \code{\link{cumsumAtSamplingLocation}}
#'
#' @inheritParams cumsumbelow
#'
#' @examples
#' # Generate example probability surfaces.
#' myiso <- rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' df <- data.frame(
#'         ID = c(-100, -80, -50),
#'         isotopeValue = c(-100, -80, -50),
#'         SD_indv = rep(5, 3)
#'         )
#' assignmentModels <- isotopeAssignmentModel(
#'         ID = df$ID,
#'         isotopeValue = df$isotopeValue,
#'         SD_indv = df$SD_indv,
#'         precip_raster = myiso,
#'         precip_SD_raster = myiso_sd,
#'         nClusters = FALSE
#'         )
#'
#' # Convert to cumulative sum surface.
#' cumulative_sum_surface <- stack(
#'      lapply( unstack( assignmentModels ), makecumsumSurface )
#'      )
#' plot(cumulative_sum_surface)
#'
#' @export
makecumsumSurface <- function(indivraster, rescale = FALSE, rename = FALSE){

  . <- "quiet" # silence 'no visible binding for global variable' call.
  vals <- NULL

  newsurface <- indivraster
  newsurface[] <- cumsumbelow( indivraster[] )

  if(rescale == TRUE){
    new.min <- 0
    new.max <- 1
    x.min <- sum(
      vals[ vals <= raster::cellStats(indivraster, "min") ] ,
      na.rm = TRUE
      )
    x.max <- sum(
      vals[ vals <= raster::cellStats(indivraster, "max") ],
      na.rm = TRUE
      )
    newsurface <- new.min + (newsurface - x.min) * ((new.max - new.min) / (x.max - x.min))
  }

  if(rename == FALSE){
    names(newsurface) <- names(indivraster)
  } else {
    if(class(rename) != "character")
      stop("argument 'rename' should be of character class.")
    names(newsurface) <- paste0(names(indivraster), rename)
  }

  return( newsurface )
}



#' Cumulative sum at coordinates
#'
#'
#' Function estimates cumulative sum of all values in a surface below the value at a specified longitude and latitude.
#'
#' @param indivraster RasterLayer representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @aliases cumsum_at_point
#' @seealso \code{\link{makecumsumSurface}}
#'
#' @examples
#' # Generate example probability surface.
#' myiso <- raster::rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' exampleSurface <- isotopeAssignmentModel(
#'          ID = "A",
#'          isotopeValue = -100,
#'          SD_indv = 5,
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd,
#'          nClusters = FALSE
#'          )
#' # Calculate odds ratio at specific point.
#' set.seed(1)
#' x <- sample( which( !is.na(exampleSurface[]) ), size = 1)
#' pt <- raster::xyFromCell(exampleSurface, x)
#' cumsumAtSamplingLocation(indivraster = exampleSurface, Lat = pt[2], Lon = pt[1])
#'
#' @export
cumsumAtSamplingLocation <- function(indivraster, Lat, Lon){
  if(!is.numeric(Lat) | !is.numeric(Lon))
    stop("'Lat' and 'Lon' must both be numeric values.")

  if(is.na(Lat) | is.na(Lon)) {return(NA)} else {

    indivcoords <- sp::SpatialPoints(cbind(Lon,Lat))
    p_atPoint <- raster::extract(indivraster, indivcoords)

    if( "matrix" %in% class(p_atPoint) | "array" %in% class(p_atPoint) ){
      if( length(p_atPoint) != 1) {
        stop("extracted value at coordinates must be of length one.")
      } else
        p_atPoint <- as.numeric( p_atPoint )
    }

    vals <- stats::na.omit( indivraster[] )
    cumsumAtPoint <- vals[ vals <= p_atPoint ] %>% sum(na.rm = TRUE)

    return(cumsumAtPoint)
  }
}
