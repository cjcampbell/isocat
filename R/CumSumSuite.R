
#' Cumulative sum below value
#'
#' Function that calculates the cumulative sum of values less than or equal to a given value.
#'
#' @param vals Object of numeric or integer class.
#'
#' @return Returns list of values representing cumulative sum of `val` values less than or equal to the input.
#'
#'
#' @examples
#' vals <- 1:10
#' cumsumbelow(vals)
#'
#' @export
cumsumbelow <- function(vals){
  if(class(vals) != "numeric" & class(vals) != "integer") stop("values entered must be numeric")
  vals %>% purrr::map(~ vals[ vals <= . ] %>% sum(na.rm = T) )
}



#' Create cumulative sum probability surface
#'
#' Converts normalized probability surface (e.g. one layer output of isotopeAssignmentModel function) to cumulative sum surfaces, i.e., one where the new value of a given cell is equal to the sum of all old values less than or eual to the old value of the cell.
#'
#' @param probabilitySurface Normalized probability surface RasterLayer
#' @param rename Character value to append to raster name (e.g. "_odds"). Defaults to FALSE.
#'
#' @return Returns rasterLayer rescaled to Cumulative Sum values.
#'
#' @inheritParams cumsumbelow
#'
#' @examples
#' # Generate example probability surfaces.
#' data(isoscape)
#' myiso <- rasterFromXYZ(isoscape)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' df <- data.frame(ID = c(-100, -80, -50), dD = c(-100, -80, -50), SD_indv = rep(5, 3))
#' assignmentModels <- isotopeAssignmentModel(ID = df$ID, dD = df$dD, SD_indv = df$SD_indv, precip_raster = myiso, precip_SD_raster = myiso_sd, nClusters = FALSE)
#'
#' # Convert to cumulative sum surface.
#' cumulative_sum_surface <- lapply(unstack(assignmentModels), makecumsumSurface) %>% stack
#' plot(cumulative_sum_surface)
#'
#' @export
makecumsumSurface <- function(indivraster, rescale = FALSE, rename = FALSE){

  newsurface <- indivraster
  newsurface[] <- indivraster[] %>%
    cumsumbelow(.) %>%
    unlist

  if(rescale == TRUE){
    new.min <- 0
    new.max <- 1
    x.min <- vals[ vals <= raster::cellStats(indivraster, "min") ] %>% sum(na.rm = T)
    x.max <- vals[ vals <= raster::cellStats(indivraster, "max") ] %>% sum(na.rm = T)
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
#'
#'
#' @param indivraster rasterlayer representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @export
cumsumAtSamplingLocation <- function(indivraster, Lat, Lon){
  if(!is.numeric(Lat) | !is.numeric(Lon))
    stop("'Lat' and 'Lon' must both be numeric values.")

  if(is.na(Lat) | is.na(Lon)) {return(NA)} else {

    indivcoords <- SpatialPoints(cbind(Lon,Lat))
    p_atPoint <- raster::extract(indivraster, indivcoords)

    vals <- na.omit( indivraster[] )
    cumsumAtPoint <- vals[ vals <= p_atPoint ] %>% sum(na.rm = T)

    return(cumsumAtPoint)
  }
}