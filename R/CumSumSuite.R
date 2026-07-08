
#' Cumulative sum below value
#'
#' Function that calculates the cumulative sum of values less than or equal to a given value.
#'
#' @param vals Object of numeric or integer class.
#'
#' @return Returns list of values representing cumulative sum of `val` values less than or equal to the input.
#'
#' @importFrom stats na.omit
#' @importFrom methods is
#'
#' @examples
#' vals <- 1:10
#' cumsumbelow(vals)
#'
#' @export cumsumbelow
#'
#'
cumsumbelow <- function(vals){
  if(!any(is(vals,"numeric"), is(vals, "integer")))
    stop("values entered must be numeric")
  unlist( lapply(vals, function(x,y){
      if(is.na(x)){ NA } else { sum(y[y <= x], na.rm = TRUE) } }, y = na.omit(vals)) )
}



#' Create cumulative sum probability surface
#'
#' Converts normalized probability surface (e.g. one layer output of isotopeAssignmentModel function) to cumulative sum surfaces, i.e., one where the new value of a given cell is equal to the sum of all old values less than or equal to the old value of the cell.
#'
#' @param indivraster Normalized probability surface SpatRaster
#' @param rescale Rescale between 0 and 1? Defaults to FALSE.
#' @param rename Character value to append to raster name (e.g. "_odds"). Defaults to FALSE.
#'
#' @return Returns SpatRaster rescaled to Cumulative Sum values.
#'
#' @aliases cumsum_surface
#'
#' @seealso \code{\link{cumsumAtSamplingLocation}}
#'
#' @importFrom methods is
#'
#' @examples
#' # Generate example probability surfaces. The isoscape is coarsened here so the
#' # example runs quickly; cumsumbelow() scales with the square of the cell count.
#' myiso    <- terra::aggregate(rast(isoscape,    type = "xyz"), 5)
#' myiso_sd <- terra::aggregate(rast(isoscape_sd, type = "xyz"), 5)
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
#' cumulative_sum_surface <- terra::rast(
#'      lapply( terra::as.list( assignmentModels ), makecumsumSurface )
#'      )
#' plot(cumulative_sum_surface)
#'
#' @export makecumsumSurface
#'
makecumsumSurface <- function(indivraster, rescale = FALSE, rename = FALSE){

  if(is(indivraster, "Raster")) indivraster <- terra::rast(indivraster)

  newsurface <- indivraster
  newsurface[] <- cumsumbelow( as.vector(indivraster[]) )

  if(rescale == TRUE){
    # Rescale the cumulative-sum surface to [0, 1]; its minimum and maximum are the
    # cumulative sums at the smallest and largest input cell values.
    r_min <- terra::global(newsurface, "min", na.rm = TRUE)[1, 1]
    r_max <- terra::global(newsurface, "max", na.rm = TRUE)[1, 1]
    newsurface <- (newsurface - r_min) / (r_max - r_min)
  }

  if(rename == FALSE){
    names(newsurface) <- names(indivraster)
  } else {
    if(!is(rename,  "character") )
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
#' @param indivraster SpatRaster representing normalized probability of origin surface
#' @param Lat Integer latitude
#' @param Lon Integer longitude
#'
#' @aliases cumsum_at_point
#' @seealso \code{\link{makecumsumSurface}}
#'
#' @importFrom methods is
#'
#' @examples
#' # Generate example probability surface.
#' myiso <- rast(isoscape, type="xyz")
#' myiso_sd <- rast(isoscape_sd, type="xyz")
#' exampleSurface <- isotopeAssignmentModel(
#'          ID = "A",
#'          isotopeValue = -100,
#'          SD_indv = 5,
#'          precip_raster = myiso,
#'          precip_SD_raster = myiso_sd,
#'          nClusters = FALSE
#'          )
#' # Calculate cumulative sum at specific point.
#' set.seed(1)
#' x <- sample( which( !is.na(exampleSurface[]) ), size = 1)
#' pt <- terra::xyFromCell(exampleSurface, x)
#' cumsumAtSamplingLocation(indivraster = exampleSurface, Lat = pt[2], Lon = pt[1])
#'
#' @export
cumsumAtSamplingLocation <- function(indivraster, Lat, Lon){

  if(is(indivraster, "Raster")) indivraster <- terra::rast(indivraster)

  if(!is(Lat, "numeric") | !is(Lon, "numeric") )
    stop("'Lat' and 'Lon' must both be numeric values.")

  if(is.na(Lat) | is.na(Lon)) {return(NA)} else {

    p_atPoint <- terra::extract(indivraster, cbind(Lon, Lat))[1, 1]

    vals <- stats::na.omit( indivraster[] )
    cumsumAtPoint <- sum( vals[ vals <= p_atPoint ], na.rm = TRUE )

    return(cumsumAtPoint)
  }
}
