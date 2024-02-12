#' Make mean multi-month isoscape and error surfaces.
#'
#' Converts a stack of monthly isoscape maps, monthly isoscape standard error
#' maps, and optionally a precipitation (amount) stack. Each stack must contain
#' layers corresponding to each of the target months.
#'
#' If precip_stack is NULL, model will assume equal precipitation amounts per month.
#'
#' @param iso_stack A RasterStack containing n layers corresponding to isoscape models for n months.
#' @param iso_se_stack A RasterStack containing n layers corresponding to isoscape standard error maps for n months.
#' @param precip_stack Either a RasterStack containing n layers corresponding to precipitation amounts for n months, or NULL (assumes equal precipitation amounts.)
#'
#' @return A list containing a mean isoscape and root-sum-of-square error map
#'
#' @importFrom raster cellStats
#' @importFrom raster calc
#' @importFrom raster compareRaster
#' @importFrom raster nlayers
#'
#' @export
makeMultiMonthIsoscape <- function(iso_stack, iso_se_stack, precip_stack = NULL) {

  stopifnot(
    class(iso_stack) == "RasterStack",
    class(iso_se_stack) == "RasterStack",
    is.null(precip_stack) | class(precip_stack) == "RasterStack",
    raster::nlayers(iso_stack) == raster::nlayers(iso_se_stack),
    is.null(precip_stack) | raster::nlayers(iso_stack) == raster::nlayers(precip_stack),
    raster::compareRaster(iso_stack, iso_se_stack, stopiffalse = F)
  )

  if(!is.null(precip_stack)) {
    precip_sum <- raster::calc(precip_stack, sum)
    precip_relativized <- precip_stack/precip_sum

    if(!raster::compareRaster(precip_relativized, iso_stack[[1]], stopiffalse = F)) {

      # Resample if rowcol do not match.
      if( !raster::compareRaster(precip_relativized, iso_stack[[1]],
                       extent=F, rowcol=TRUE, crs=F, res=F, orig=F,
                       rotation=F, values=FALSE, stopiffalse=F,
                       showwarning=FALSE) ) {
        precip_relativized <- raster::resample(precip_relativized, iso_stack[[1]] )
      }

      # Crop and extend if extents do not match.
      if( !raster::compareRaster(precip_relativized, iso_stack[[1]],
                         extent=TRUE, rowcol=FALSE, crs=FALSE, res=FALSE,
                         orig=FALSE, rotation=FALSE, values=FALSE,
                         stopiffalse=FALSE, showwarning=FALSE) ) {
        precip_relativized <-    raster::crop(
          raster::extend(precip_relativized, iso_stack[[1]] ),
          iso_stack[[1]]
        )
      }

      stopifnot(
        "Rasterstacks are not compatible. Use raster::compareRaster to examine
        the differences between iso_stack and precip_stack." =
        raster::compareRaster(precip_relativized, iso_stack)
        )

    }

  } else {
    precip_relativized <- iso_stack
    precip_relativized[] <- 1/nlayers(iso_stack)
  }

  # Normalize precipitation isoscape values for each cell while correcting
  # for precipitation amounts.
  stack1 <-  precip_relativized * iso_stack
  mean_iso <- raster::calc(stack1, fun = mean)

  # Calculate square of errors.
  iso_se <- raster::overlay(iso_se_stack, fun = function(...) { sqrt( sum(...^2) ) } )

  return(list(mean_iso = mean_iso, iso_se = iso_se))

}
