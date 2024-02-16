#' Find Product Then Normalize Function
#'
#' @keywords internal

#' @importFrom raster cellStats

#' @export .findProductThenNormalize

.findProductThenNormalize <- function(...){
  a <- list(...)
  if(length(a) == 1) {
    b <- a[[1]]
  } else {
    b <- do.call("prod", a)
  }
  c <- b / raster::cellStats(b, "sum")
  return(c)
}


#' Make probability-of-origin surfaces
#'
#' @param .ID ID value or vector of values (for naming assignment model layers). If missing, will count from 1.
#' @param .isotopeValue Isotope precipitation value or vector of values.
#' @param .SD_indv error associated with transfer function fit. Value or vector of values. If missing, will assume value of 0.
#' @param precip_raster precipitation isoscape raster.
#' @param precip_SD_raster precipitation isoscape standard deviation raster.
#'
#' @keywords internal
#'
#' @export .assignmentMaker

.assignmentMaker <- function(.ID, .isotopeValue, .SD_indv, precip_raster, precip_SD_raster){
  # Calculate total error.
  totError <- sqrt((precip_SD_raster)^2 + .SD_indv^2)
  # Assignment function.
  assign <- (1 / sqrt(2 * pi * totError^2)) * exp(-1 * (.isotopeValue - precip_raster)^2 / (2 * totError^2))

  # Normalize to sum to 1.
  assign_norm <- .findProductThenNormalize(assign)

  names(assign_norm) <- paste0(.ID)
  return(assign_norm)
}


#' Compare rasters by checking whether they can stack.
#'
#' @keywords internal
#'
#' @export .compareMyRasters

.compareMyRasters <- function(...){
  tryCatch(
    expr={ raster::stack(...)},
    error = function(e){
      stop("Rasters included as arguments don't match up.
             See '?compareRaster' for more. Double check precip_raster,
             precip_SD_raster, and additionalModels!")
    }
  )
}



#' Isotope assignment model function
#'
#' Creates isotope assignment models projections of probable origin. Results returned as a RasterStack, with layer names corresponding to individual ID.
#' @param ID ID value or vector of values (for naming assignment model layers). If missing, will count from 1.
#' @param isotopeValue Isotope precipitation value or vector of values.
#' @param SD_indv error associated with transfer function fit. Value or vector of values. If missing, will assume value of 0.
#' @param precip_raster precipitation isoscape raster.
#' @param precip_SD_raster precipitation isoscape standard deviation raster.
#' @param additionalModels optional additional model raster object (e.g. an SDM, rasterized range map, or stack thereof). If specified, function will return isotope assignment rasters and the product of these additionalModels and each assignmentRaster.
#' @param additionalModel_name optional filename for additionalModel .grd savepath
#' @param savePath If specified, function will save results to this path as a '.grd' file.
#' @param nClusters integer of cores to run in parallel with doParallel. Default FALSE.
#'
#' @importFrom parallel mcmapply
#' @importFrom foreach %dopar%
#' @importFrom methods is
#'
#' @examples
#' myiso <- rasterFromXYZ(isoscape)
#' raster::plot(myiso)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' df <- data.frame(
#'          ID = paste0("Example.", 1:3),
#'          isotopeValue = c(-100, -80, -50),
#'          SD_indv = rep(5, 3)
#'          )
#' assignmentModels <- isotopeAssignmentModel(
#'                         ID = df$ID,
#'                         isotopeValue = df$isotopeValue,
#'                         SD_indv = df$SD_indv,
#'                         precip_raster = myiso,
#'                         precip_SD_raster = myiso_sd
#'                         )
#' raster::plot(assignmentModels)
#'
#'# Add additionalModels:
#' range_raster <- myiso
#' range_raster[] <- as.numeric( 1:ncell(myiso) %% 60 >= 10)
#' plot(range_raster)
#'
#' sdm_raster <- myiso
#' sdm_raster[] <- (1:ncell(sdm_raster))^2
#' sdm_raster <- sdm_raster / raster::cellStats(sdm_raster, "sum")
#' plot(sdm_raster)
#'
#' extraModels <- raster::stack(range_raster, sdm_raster)
#' assignmentModels <- isotopeAssignmentModel(
#'                         ID = paste0("Combo.",df$ID),
#'                         isotopeValue = df$isotopeValue,
#'                         SD_indv = df$SD_indv,
#'                         precip_raster = myiso,
#'                         precip_SD_raster = myiso_sd,
#'                         additionalModels = extraModels
#'                         )
#' raster::plot(assignmentModels)
#'
#'
#'
#' @export isotopeAssignmentModel



isotopeAssignmentModel <- function(ID, isotopeValue, SD_indv = 0, precip_raster, precip_SD_raster, additionalModels = FALSE, additionalModel_name = "CombinedIsotope-OtherModelAssignments", savePath = FALSE, nClusters = FALSE) {

  #### Tests -------------------------------------------------------------------
  if( missing(isotopeValue) ) {   stop("isotopeValue object not found.") }
  if( missing(precip_raster) ) {  stop("Precipitation isoscape not found.") }
  if( missing(precip_SD_raster)){
    stop("Precip isoscape error raster not found.")
  }
  if( missing(ID) ) {             ID <- seq(1, length(isotopeValue), 1)  }
  if( length(SD_indv == 0) & SD_indv[1] == 0 ) {
    SD_indv <- rep(0, length(ID))
  }

  checkTheseRasters <- list(precip_raster = precip_raster, precip_SD_raster = precip_SD_raster)
  if( !is(additionalModels, "logical") ) {
    checkTheseRasters$additionalModels <- additionalModels
  }
  .compareMyRasters(checkTheseRasters)

  # If parallel is to be used.
  if( is(nClusters, "numeric") ) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Package \"parallel\" needed for this function to work when
           nClusters argument specified.",
           call. = FALSE)
    }
  }

  # Setup. ##
  ID0 <- make.names(ID)
  if( all(ID0 == ID) == FALSE ) warning("ID values will be transformed on output. See '?make.names' for more.")


  #### Apply -------------------------------------------------------------------


  # Without running in parallel. ------------------
  if( !is(nClusters,"numeric") ){

    ## Make assignments
    listOfAssigments <- mapply(
      FUN = .assignmentMaker,
      ID0,  isotopeValue, SD_indv,
      MoreArgs =  list( precip_raster = precip_raster, precip_SD_raster = precip_SD_raster )
    )
    stackOfAssignments <-  raster::stack(listOfAssigments)

    ### Save
    if( savePath != FALSE ) {
      raster::writeRaster(
        x = stackOfAssignments,
        filename = file.path(savePath, "IsotopeAssignments.grd"),
        format = "raster",
        overwrite = TRUE
      )
    }

    ## Make combo models.
    if(!is(additionalModels, "logical") ) {
      comboAssignments <- mapply(
        FUN = .findProductThenNormalize,
        listOfAssigments,
        MoreArgs = list(additionalModels)
      )
      stackOfCombinations <- raster::stack(comboAssignments)

      ### Save
      if( savePath != FALSE ) {
        raster::writeRaster(
          x = stackOfCombinations,
          filename = file.path(savePath, paste0( additionalModel_name, ".grd")),
          format = "raster",
          overwrite = TRUE
        )
      }

      return(stackOfCombinations)
    } else {
      return(stackOfAssignments)
    }
  }

  # With running in parallel. -----------------

  if( is(nClusters, "numeric") ) {

    ## Make assignments
    cl <- parallel::makeCluster(nClusters)

    listOfAssigments <- parallel::mcmapply(
      FUN = .assignmentMaker,
      ID0, isotopeValue, SD_indv,
      MoreArgs =  list( precip_raster, precip_SD_raster ))

    parallel::stopCluster(cl)

    stackOfAssignments <-  raster::stack(listOfAssigments)

    ### Save
    if( savePath != FALSE ) {
      raster::writeRaster(
        x = stackOfAssignments,
        filename = file.path(savePath, "IsotopeAssignments.grd"),
        format = "raster",
        overwrite = TRUE
      )
    }

    ## Make combo models.
    if(!is(additionalModels, "logical") ) {
      cl <- parallel::makeCluster(nClusters)
      comboAssignments <- mcmapply(
        FUN = .findProductThenNormalize,
        listOfAssigments,
        MoreArgs = list(additionalModels)
      )
      parallel::stopCluster(cl)
      stackOfCombinations <- raster::stack(comboAssignments)

      ### Save
      if( savePath != FALSE ) {
        raster::writeRaster(
          x = stackOfCombinations,
          filename = file.path(savePath, paste0( additionalModel_name, ".grd")),
          format = "raster",
          overwrite = TRUE
        )
      }
      return(stackOfCombinations)
    } else {
      return(stackOfAssignments)
    }

  }

}
