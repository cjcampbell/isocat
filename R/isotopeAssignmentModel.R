#' Isotope assignment model function
#'
#' Creates isotope assignment models projections of probable origin. Results returned as a RasterStack, with layer names corresponding to individual ID.
#' @param ID ID value or vector of values (for naming assignment model layers). If missing, will count from 1.
#' @param isotopeValue Isotope precipitation value or vector of values.
#' @param SD_indv error associated with transfer function fit. Value or vector of values. If missing, will assume value of 0.
#' @param precip_raster precipitation isoscape raster.
#' @param precip_SD_raster precipitation isoscape standard deviation raster.
#' @param additionalModel optional additional model RasterLayer (e.g. an SDM, rasterized range map). If specified, function will return isotope assignment rasters and the product of this additionalModel and each assignmentRaster.
#' @param additionalModel_name optional filename for additionalModel .grd path
#' @param savePath If specified, function will save results to this path as a '.grd' file.
#' @param nClusters integer of cores to run in parallel with doParallel. Default FALSE.
#'
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#'
#' @examples
#' myiso <- rasterFromXYZ(isoscape)
#' raster::plot(myiso)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' df <- data.frame(
#'          ID = c(-100, -80, -50),
#'          isotopeValue = c(-100, -80, -50),
#'          SD_indv = rep(5, 3)
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
#'
#' @export isotopeAssignmentModel
#'
#'

isotopeAssignmentModel <- function(ID, isotopeValue, SD_indv, precip_raster, precip_SD_raster, additionalModel = FALSE, additionalModel_name = "CombinedIsotope-OtherModelAssignments", savePath = FALSE, nClusters = FALSE) {

  if(missing(isotopeValue)){
    stop("isotopeValue object not found.")
  }
  if(missing(precip_raster)){
    stop("Precipitation isoscape not found.")
  }
  if(missing(precip_SD_raster)){
    stop("Precipitation isoscape error raster not found.")
  }
  if(missing(ID)) {
    ID <- seq(1, length(isotopeValue), 1)
  }
  if(missing(SD_indv)) {
    SD_indv <- rep(0, length(ID)) }


  if(class(nClusters) == "numeric"){
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package \"doParallel\" needed for this function to work as called.",
           call. = FALSE)
      }

    cl <- parallel::makeCluster(nClusters)
    doParallel::registerDoParallel(cl)

    i <- NULL
    listOfAssigments <- foreach(i = 1:length(isotopeValue), .packages="raster") %dopar% {
      # Calculate total error.
      totError <- sqrt((precip_SD_raster)^2 + SD_indv[i]^2)

      # Assignment function.
      assign <- (1 / sqrt(2 * pi * totError^2)) * exp(-1 * (isotopeValue[i] - precip_raster)^2 / (2 * totError^2))

      # Normalize to sum to 1.
      assign_norm <- assign/raster::cellStats(assign, "sum")

      names(assign_norm) <- paste0(ID[i])

      if(class(additionalModel) == "RasterLayer"){
        if(class(additionalModel) != "logical"){
          if(missing(additionalModel))
            stop("additionalModel raster not found.")
          if(class(additionalModel) != "RasterLayer")
            stop("additionalModel class not 'RasterLayer'.")

          # Bring in additionalModel
          combo_prod <- prod(assign_norm, additionalModel)
          combo_norm <- combo_prod / raster::cellStats(combo_prod, "sum")
          names(combo_norm) <- names(assign_norm)

          return(list(isotopeAssignments = assign_norm, comboAssignments = combo_norm))
        }
      } else {
        return(list(isotopeAssignments = assign_norm))
      }

    }
    parallel::stopCluster(cl)

  } else{

    listOfAssigments <- lapply(1:length(isotopeValue), function(i){
      # Calculate total error.
      totError <- sqrt((precip_SD_raster)^2 + SD_indv[i]^2)

      # Assignment function.
      assign <- (1 / sqrt(2 * pi * totError^2)) * exp(-1 * (isotopeValue[i] - precip_raster)^2 / (2 * totError^2))

      # Normalize to sum to 1.
      assign_norm <- assign/raster::cellStats(assign, "sum")

      names(assign_norm) <- paste0(ID[i])


      if(class(additionalModel) == "RasterLayer"){

        # Bring in additionalModel
        combo_prod <- prod(assign_norm, additionalModel)
        combo_norm <- combo_prod / raster::cellStats(combo_prod, "sum")
        names(combo_norm) <- names(assign_norm)

        return(list(isotopeAssignments = assign_norm, comboAssignments = combo_norm))

      } else {
        return(list(isotopeAssignments = assign_norm))
      }
    })
  }

  stackOfAssignments <-  raster::stack(lapply(listOfAssigments, function(x) x[["isotopeAssignments"]]))

  if(class(additionalModel) == "RasterLayer"){
    stackOfCombinations <- raster::stack(lapply(listOfAssigments, function(x) x[["comboAssignments"]]))
  }

  if(savePath != FALSE){
    raster::writeRaster(x = stackOfAssignments,
                filename = file.path(savePath, "IsotopeAssignments.grd"), format = "raster",
                overwrite = TRUE)
    if(class(additionalModel) == "RasterLayer"){
      raster::writeRaster(x = stackOfCombinations,
                  filename = file.path(savePath, paste0( additionalModel_name, ".grd")), format = "raster",
                  overwrite = TRUE)
    }
    return("Success!")
  } else {
    if(class(additionalModel) == "RasterLayer"){
      return(list(
        isotopeAssignments = stackOfAssignments,
        productOfAdditionalModel = stackOfCombinations))
    } else { return(stackOfAssignments)
    }
  }
}
