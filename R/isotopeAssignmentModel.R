#' Isotope assignment model function
#'
#' Creates isotope assignment models projections of probable origin. Results returned as a rasterstack, with layer names corresponding to individual ID.
#' @param ID ID value or vector of values (for naming assignment model layers). If missing, will count from 1.
#' @param dD Isotope precipitation value or vector of values.
#' @param SD_indv error associated with transfer function fit. Value or vector of values. If missing, will assume value of 0.
#' @param precip_raster precipitation isoscape raster.
#' @param precip_SD_raster precipitation isoscape standard deviation raster.
#' @param additionalModel optional additional model raster (e.g. an SDM or rasterized rangemap). If specified, function will return isotope assignment rasters and the product of this additionalModel and each assignmentRaster.
#' @param savePath If specified, function will save results to this path as a .grd.
#' @param nClusters integer of cores to run in parallel with doParallel. Default FALSE.
#' @examples
#' data(isoscape)
#' myiso <- rasterFromXYZ(isoscape)
#' raster::plot(myiso)
#' myiso_sd <- rasterFromXYZ(isoscape_sd)
#' df <- data.frame(ID = c(-100, -80, -50), dD = c(-100, -80, -50), SD_indv = rep(5, 3))
#' assignmentModels <- isotopeAssignmentModel(ID = df$ID, dD = df$dD, SD_indv = df$SD_indv, precip_raster = myiso, precip_SD_raster = myiso_sd, nClusters = FALSE)
#' raster::plot(assignmentModels)
#'

isotopeAssignmentModel <- function(ID, dD, SD_indv, precip_raster, precip_SD_raster, additionalModel = FALSE, savePath = FALSE, nClusters = FALSE) {

  if(missing(dD)){
    stop("dD value not found.")
  }
  if(missing(precip_raster)){
    stop("Precipitation isoscape not found.")
  }
  if(missing(precip_SD_raster)){
    stop("Precipitation isoscape error raster not found.")
  }
  if(missing(ID)) {
    ID <- seq(1, length(dD), 1)
  }
  if(missing(SD_indv)) {
    SD_indv <- rep(0, length(ID)) }


  if(class(nClusters) == "numeric"){

    ifelse(
      "doParallel" %in% rownames(installed.packages()),
      require(doParallel),
      stop("This function applies library 'doParallel' for parallel processing. Please install this package.")
    )
    ifelse(
      "raster" %in% rownames(installed.packages()),
      require(raster),
      stop("This function applies library 'raster'. Please install this package.")
    )

    cl <- makeCluster(nClusters)
    registerDoParallel(cl)

    listOfAssigments <- foreach(i = 1:length(dD), .packages="raster") %dopar% {
      # Calculate total error.
      totError <- sqrt((precip_SD_raster)^2 + SD_indv[i]^2)

      # Assignment function.
      assign <- (1 / sqrt(2 * pi * totError^2)) * exp(-1 * (dD[i] - precip_raster)^2 / (2 * totError^2))

      # Normalize to sum to 1.
      assign_norm <- assign/cellStats(assign, "sum")

      names(assign_norm) <- paste0(ID[i])

      if(class(additionalModel) == "RasterLayer"){
        if(class(additionalModel) != "logical"){
          if(missing(additionalModel))
            stop("additionalModel raster not found.")
          if(class(additionalModel) != "RasterLayer")
            stop("additionalModel class not 'RasterLayer'.")

          # Bring in additionalModel
          combo_prod <- prod(assign_norm, additionalModel)
          combo_norm <- combo_prod / cellStats(combo_prod, "sum")

          return(list(isotopeAssignments = assign_norm, comboAssignments = combo_norm))
        }
      } else {
        return(list(isotopeAssignments = assign_norm))
      }

    }
    stopCluster(cl)

  } else{

    listOfAssigments <- lapply(1:length(dD), function(i){
      # Calculate total error.
      totError <- sqrt((precip_SD_raster)^2 + SD_indv[i]^2)

      # Assignment function.
      assign <- (1 / sqrt(2 * pi * totError^2)) * exp(-1 * (dD[i] - precip_raster)^2 / (2 * totError^2))

      # Normalize to sum to 1.
      assign_norm <- assign/cellStats(assign, "sum")

      names(assign_norm) <- paste0(ID[i])


      if(class(additionalModel) == "RasterLayer"){

        # Bring in additionalModel
        combo_prod <- prod(assign_norm, additionalModel)
        combo_norm <- combo_prod / cellStats(combo_prod, "sum")

        return(list(isotopeAssignments = assign_norm, comboAssignments = combo_norm))

      } else {
        return(list(isotopeAssignments = assign_norm))
      }
    })
  }

  stackOfAssignments <-  stack(lapply(listOfAssigments, function(x) x[["isotopeAssignments"]]))

  if(class(additionalModel) == "RasterLayer"){
    stackOfCombinations <- stack(lapply(listOfAssigments, function(x) x[["comboAssignments"]]))
  }

  if(savePath != FALSE){
    writeRaster(x = stackOfAssignments,
                filename = paste0(savePath, "IsotopeAssignments.grd"), format = "raster",
                overwrite = TRUE)
    if(class(additionalModel) == "RasterLayer"){
      writeRaster(x = stackOfCombinations,
                  filename = paste0(savePath, "CombinedIsotope-OtherModelAssignments.grd"), format = "raster",
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
