## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(kableExtra)

## ----load isocat---------------------------------------------------------
library(isocat)

## ----load isoscape data--------------------------------------------------
data(isoscape)
myiso <- rasterFromXYZ(isoscape)
raster::plot(myiso)

## ----load isoscape data 2------------------------------------------------
myiso_sd <- rasterFromXYZ(isoscape_sd)

## ----example dataframe---------------------------------------------------
df <- data.frame(ID = LETTERS[1:3], dD = c(-100, -80, -50), SD_indv = rep(5, 3))
kable(df)

## ----prob of orgin surface-----------------------------------------------
assignmentModels <- isotopeAssignmentModel(
  ID = df$ID, 
  dD = df$dD, 
  SD_indv = df$SD_indv, 
  precip_raster = myiso, 
  precip_SD_raster = myiso_sd, 
  nClusters = FALSE
  )
raster::plot(assignmentModels)

## ----schoenersD----------------------------------------------------------
# Calculate Schoener's D-metric of spatial similarity between two of the example probability surfaces.
schoenersD(assignmentModels[[1]], assignmentModels[[2]])

## ----simmatrix-----------------------------------------------------------
simmatrixMaker(
  assignmentModels,
  nClusters = FALSE,
  csvSavePath = FALSE
)

