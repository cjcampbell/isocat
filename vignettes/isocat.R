## ----setup, include = FALSE, cache = FALSE-------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

library(kableExtra)
library(isocat)
library(pvclust)
library(rasterVis)
library(ggplot2)
library(viridisLite)
library(gridExtra)
library(dplyr)


## ----load_isocat---------------------------------------------------------
library(isocat)

## ----load_isoscape_data--------------------------------------------------
myiso <- rasterFromXYZ(isoscape)

## ----load_isoscape_data_2------------------------------------------------
myiso_sd <- rasterFromXYZ(isoscape_sd)

## ----plot_isoscape_data--------------------------------------------------
library(ggplot2,   quietly = TRUE)
library(rasterVis, quietly = TRUE)
library(gridExtra, quietly = TRUE)
gglayers <-  list(
  geom_tile(aes(fill = value)),
  coord_equal(),
  theme_bw(),
  scale_x_continuous(name = "Long", expand = c(0,0)),
  scale_y_continuous(name = "Lat", expand = c(0,0))
)
lab1 <- list(
  gglayers, 
  scale_fill_gradient(name = expression(paste(delta, "D (\u2030)")), low = 'grey10', high = 'grey90')
  )

gridExtra::grid.arrange( 
  gplot(myiso) + lab1 + ggtitle("Example Isoscape"), 
  gplot(myiso_sd) + lab1 + ggtitle("Standard Deviation"),
  ncol = 2
  )
  

## ----example_dataframe---------------------------------------------------
n <- 6 # Number of example rasters
set.seed(1)
df <- data.frame(
  ID = LETTERS[1:n], 
  isotopeValue = sample(cellStats(myiso, "min"):cellStats(myiso, "max"), n, replace = TRUE), 
  SD_indv = rep(5, n),
  stringsAsFactors = FALSE 
  )
kableExtra::kable(df)

## ----prob_of_orgin_surface, fig.width=6, fig.height=3--------------------
assignmentModels <- isotopeAssignmentModel(
  ID = df$ID,
  isotopeValue = df$isotopeValue, 
  SD_indv = df$SD_indv, 
  precip_raster = myiso, 
  precip_SD_raster = myiso_sd, 
  nClusters = FALSE
  )

# Plot.
ggProb <- list(
  facet_wrap(~ variable),
  scale_fill_gradient(name = "Probability\nOf Origin", low = 'darkblue', high = 'yellow') 
  )

gplot(assignmentModels) + gglayers + ggProb

## ----schoenersD----------------------------------------------------------
# Calculate Schoener's D-metric of spatial similarity between 
# two of the example probability surfaces.

schoenersD(assignmentModels[[1]], assignmentModels[[2]])

## ----simmatrix-----------------------------------------------------------
mySimilarityMatrix <- simmatrixMaker(
  assignmentModels,
  nClusters = FALSE,
  csvSavePath = FALSE
)
mySimilarityMatrix

## ----pvclusthelp, eval = F-----------------------------------------------
#  help(pvclust)

## ----clusterSimmatrix, warning = F---------------------------------------
cS <- clusterSimmatrix(
  simmatrix = mySimilarityMatrix,
  dist_mthd = "correlation", 
  hclust_mthd = "average",
  nBoot = 1000,  
  nClusters = FALSE,
  r = seq(.7,1.4,by=.1)
  )
plot(cS)

## ----hclust_instead------------------------------------------------------
hS <- hclust(dist(data.matrix(mySimilarityMatrix)))
plot(hS)

## ----cluster_cutting_code------------------------------------------------
myheight <- 0.05

plot(as.dendrogram(cS$hclust), horiz = FALSE)
abline(h = myheight, col = "red", lwd = 2, lty = 2)

df$cluster <- dendextend::cutree(cS$hclust, h = myheight)

kableExtra::kable(df)

