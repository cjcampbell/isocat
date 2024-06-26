% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterSimmatrix.R
\name{clusterSimmatrix}
\alias{clusterSimmatrix}
\title{Hierarchical clustering analysis of similarity matrix}
\usage{
clusterSimmatrix(
  simmatrix,
  dist_mthd = "correlation",
  hclust_mthd = "average",
  nBoot = 1000,
  nClusters = FALSE,
  r = seq(0.7, 1.4, by = 0.1)
)
}
\arguments{
\item{simmatrix}{symmetric similarity matrix object.}

\item{dist_mthd}{Distance measure to be used. Defaults to "correlation". See help(pvclust).}

\item{hclust_mthd}{Method of clustering. Defaults to "average". See help(pvclust).}

\item{nBoot}{number of bootstrap replications. Defaults to 1000. See help(pvclust).}

\item{nClusters}{number of clusters to run in parallel using 'doParallel'. Defaults to FALSE (non-parallel).}

\item{r}{Relative size of bootstrap replications.}
}
\description{
Function applies hierarchical clustering analysis to similarity matrix, such as one output by 'simmatrixMaker' function. Just a wrapper for pvclust. Output is a pvclust object.
}
\examples{
\donttest{
# Create probability-of-origin maps to compare.
myiso <- rast(isoscape, type="xyz")
plot(myiso)
myiso_sd <- rast(isoscape_sd, type="xyz")
n <- 5
set.seed(42)
df <- data.frame(
         ID = LETTERS[1:n],
         isotopeValue = sample(-120:-40, n),
         SD_indv = rep(5, n)
         )
assignmentModels <- isotopeAssignmentModel(
                        ID = df$ID,
                        isotopeValue = df$isotopeValue,
                        SD_indv = df$SD_indv,
                        precip_raster = myiso,
                        precip_SD_raster = myiso_sd,
                        nClusters = FALSE
                        )
raster::plot(assignmentModels)
# Compare maps with simmatrixMaker.
mymatrix <- schoenersDsimmatrix(assignmentModels)
# Cluster similarity matrix.
clust_results <- clusterSimmatrix(mymatrix, dist_mthd = "correlation",
    hclust_mthd = "average", nBoot = 1000,  nClusters = FALSE,
    r = seq(.7,1.4,by=.1) )
clust_results
}

}
