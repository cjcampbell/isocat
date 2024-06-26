% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeOddsSurface.R
\name{makeOddsSurfaces}
\alias{makeOddsSurfaces}
\alias{odds_surface}
\title{Convert probability surface to odds-ratio surface}
\usage{
makeOddsSurfaces(probabilitySurface, rename = FALSE)
}
\arguments{
\item{probabilitySurface}{Normalized probability surface RasterLayer}

\item{rename}{Character value to append to raster name (e.g. "_odds"). Defaults to FALSE.}
}
\value{
Returns RasterLayer rescaled to Odds Ratio values.
}
\description{
Converts normalized probability surface (e.g. one layer output of isotopeAssignmentModel function) to Odds Ratio surfaces.
}
\examples{
# Generate example probability surfaces.
myiso <- rast(isoscape, type="xyz")
myiso_sd <- rast(isoscape_sd, type="xyz")
df <- data.frame(
         ID = c(-100, -80, -50),
         isotopeValue = c(-100, -80, -50),
         SD_indv = rep(5, 3)
         )
assignmentModels <- isotopeAssignmentModel(
         ID = df$ID,
         isotopeValue = df$isotopeValue,
         SD_indv = df$SD_indv,
         precip_raster = myiso,
         precip_SD_raster = myiso_sd,
         nClusters = FALSE
         )

# Convert to odds ratio surfaces.
odds_ratio_surface <- stack(
   lapply( unstack(assignmentModels), makeOddsSurfaces )
   )
plot(odds_ratio_surface)

}
\seealso{
\code{\link{oddsAtSamplingLocation}}
}
