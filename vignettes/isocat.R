## ----load isocat---------------------------------------------------------
library(isocat)

## ----load isoscape data,  fig.width=6, fig.height=4----------------------
data(isoscape)
myiso <- rasterFromXYZ(isoscape)

## ----load isoscape data 2------------------------------------------------
data(isoscape_sd)
myiso_sd <- rasterFromXYZ(isoscape_sd)

## ----plot isoscape data--------------------------------------------------
library(ggplot2, quietly = T); library(rasterVis, quietly = T); library(gridExtra, quietly = T)
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
  

## ----example dataframe---------------------------------------------------
n <- 6 # Number of example rasters
set.seed(1)
df <- data.frame(
  ID = LETTERS[1:n], 
  dD = sample(cellStats(myiso, "min"):cellStats(myiso, "max"), n, replace = T), 
  SD_indv = rep(5, n)
  )
kableExtra::kable(df)

## ----prob of orgin surface, fig.width=6, fig.height=6--------------------
assignmentModels <- isotopeAssignmentModel(
  ID = df$ID,
  dD = df$dD, 
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
# Calculate Schoener's D-metric of spatial similarity between two of the example probability surfaces.
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

## ----hclust instead------------------------------------------------------
hS <- hclust(dist(data.matrix(mySimilarityMatrix)))
plot(hS)

## ----cluster cutting code------------------------------------------------
myheight <- 0.05

plot(as.dendrogram(cS$hclust), horiz = F)
abline(h = myheight, col = "red", lwd = 2, lty = 2)

df$cluster <- dendextend::cutree(cS$hclust, h = myheight)

kableExtra::kable(df)

## ----Create mean aggregate surfaces, eval = T----------------------------
meanSurfaces <- meanAggregateClusterProbability( 
  indivIDs = df$ID, 
  clusters = df$cluster, 
  surfaces = assignmentModels, 
  nClust = FALSE 
  )

gplot(meanSurfaces) + gglayers + ggProb

## ----summary surface-----------------------------------------------------
summaryMap <- projectSummaryMaxSurface(surfaces = meanSurfaces, nClust = FALSE)

gplot(summaryMap) + 
  gglayers +
  scale_fill_viridis_c(name = "Cluster")

## ----example surface-----------------------------------------------------
set.seed(42)
p <- isotopeAssignmentModel(
  ID = "Example",
  dD = sample(-125:-25, 1), 
  SD_indv = 5, 
  precip_raster = myiso, 
  precip_SD_raster = myiso_sd, 
  nClusters = FALSE
  )[[1]]

# Example Point
pt <- data.frame(x = -103, y = 48)
ptDeets <- list(
  geom_point(
    data = pt, 
    col = "red", shape = 1, size = 2,
    aes(x = x, y = y)
    )
)

ex_plot <- gplot(p) + gglayers + ggProb + ptDeets

ex_hist <- data.frame(x = p[]) %>% 
  ggplot(.) +
  geom_density(aes(x = x)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = "Probability Value") +
  theme_bw() +
  geom_vline(aes(xintercept = extract(p, pt)), linetype = "dashed", col = "red")

gridExtra::grid.arrange(ex_plot, ex_hist, ncol = 2, widths = c(2,1)) 

## ----make cumulative sum surface, eval = T-------------------------------
CumSumEx <- makecumsumSurface(p)

## ----plot cumulative sum surface, eval = T-------------------------------
cumsum_plot <- gplot(CumSumEx) + 
  gglayers + ptDeets +
  scale_fill_gradient(
    name = "Cumulative Sum\nProbability\nOf Origin", low = 'darkblue', high = 'yellow') 

cumsum_hist <- data.frame(x = CumSumEx[]) %>% 
  ggplot(.) +
  geom_density(aes(x = x)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = "Cumulative Sum\nProbability Value") +
  theme_bw() +
  geom_vline(aes(xintercept = extract(CumSumEx, pt)), linetype = "dashed", col = "red")

gridExtra::grid.arrange( cumsum_plot, cumsum_hist, ncol = 2, widths = c(2,1) ) 

## ----odds ratio surface, eval = T----------------------------------------
OddsRatioEx <- makeOddsSurfaces(p)

## ----eval odds ratio surface---------------------------------------------
odds_plot <- gplot(OddsRatioEx) + 
  gglayers + ptDeets +
  scale_fill_gradient(
    name = "Odds-Ratio\nProbability\nOf Origin", low = 'darkblue', high = 'yellow') 

odds_hist <- data.frame(x = OddsRatioEx[]) %>% 
  ggplot(.) +
  geom_density(aes(x = x)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = "Odds Ratio Value") +
  theme_bw() +
  geom_vline(aes(xintercept = extract(OddsRatioEx, pt)), linetype = "dashed", col = "red")

gridExtra::grid.arrange( odds_plot, odds_hist, ncol = 2, widths = c(2,1) ) 

## ----quantile surface, eval = T------------------------------------------
QuantileEx <- makeQuantileSurfaces(p)

## ----eval quantile surface-----------------------------------------------
quantile_plot <- gplot(QuantileEx) + 
  gglayers + ptDeets +
  scale_fill_gradient(
    name = "Quantile\nProbability\nOf Origin", low = 'darkblue', high = 'yellow') 

quantile_hist <- data.frame(x = QuantileEx[]) %>% 
  ggplot(.) +
  geom_density(aes(x = x)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = "Quantile Value") +
  theme_bw() +
  geom_vline(aes(xintercept = extract(QuantileEx, pt)), linetype = "dashed", col = "red")

gridExtra::grid.arrange( quantile_plot, quantile_hist, ncol = 2, widths = c(2,1) ) 

## ----quantsim values, eval = T-------------------------------------------
q <- rweibull(20000, 6, .98)
q <- sample( q[ q >=0 & q <= 1 ], 10000, replace = T)
hist(q)

## ----quantsim surface, eval = T------------------------------------------
QuantSimEx <- makeQuantileSimulationSurface(
  probabilitySurface = p, 
  ValidationQuantiles = q,
  rename = FALSE, rescale = TRUE
  )

## ----eval quantsim surface, eval = T-------------------------------------
quantsim_plot <- gplot(QuantSimEx) + 
  gglayers + ptDeets +
  scale_fill_gradient(
    name = "Quantile-Simulation\nProbability\nOf Origin", low = 'darkblue', high = 'yellow') 

quantsim_hist <- data.frame(x = QuantSimEx[]) %>% 
  ggplot(.) +
  geom_density(aes(x = x)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = "Quantile-Simulation Value") +
  theme_bw() +
  geom_vline(aes(xintercept = extract(QuantSimEx, pt)), linetype = "dashed", col = "red")

gridExtra::grid.arrange( quantsim_plot, quantsim_hist, ncol = 2, widths = c(2,1) ) 

