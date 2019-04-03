The following files may appear in the results archive for IsoMAP precipitation prediction jobs:

obs.txt . observational data from the IsoMAP databases used to optimize model parameters, in tabular format; columns (ID:IsoMAP station ID; time:time of observation, not used in current IsoMAP jobs; long:station longitude; lat:station latitude; y:precipitation-amount weighted isotopic composition of precipitation at station; subsequent columns contain unweighted average independent variable values for variables and sources selected during model design)

job1.err . for internal IsoMAP use; may or may not be present

coeff.reg . optimized model coefficients for the regression model; columns (Estimate:parameter estimate; Std:standard deviation of parameter estimate; t-value:t statistic for the estimated parameter estimate; p-value:probability that the estimated parameter value is not different from zero); first row of values corresponds to the intercept of the regression function, remaining columns correspond to independent variables in the order they appear in obs.txt

mse.reg . mean square error of optimized regression model

aicbic.reg . Akaike.s information criterion and Bayesian information criterion values for the regression model

R2.reg . coefficient of determination for the regression model

residualplot.reg . data points used to generate the residual plot for the regression model; columns (predict:model-predicted isotope value at a given station; residual:difference of model-predicted and observed values at a given station); rows given values for each station in obs.txt

qqplot.reg . data points used to generate the quantile-quantile plot for the regression model; columns (normal-quantile:rank quantile of residual value at a given station, assuming a normal distribution; residual:difference of model-predicted and observed values at a given station) ; rows given values for each station in obs.txt

anova.reg . results of analysis of variance for the regression model; columns (df:degrees of freedom; SS:sum of squares, calculated as type I sum of squares; MS:mean square; Fvalue:F statistic for regression using a given variable; Pvalue:probability that the relationship between the independent variable and the isotope ratio observations results from random chance); rows given values for each independent variable in the order they appear in obs.txt followed by the residual (error) and total sum of squares

significance.reg . as for anova.reg, except SS is calculated as type II sum of squares

moranI.reg . results of Moran.s I test for spatial autocorrelation in the regression model residuals; columns (Ivalue:Moran.s I statistic; Mean:mean expected value for Moran.s I; Std:standard deviation of expected value for Moran.s I, Z_value:Z statistic for the observed Moran.s I score; P-value:probability that the observed level of spatial autocorrelation results from random chance)

estimate.reg . covariance matrix for the regression model; first row contains number of data, degrees of freedom; subsequent rows contain the covariance matrix

coeff.krig . optimized model coefficients for the geostatistical model; columns (Estimate:parameter estimate; Std:standard deviation of parameter estimate; t-value:t statistic for the estimated parameter estimate; p-value:probability that the estimated parameter value is not different from zero); first row of values corresponds to the intercept of the regression function, remaining columns correspond to independent variables in the order they appear in obs.txt; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

mse.krig . mean square error of optimized geostatistical model; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

aicbic.krig . Akaike.s information criterion and Bayesian information criterion values for the geostatistical model; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

residualplot.krig . data points used to generate the residual plot for the geostatistical model; columns (predict:model-predicted isotope value at a given station; residual:difference of model-predicted and observed values at a given station); rows given values for each station in obs.txt; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

qqplot.krig . data points used to generate the quantile-quantile plot for the geostatistical model; columns (normal-quantile:rank quantile of residual value at a given station, assuming a normal distribution; residual:difference of model-predicted and observed values at a given station) ; rows given values for each station in obs.txt; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

anova.krig . results of analysis of variance for the geostatistical model; columns (df:degrees of freedom; SS:sum of squares; MS:mean square; Fvalue:F statistic for regression using a given variable; Pvalue:probability that the relationship between the independent variable and the isotope ratio observations results from random chance); rows given values for each independent variable in the order they appear in obs.txt followed by the residual (error) and total sum of squares; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

estimate.krig . covariance matrix for the geostatistical model; first row contains number of data, degrees of freedom; subsequent rows contain the covariance matrix; file is only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

cv.txt .results from full leave-one-out cross validation of model fitting procedure; CVregL1:L1 norm (mean absolute residual) for regression model; CVregL2:L2 norm (mean square residual) for the regression model; CVkrigL1:L1 norm for geostatistical model; CVkrigL2:L2 norm for geostatistical model; geostatistical model results are only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

gnuplot_commands_reg.txt . for internal IsoMAP use

qqplotreg.png . image file of quantile-quantile plot for regression model

residualplotreg.png . image file of residual plot for regression model 

gnuplot_commands_krig.txt . for internal IsoMAP use

qqplotkrig.png . image file of quantile-quantile plot for geostatistical model

residualplotkrig.png . image file of residual plot for geostatistical model

metadata.xml . full version metadata documenting job provenance, including user-selected parameters, data completeness (calculated as the fraction of the average total precipitation across months selected that is represented by isotopic measurements, precipitation amounts derived from CRU gridded climate data), and information on job execution

metadata.props . for internal IsoMAP use

roast.txt . input data, in tabular form, for each grid cell in the map prediction domain; columns (ID:grid cell ID; time:time of observation, not used in current IsoMAP jobs; long: longitude of grid cell center; lat: latitude of grid cell center; Int:model intercept from external prediction function, not used in current IsoMAP jobs; subsequent columns contain unweighted average independent variable values for variables and sources selected during prediction job design)

prediction.txt . output of precipitation isotope model, in tabular form; columns (ID:grid cell ID; time:time of observation, not used in current IsoMAP jobs; long: longitude of grid cell center; lat: latitude of grid cell center; Int:model intercept from external prediction function, not used in current IsoMAP jobs; subsequent columns contain unweighted average independent variable values for variables and sources selected during prediction job design; predreg:predicted isotope value at grid cell, based on regression model; stdreg:standard deviation of model prediction at grid cell, based on regression model; predkrig:predicted isotope value at grid cell, based on geostatistical model; stdkrig:standard deviation of model prediction at grid cell, based on geostatistical model); geostatistical model results are only present if Moran.s I test indicates significant spatial autocorrelation of the regression model residuals at 95% confidence level

predreg.asc . predreg values from prediction.txt, in gridded Arc ASCII format; can be imported to ArcGIS using the ASCII to Raster tool 

stdreg.asc . stdreg values from prediction.txt, in gridded Arc ASCII format; can be imported to ArcGIS using the ASCII to Raster tool 

predkrig.asc . predkrig values from prediction.txt, in gridded Arc ASCII format.  Can be imported to ArcGIS using the ASCII to Raster tool; will only be present for jobs whose parent job included geostatistical model predictions

stdkrig.asc . stdkrig values from prediction.txt, in gridded Arc ASCII format.  Can be imported to ArcGIS using the ASCII to Raster tool; will only be present for jobs whose parent job included geostatistical model predictions

predreg.tiff - predreg values from prediction.txt, in gridded GeoTIFF format; can be opened or imported in most GIS and image analysis software

predreg.tiff.aux.xml . auxiliary file containing statistics for predreg.tiff

predkrig.tiff - predkrig values from prediction.txt, in gridded GeoTIFF format; can be opened or imported in most GIS and image analysis software; will only be present for jobs whose parent job included geostatistical model predictions

predreg.tiff.aux.xml . auxiliary file containing statistics for predreg.tiff; will only be present for jobs whose parent job included geostatistical model predictions

stdreg.tiff - stdreg values from prediction.txt, in gridded GeoTIFF format; can be opened or imported in most GIS and image analysis software

stdreg.tiff.aux.xml . auxiliary file containing statistics for stdreg.tiff

stdkrig.tiff - stdkrig values from prediction.txt, in gridded GeoTIFF format; can be opened or imported in most GIS and image analysis software; will only be present for jobs whose parent job included geostatistical model predictions

stdreg.tiff.aux.xml . auxiliary file containing statistics for stdreg.tiff; will only be present for jobs whose parent job included geostatistical model predictions

NNNNN.xml . (file name includes job number) summary metadata for internal IsoMAP use

