#' Plot the distribution of an origin variable per individual
#'
#' Visualizes the per-individual distribution of a variable across sampled origin points
#' (e.g. an environmental covariate from \code{\link{extractEnvPoints}}, or a coordinate)
#' as horizontal violins, one per group. This shows the spread of plausible origin values
#' for each individual, a ridgeline-style read on the point cloud from
#' \code{\link{sampleOriginPoints}}.
#'
#' The violins depict the raw sampled distribution (probability weighting is already in
#' the sample). Movement-prior weighting affects the summaries from
#' \code{\link{summarizeDistribution}}, not this display; overlay those summaries with a
#' \code{point}/\code{errorbar} layer if desired.
#'
#' Requires the \pkg{ggplot2} package.
#'
#' @param points A \code{data.frame} of points with a grouping column and the variable to
#'   plot (e.g. output of \code{\link{extractEnvPoints}}).
#' @param var Name of the column to plot.
#' @param group Name of the grouping column (typically the individual ID). Defaults to
#'   \code{"ID"}.
#' @param fill Fill color for the violins. Defaults to \code{"steelblue"}.
#' @param xlab,ylab Axis labels. Default to \code{var} and \code{group}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{sampleOriginPoints}}, \code{\link{extractEnvPoints}},
#'   \code{\link{summarizeDistribution}}
#'
#' @importFrom methods is
#'
#' @examples
#' # Sample origin points from example surfaces and attach an env value.
#' myiso <- rast(isoscape, type = "xyz")
#' myiso_sd <- rast(isoscape_sd, type = "xyz")
#' surfaces <- isotopeAssignmentModel(
#'         ID = c("A", "B", "C"), isotopeValue = c(-120, -90, -60),
#'         SD_indv = c(5, 5, 5),
#'         precip_raster = myiso, precip_SD_raster = myiso_sd, nClusters = FALSE
#'         )
#' env <- myiso; names(env) <- "d2H"
#' set.seed(1)
#' pts <- sampleOriginPoints(surfaces, n = 500)
#' pts <- extractEnvPoints(pts, env)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plotOriginDistribution(pts, var = "d2H")
#' }
#'
# `.data` is the ggplot2/rlang tidy-eval pronoun, resolved in the plot's data mask at
# render time; declare it so R CMD check does not flag it as an undefined global.
utils::globalVariables(".data")

#' @export
plotOriginDistribution <- function(points, var, group = "ID",
                                   fill = "steelblue",
                                   xlab = var, ylab = group){

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plotOriginDistribution(). Please install it.")
  if(!is.data.frame(points))
    stop("'points' must be a data.frame.")
  if(!var %in% names(points))
    stop("column '", var, "' not found in 'points'.")
  if(!group %in% names(points))
    stop("grouping column '", group, "' not found in 'points'.")

  ggplot2::ggplot(
      points,
      ggplot2::aes(x = .data[[var]], y = .data[[group]])
    ) +
    ggplot2::geom_violin(fill = fill, color = NA, alpha = 0.8, scale = "width") +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_bw()
}
