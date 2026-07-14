# `.data` is the ggplot2/rlang tidy-eval pronoun, resolved in the plot's data mask at
# render time; declare it so R CMD check does not flag it as an undefined global.
utils::globalVariables(".data")

#' Plot the distribution of an origin variable per individual
#'
#' Visualizes the per-individual distribution of a variable across sampled origin points
#' (e.g. an environmental covariate from \code{\link{extractEnvPoints}}, or a coordinate),
#' one row per group. The sampled origin points are Monte-Carlo draws from the
#' probability-of-origin surface, so the distribution-of-draws idioms from \pkg{ggdist}
#' apply directly.
#'
#' The default \code{type = "halfeye"} draws a \pkg{ggdist} slab-and-interval ("eye")
#' summary: a density with a median-and-quantile-interval beneath it. Other types:
#' \code{"eye"} (two-sided density), \code{"dots"} (quantile dotplot), and \code{"violin"}
#' (a plain \pkg{ggplot2} violin, requiring only \pkg{ggplot2}).
#'
#' The displayed density and intervals are computed from the \emph{raw} sampled points
#' (probability weighting is already in the sample); they do \emph{not} reflect the
#' movement-prior weighting applied by \code{\link{summarizeDistribution}}. To visualize a
#' movement-weighted summary, overlay precomputed intervals with a
#' \code{point}/\code{interval} layer instead.
#'
#' Requires the \pkg{ggplot2} package; the \code{"halfeye"}, \code{"eye"}, and
#' \code{"dots"} types additionally require \pkg{ggdist}.
#'
#' @param points A \code{data.frame} of points with a grouping column and the variable to
#'   plot (e.g. output of \code{\link{extractEnvPoints}}).
#' @param var Name of the column to plot.
#' @param group Name of the grouping column (typically the individual ID). Defaults to
#'   \code{"ID"}.
#' @param type Distribution geom: one of \code{"halfeye"} (default), \code{"eye"},
#'   \code{"dots"} (all via \pkg{ggdist}), or \code{"violin"} (via \pkg{ggplot2}).
#' @param fill Fill color. Defaults to \code{"steelblue"}.
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
#' # Default "halfeye" (needs ggdist); "violin" needs only ggplot2.
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("ggdist", quietly = TRUE)) {
#'   plotOriginDistribution(pts, var = "d2H")
#' }
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plotOriginDistribution(pts, var = "d2H", type = "violin")
#' }
#'
#' @export
plotOriginDistribution <- function(points, var, group = "ID",
                                   type = c("halfeye", "eye", "dots", "violin"),
                                   fill = "steelblue",
                                   xlab = var, ylab = group){

  type <- match.arg(type)

  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plotOriginDistribution(). Please install it.")
  if(type != "violin" && !requireNamespace("ggdist", quietly = TRUE))
    stop("Package 'ggdist' is required for type = '", type,
         "' in plotOriginDistribution(). Install it, or use type = 'violin'.")
  if(!is.data.frame(points))
    stop("'points' must be a data.frame.")
  if(!var %in% names(points))
    stop("column '", var, "' not found in 'points'.")
  if(!group %in% names(points))
    stop("grouping column '", group, "' not found in 'points'.")

  layer <- switch(type,
    halfeye = ggdist::stat_halfeye(fill = fill, alpha = 0.8),
    eye     = ggdist::stat_eye(fill = fill, alpha = 0.8),
    dots    = ggdist::stat_dots(fill = fill, color = NA),
    violin  = ggplot2::geom_violin(fill = fill, color = NA, alpha = 0.8, scale = "width")
  )

  ggplot2::ggplot(
      points,
      ggplot2::aes(x = .data[[var]], y = .data[[group]])
    ) +
    layer +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_bw()
}
