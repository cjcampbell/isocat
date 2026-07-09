#' Fit a tissue-isoscape transfer function as a Bayesian errors-in-variables regression
#'
#' Fits the transfer function that maps environmental isoscape values to tissue
#' isotope values (e.g. precipitation \eqn{\delta^2}H to fur \eqn{\delta^2}H), the
#' relationship isocat needs to build probability-of-origin surfaces. Unlike the
#' classic workflow---where the user fits an ordinary regression themselves, hand-
#' applies \code{slope * iso + intercept} to the isoscape, and passes the result to
#' \code{\link{isotopeAssignmentModel}}---this fits the relationship as a Bayesian
#' regression that propagates measurement error in \emph{both} the tissue values and
#' the isoscape values (an errors-in-variables model), and returns an object that
#' \code{\link{makeAssignmentSurface}} consumes directly.
#'
#' The function is a thin, opinionated wrapper around \pkg{brms}: you name the columns
#' of your known-origin data frame and it assembles the \code{brms} formula for you,
#' using \code{mi()} for tissue measurement error (a modelled response error) and
#' \code{me()} for isoscape error (a measured predictor). Power users who already know
#' \pkg{brms} can bypass the builder entirely with \code{formula}.
#'
#' \pkg{brms} is a \code{Suggests} dependency and is loaded only when this function is
#' called; it (and a working Stan backend such as \pkg{cmdstanr} or \pkg{rstan}) must
#' be installed.
#'
#' @param data A data frame of known-origin individuals (one row per sample).
#' @param tissue Column name (string) of the observed tissue isotope value, the
#'   regression response (e.g. \code{"fur"}).
#' @param iso Column name (string) of the isoscape value at each sample's known
#'   origin, the regression predictor (e.g. \code{"iso"}).
#' @param tissue_se Optional column name of the tissue measurement error (standard
#'   deviation). When supplied, enters the model as \code{tissue | mi(tissue_se)}.
#' @param iso_se Optional column name of the isoscape standard error. When supplied,
#'   the predictor enters as \code{me(iso, iso_se, gr = NULL)}.
#' @param weights Optional column name of case weights (e.g. down-weighting samples
#'   collected outside a moult window). Enters as \code{weights(weights)}.
#' @param prior Optional \pkg{brms} prior(s) for anchoring the fit on prior knowledge
#'   (e.g. published transfer-function estimates). Build them with the convenience
#'   constructor \code{\link{transferPrior}} or with \code{brms::prior()} directly;
#'   \code{NULL} uses \pkg{brms} defaults. Informative priors are the main reason to
#'   reach for this over an ordinary regression when known-origin samples are few.
#' @param family A \code{brms}/\code{glm} family. Defaults to \code{gaussian()}.
#' @param formula Optional escape hatch: a full \code{brms::bf()} formula that
#'   overrides the column-name builder. When supplied, \code{tissue}/\code{iso}/
#'   \code{tissue_se}/\code{iso_se}/\code{weights} are used only to label the returned
#'   object and to locate the point estimates.
#' @param ... Passed to \code{brms::brm()} (e.g. \code{chains}, \code{iter},
#'   \code{cores}, \code{backend}, \code{seed}, \code{file}).
#'
#' @return An S3 object of class \code{isocat_transfer}: a list with elements
#'   \describe{
#'     \item{\code{fit}}{the underlying \code{brmsfit}.}
#'     \item{\code{formula_str}}{the assembled model formula, as a string.}
#'     \item{\code{vars}}{named list of the column names (\code{tissue}, \code{iso},
#'       \code{tissue_se}, \code{iso_se}, \code{weights}), used to build prediction
#'       data from isoscape layers at assignment time.}
#'     \item{\code{point}}{cached point estimates \code{list(intercept, slope, sigma)}
#'       (posterior means), used by the point-estimate assignment path.}
#'     \item{\code{n}}{number of observations fitted.}
#'   }
#'   Methods: \code{print}, \code{coef} (returns the point estimates), and
#'   \code{pp_check} (delegates to \code{brms::pp_check()}).
#'
#' @seealso \code{\link{makeAssignmentSurface}} to turn the fitted transfer function
#'   into probability-of-origin surfaces; \code{\link{isotopeAssignmentModel}} for the
#'   classic hand-applied-transfer workflow.
#'
#' @examples
#' \dontrun{
#' # Known-origin individuals: observed tissue value, isoscape value at origin,
#' # and measurement error on each.
#' set.seed(42)
#' known <- data.frame(
#'   fur    = -40 + 1.1 * runif(30, -60, -10) + rnorm(30, 0, 6),
#'   iso    = runif(30, -60, -10),
#'   fur_me = 3,
#'   iso_se = 3
#' )
#'
#' tf <- fitTransferFunction(
#'   data      = known,
#'   tissue    = "fur",
#'   iso       = "iso",
#'   tissue_se = "fur_me",
#'   iso_se    = "iso_se",
#'   chains    = 2, iter = 1000, seed = 42
#' )
#'
#' tf                 # formula + point estimates
#' coef(tf)           # intercept, slope, sigma
#' pp_check(tf)       # posterior-predictive check (via brms); needs brms attached
#'
#' # Anchor a small sample on an informative slope prior, e.g. a published
#' # transfer-function slope (see transferPrior()):
#' tf_anchored <- fitTransferFunction(
#'   data = known, tissue = "fur", iso = "iso",
#'   tissue_se = "fur_me", iso_se = "iso_se",
#'   prior = transferPrior(slope = c(1.1, 0.05)),
#'   chains = 2, iter = 1000, seed = 42
#' )
#' }
#'
#' @importFrom methods is
#' @export fitTransferFunction
fitTransferFunction <- function(data, tissue, iso,
                                tissue_se = NULL, iso_se = NULL, weights = NULL,
                                prior = NULL, family = stats::gaussian(),
                                formula = NULL, ...) {

  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package 'brms' is required for fitTransferFunction(). ",
         "Install it with install.packages('brms').")

  if (!is.data.frame(data))
    stop("'data' must be a data.frame.")

  # Validate that every named column exists. c() drops NULL arguments, so this
  # collects only the columns the caller actually supplied.
  provided <- c(tissue = tissue, iso = iso, tissue_se = tissue_se,
                iso_se = iso_se, weights = weights)
  if (!all(vapply(provided, function(x) is.character(x) && length(x) == 1L, logical(1))))
    stop("'tissue', 'iso', and any *_se/'weights' arguments must each be a single column name.")
  missingCols <- setdiff(unname(provided), names(data))
  if (length(missingCols))
    stop("Columns not found in 'data': ", paste(missingCols, collapse = ", "))

  # Assemble the brms formula from the column names, unless the user overrides it.
  bform <- if (is.null(formula)) {
    .transferFormula(tissue, iso, tissue_se, iso_se, weights)
  } else {
    formula
  }
  formula_str <- paste(deparse(stats::formula(bform)), collapse = " ")

  # brms parses the special formula terms (me/mi/weights) by looking them up on the
  # search path, not in the formula's environment, so brms must be attached for brm()
  # to build the model. Attach it only for this call if the user hasn't already, and
  # restore the search path afterwards.
  if (!"package:brms" %in% search()) {
    suppressPackageStartupMessages(attachNamespace("brms"))
    on.exit(try(detach("package:brms"), silent = TRUE), add = TRUE)
  }

  fit <- brms::brm(bform, data = data, family = family, prior = prior, ...)

  structure(
    list(
      fit         = fit,
      formula_str = formula_str,
      vars        = list(tissue = tissue, iso = iso, tissue_se = tissue_se,
                         iso_se = iso_se, weights = weights),
      point       = .transferPoint(fit, iso, iso_se),
      n           = stats::nobs(fit)
    ),
    class = "isocat_transfer"
  )
}


#' Construct informative priors for a transfer-function fit
#'
#' A convenience wrapper that turns familiar quantities---a slope, an intercept, a
#' residual scatter, or the mean of the latent isoscape variable---into the \pkg{brms}
#' priors that \code{\link{fitTransferFunction}} expects, so users can anchor a fit on
#' prior knowledge (for example published transfer-function estimates for their taxon
#' or functional group) without knowing \pkg{brms}'s prior-class syntax. This is
#' especially useful when known-origin samples are too few to estimate the slope and
#' intercept precisely on their own, where the slope and intercept trade off strongly.
#'
#' Each argument is a length-2 numeric \code{c(mean, sd)} giving a normal prior. Only
#' the terms you supply are constrained; the rest keep \pkg{brms} defaults.
#'
#' @param slope Prior on the isoscape slope (\pkg{brms} class \code{"b"}). For an
#'   isotope transfer function a slope near 1 is the biologically meaningful anchor.
#' @param intercept Prior on the intercept (class \code{"Intercept"}). Note \pkg{brms}
#'   centres predictors internally, so this constrains the intercept at the mean
#'   isoscape value, not at zero.
#' @param sigma Prior on the residual standard deviation (class \code{"sigma"};
#'   automatically truncated to be positive).
#' @param meanme Prior on the mean of the latent isoscape variable (class
#'   \code{"meanme"}); only valid when the model uses \code{me()} (i.e. \code{iso_se}
#'   was supplied to \code{fitTransferFunction}).
#'
#' @return A \code{brmsprior} object suitable for the \code{prior} argument of
#'   \code{\link{fitTransferFunction}}.
#'
#' @seealso \code{\link{fitTransferFunction}}.
#'
#' @examples
#' \dontrun{
#' # A tight slope prior near 1 plus a looser intercept prior:
#' transferPrior(slope = c(1.0, 0.05), intercept = c(-25, 10))
#' }
#'
#' @export transferPrior
transferPrior <- function(slope = NULL, intercept = NULL, sigma = NULL, meanme = NULL) {

  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package 'brms' is required for transferPrior(). ",
         "Install it with install.packages('brms').")

  specs   <- list(slope = slope, intercept = intercept, sigma = sigma, meanme = meanme)
  classes <- c(slope = "b", intercept = "Intercept", sigma = "sigma", meanme = "meanme")
  specs   <- specs[!vapply(specs, is.null, logical(1))]
  if (!length(specs))
    stop("Supply at least one of 'slope', 'intercept', 'sigma', or 'meanme' as c(mean, sd).")

  parts <- lapply(names(specs), function(nm) {
    v <- specs[[nm]]
    if (!is.numeric(v) || length(v) != 2L)
      stop("'", nm, "' must be a length-2 numeric vector c(mean, sd).")
    brms::prior_string(sprintf("normal(%g, %g)", v[1], v[2]), class = classes[[nm]])
  })

  Reduce(`+`, parts)
}


#' Build the brms transfer-function formula from column names
#'
#' Internal helper for \code{\link{fitTransferFunction}}. Assembles a \code{brms::bf()}
#' formula, using \code{mi()} for tissue (response) measurement error, \code{me(...,
#' gr = NULL)} for isoscape (predictor) measurement error, and \code{weights()} for
#' case weights. Exposed (dot-prefixed, exported) so its string-building logic can be
#' tested without fitting a model.
#'
#' @param tissue,iso,tissue_se,iso_se,weights Column-name strings; \code{tissue_se},
#'   \code{iso_se}, and \code{weights} may be \code{NULL}.
#' @return A \code{brmsformula} object.
#' @keywords internal
#' @export .transferFormula
.transferFormula <- function(tissue, iso, tissue_se = NULL, iso_se = NULL, weights = NULL) {

  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package 'brms' is required to build the transfer-function formula.")

  # Left-hand side: response, plus mi()/weights() addition terms if supplied.
  lhs_addons <- character(0)
  if (!is.null(tissue_se)) lhs_addons <- c(lhs_addons, sprintf("mi(%s)", tissue_se))
  if (!is.null(weights))   lhs_addons <- c(lhs_addons, sprintf("weights(%s)", weights))
  lhs <- if (length(lhs_addons))
    paste(tissue, "|", paste(lhs_addons, collapse = " + ")) else tissue

  # Right-hand side: me() carries isoscape error; gr = NULL keeps the latent isoscape
  # variable ungrouped (one latent value per observation), matching the reference fit.
  rhs <- if (!is.null(iso_se)) sprintf("me(%s, %s, gr = NULL)", iso, iso_se) else iso

  brms::bf(stats::as.formula(paste(lhs, "~", rhs)))
}


#' Extract cached point estimates from a fitted transfer function
#'
#' Internal helper for \code{\link{fitTransferFunction}}. Pulls the intercept, slope,
#' and residual sigma from a \code{brmsfit} by posterior-summary row name rather than
#' by position, so it is robust to \pkg{brms} renaming terms (the \code{me()} slope is
#' mangled to \code{bsp_...}). Returns \code{NA} for any term it cannot locate (e.g.
#' when a custom \code{formula} has no single identifiable slope).
#'
#' @param fit A \code{brmsfit}.
#' @param iso,iso_se Column names, used to find the slope row (a plain \code{b_<iso>}
#'   coefficient when \code{iso_se} is \code{NULL}, otherwise the \code{bsp_} special
#'   effect).
#' @return A list \code{list(intercept, slope, sigma)} of posterior means.
#' @keywords internal
#' @export .transferPoint
.transferPoint <- function(fit, iso, iso_se = NULL) {

  ps <- brms::posterior_summary(fit)
  rn <- rownames(ps)
  getEst <- function(rows) if (length(rows)) unname(ps[rows[1], "Estimate"]) else NA_real_

  slope_rows <- if (!is.null(iso_se)) grep("^bsp_", rn) else which(rn == paste0("b_", iso))

  list(
    intercept = getEst(which(rn == "b_Intercept")),
    slope     = getEst(slope_rows),
    sigma     = getEst(which(rn == "sigma"))
  )
}


#' @export
print.isocat_transfer <- function(x, ...) {
  cat("<isocat_transfer>  Bayesian tissue-isoscape transfer function (brms)\n")
  cat("  formula: ", x$formula_str, "\n", sep = "")
  cat("  n:       ", x$n, "\n", sep = "")
  cat("  point estimates (posterior mean):\n")
  cat(sprintf("    intercept = %s\n    slope     = %s\n    sigma     = %s\n",
              format(x$point$intercept, digits = 4),
              format(x$point$slope,     digits = 4),
              format(x$point$sigma,     digits = 4)))
  cat("  -> coef() for point estimates, pp_check() for diagnostics,\n")
  cat("     makeAssignmentSurface() to build probability-of-origin maps.\n")
  invisible(x)
}


#' @export
coef.isocat_transfer <- function(object, ...) {
  c(intercept = object$point$intercept,
    slope     = object$point$slope,
    sigma     = object$point$sigma)
}


#' @exportS3Method brms::pp_check
pp_check.isocat_transfer <- function(object, ...) {
  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package 'brms' is required for pp_check().")
  brms::pp_check(object$fit, ...)
}
