#' Build probability-of-origin surfaces from a fitted transfer function
#'
#' Turns an untransformed isoscape and a transfer function fitted with
#' \code{\link{fitTransferFunction}} into per-individual probability-of-origin
#' surfaces. Unlike \code{\link{isotopeAssignmentModel}} (the classic path, where the
#' caller transforms the isoscape into tissue space by hand), this applies the transfer
#' function internally and offers a posterior-predictive path that propagates the full
#' transfer-function uncertainty.
#'
#' \code{method} and \code{metric} are orthogonal. \code{method} sets how the predictive
#' tissue distribution at each cell, \code{(mu, sd)}, is obtained; \code{metric} sets how
#' the observed value is scored against it. This predictive surface does not depend on
#' the individual, so it is computed once and reused across all individuals.
#'
#' \describe{
#'   \item{\code{method = "posterior"}}{per-cell \code{brms::posterior_predict} (chunked,
#'     \code{ndraws} draws); \code{mu}/\code{sd} are the draw mean/sd. Propagates
#'     transfer-parameter uncertainty and isoscape error automatically. Needs \pkg{brms}.}
#'   \item{\code{method = "point"}}{point-estimate transform
#'     \code{mu = intercept + slope*iso}, with
#'     \code{sd = sqrt((slope*iso_se)^2 + sigma^2)} by default (see \code{iso_se_scaling}).}
#'   \item{\code{metric = "overlap"}}{\code{2*pnorm(-|value - mu| / sqrt(2*(tissue_SD^2 +
#'     sd^2)))} --- an absolute, \strong{unnormalized} per-cell consistency probability.}
#'   \item{\code{metric = "density"}}{Gaussian density
#'     \code{dnorm(value, mu, sqrt(sd^2 + tissue_SD^2))} (see \code{tissue_SD_in_density}),
#'     normalized by default.}
#' }
#'
#' @param isotopeValue Observed tissue isotope value(s); a scalar or a vector with one
#'   entry per individual. Names, or \code{ID}, become the output layer names.
#' @param isoscape \code{SpatRaster} of the \strong{untransformed} isoscape mean (the
#'   predictor the transfer function was fitted against). Legacy \code{Raster*} inputs
#'   are coerced.
#' @param isoscape_SD \code{SpatRaster} of the isoscape standard error, same geometry as
#'   \code{isoscape}.
#' @param transfer An \code{isocat_transfer} object from \code{\link{fitTransferFunction}}.
#' @param ID Optional vector of individual identifiers (defaults to the names of
#'   \code{isotopeValue}, else \code{indv_1, indv_2, ...}).
#' @param tissue_SD Measurement error (standard deviation) on the observed value(s);
#'   scalar (recycled) or one per individual. Enters the overlap metric always, and the
#'   density metric per \code{tissue_SD_in_density}.
#' @param method One of \code{"posterior"} (default) or \code{"point"}.
#' @param metric One of \code{"overlap"} (default) or \code{"density"}.
#' @param iso_se_scaling Point path only. \code{"slope"} (default) propagates isoscape
#'   error through the slope, \code{sqrt((slope*iso_se)^2 + sigma^2)}; \code{"none"} uses
#'   \code{sqrt(iso_se^2 + sigma^2)}, reproducing the classic (published) surfaces.
#' @param tissue_SD_in_density Density metric only. If \code{TRUE} (default), the density
#'   spread includes \code{tissue_SD} (\code{sqrt(sd^2 + tissue_SD^2)}), matching how the
#'   overlap metric treats it; \code{FALSE} omits it, reproducing the classic density
#'   surface. (The overlap metric always includes \code{tissue_SD}.)
#' @param ndraws Posterior draws per cell for \code{method = "posterior"}.
#' @param chunkSize Cells per \code{posterior_predict} batch (memory control).
#' @param normalize Whether to normalize each surface to sum to 1. \code{NULL} (default)
#'   keys to the metric: \code{FALSE} for overlap, \code{TRUE} for density.
#' @param savePath \code{FALSE} (default) returns the surfaces in memory; a directory
#'   path writes one \code{<ID>.tif} per individual there instead.
#'
#' @return A \code{SpatRaster} with one layer per individual (named by \code{ID}). With
#'   \code{savePath}, a \code{SpatRaster} pointing at the written files.
#'
#' @seealso \code{\link{fitTransferFunction}}, \code{\link{isotopeAssignmentModel}},
#'   \code{\link{makeQuantileSurfaces}}, \code{\link{surfaceSimilarityMatrix}}.
#'
#' @examples
#' \dontrun{
#' myiso    <- rast(isoscape,    type = "xyz")
#' myiso_sd <- rast(isoscape_sd, type = "xyz")
#'
#' # tf <- fitTransferFunction(known, "fur", "iso", "fur_me", "iso_se")
#'
#' # Posterior-overlap surfaces (the new default arm):
#' surfaces <- makeAssignmentSurface(
#'   isotopeValue = c(A = -120, B = -80),
#'   isoscape = myiso, isoscape_SD = myiso_sd,
#'   transfer = tf, tissue_SD = 3
#' )
#'
#' # Point-density surfaces (~ the classic path) for comparison:
#' comp <- makeAssignmentSurface(
#'   isotopeValue = c(A = -120, B = -80),
#'   isoscape = myiso, isoscape_SD = myiso_sd,
#'   transfer = tf, method = "point", metric = "density"
#' )
#' }
#'
#' @importFrom methods is
#' @importFrom stats pnorm dnorm sd
#' @export makeAssignmentSurface
makeAssignmentSurface <- function(isotopeValue, isoscape, isoscape_SD, transfer,
                                  ID = NULL, tissue_SD = 0,
                                  method = c("posterior", "point"),
                                  metric = c("overlap", "density"),
                                  iso_se_scaling = c("slope", "none"),
                                  tissue_SD_in_density = TRUE,
                                  ndraws = 1000, chunkSize = 1e4,
                                  normalize = NULL, savePath = FALSE) {

  if (!inherits(transfer, "isocat_transfer"))
    stop("'transfer' must be an isocat_transfer object from fitTransferFunction().")
  if (is(isoscape, "Raster"))    isoscape    <- terra::rast(isoscape)
  if (is(isoscape_SD, "Raster")) isoscape_SD <- terra::rast(isoscape_SD)
  terra::compareGeom(isoscape, isoscape_SD)

  method         <- match.arg(method)
  metric         <- match.arg(metric)
  iso_se_scaling <- match.arg(iso_se_scaling)

  n <- length(isotopeValue)
  if (is.null(ID))
    ID <- if (!is.null(names(isotopeValue))) names(isotopeValue) else paste0("indv_", seq_len(n))
  if (length(tissue_SD) == 1L) tissue_SD <- rep(tissue_SD, n)
  if (length(tissue_SD) != n)
    stop("'tissue_SD' must be length 1 or length(isotopeValue).")
  if (is.null(normalize)) normalize <- (metric == "density")

  # Predictive tissue distribution per cell -- individual-independent, computed once.
  pred <- .predictSurface(transfer, isoscape, isoscape_SD, method = method,
                          iso_se_scaling = iso_se_scaling, ndraws = ndraws,
                          chunkSize = chunkSize)
  muV <- terra::values(pred[["mu"]])[, 1]
  sdV <- terra::values(pred[["sd"]])[, 1]

  score_one <- function(i) {
    y <- isotopeValue[i]; t <- tissue_SD[i]
    prob <- if (metric == "overlap") {
      .overlapProb(y, t, muV, sdV)
    } else {
      totSD <- if (tissue_SD_in_density) sqrt(sdV^2 + t^2) else sdV
      stats::dnorm(y, mean = muV, sd = totSD)
    }
    if (normalize) prob <- prob / sum(prob, na.rm = TRUE)
    prob
  }

  if (isFALSE(savePath)) {
    out <- terra::rast(isoscape, nlyrs = n)
    terra::values(out) <- vapply(seq_len(n), score_one, numeric(length(muV)))
    names(out) <- ID
    return(out)
  }

  if (!dir.exists(savePath)) dir.create(savePath, recursive = TRUE)
  paths <- vapply(seq_len(n), function(i) {
    r <- terra::rast(isoscape, nlyrs = 1)
    terra::values(r) <- score_one(i)
    names(r) <- ID[i]
    fp <- file.path(savePath, paste0(ID[i], ".tif"))
    terra::writeRaster(r, fp, overwrite = TRUE)
    fp
  }, character(1))
  terra::rast(paths)
}


#' Predictive tissue distribution per isoscape cell
#'
#' Internal engine for \code{\link{makeAssignmentSurface}}. Returns a two-layer
#' \code{SpatRaster} \code{(mu, sd)} giving, at each cell, the mean and standard
#' deviation of the tissue value the transfer function predicts for an individual that
#' originated there. Independent of the observed value and of \code{tissue_SD}, so it is
#' computed once per call and reused across individuals.
#'
#' Cells with missing isoscape values, or non-positive isoscape SE (which
#' \code{brms::posterior_predict} rejects), are returned as \code{NA}. Results are
#' written back into a template raster by cell index rather than rebuilt from
#' coordinates.
#'
#' @param transfer An \code{isocat_transfer}.
#' @param isoscape,isoscape_SD Isoscape mean and SE \code{SpatRaster}s.
#' @param method \code{"posterior"} or \code{"point"}.
#' @param iso_se_scaling \code{"slope"} or \code{"none"} (point path; see
#'   \code{\link{makeAssignmentSurface}}).
#' @param ndraws,chunkSize Posterior draws per cell and cells per batch.
#' @return A two-layer \code{SpatRaster} named \code{c("mu", "sd")}.
#' @keywords internal
#' @export .predictSurface
.predictSurface <- function(transfer, isoscape, isoscape_SD,
                            method = c("posterior", "point"),
                            iso_se_scaling = c("slope", "none"),
                            ndraws = 1000, chunkSize = 1e4) {

  method         <- match.arg(method)
  iso_se_scaling <- match.arg(iso_se_scaling)

  isoV <- terra::values(isoscape)[, 1]
  seV  <- terra::values(isoscape_SD)[, 1]
  ok   <- which(!is.na(isoV) & !is.na(seV) & seV > 0)

  mu <- rep(NA_real_, terra::ncell(isoscape))
  sd <- rep(NA_real_, terra::ncell(isoscape))

  if (method == "point") {
    p <- transfer$point
    if (any(is.na(c(p$intercept, p$slope, p$sigma))))
      stop("transfer$point is missing intercept/slope/sigma; method = 'point' needs all three.")
    mu[ok] <- p$intercept + p$slope * isoV[ok]
    sd[ok] <- if (iso_se_scaling == "slope")
      sqrt((p$slope * seV[ok])^2 + p$sigma^2) else sqrt(seV[ok]^2 + p$sigma^2)

  } else {
    if (!requireNamespace("brms", quietly = TRUE))
      stop("Package 'brms' is required for method = 'posterior'.")
    if (!"package:brms" %in% search()) {
      suppressPackageStartupMessages(attachNamespace("brms"))
      on.exit(try(detach("package:brms"), silent = TRUE), add = TRUE)
    }
    iso_name <- transfer$vars$iso
    se_name  <- transfer$vars$iso_se
    tse_name <- transfer$vars$tissue_se

    batches <- split(ok, ceiling(seq_along(ok) / chunkSize))
    for (b in batches) {
      nd <- data.frame(isoV[b]); names(nd) <- iso_name
      if (!is.null(se_name))  nd[[se_name]]  <- seV[b]
      # brms needs the mi() response-error column present and positive, but ignores its
      # value in prediction (see project note q3-brms-no-double-counting).
      if (!is.null(tse_name)) nd[[tse_name]] <- 1
      pp <- brms::posterior_predict(transfer$fit, newdata = nd, ndraws = ndraws)
      mu[b] <- colMeans(pp)
      sd[b] <- apply(pp, 2, stats::sd)
    }
  }

  out <- terra::rast(isoscape, nlyrs = 2)
  terra::values(out) <- cbind(mu, sd)
  names(out) <- c("mu", "sd")
  out
}


#' Normal-overlap consistency probability
#'
#' Internal helper for \code{\link{makeAssignmentSurface}}. Scores an observed value
#' \code{N(mu1, sd1^2)} against a predicted cell distribution \code{N(mu2, sd2^2)} as the
#' two-sided tail of their pooled-variance difference,
#' \code{2*pnorm(-|mu1 - mu2| / sqrt(2*(sd1^2 + sd2^2)))}. Vectorized over \code{mu2}/
#' \code{sd2} (the per-cell surfaces).
#'
#' @param mu1,sd1 Observed value and its measurement error.
#' @param mu2,sd2 Predicted per-cell mean and standard deviation.
#' @return A numeric vector of probabilities on \code{[0, 1]}.
#' @keywords internal
#' @export .overlapProb
.overlapProb <- function(mu1, sd1, mu2, sd2) {
  2 * stats::pnorm(-abs(mu1 - mu2) / sqrt(2 * (sd1^2 + sd2^2)))
}
