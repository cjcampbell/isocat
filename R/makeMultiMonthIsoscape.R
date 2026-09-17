#' Make mean multi-month isoscape and error surfaces.
#'
#' Combines a stack of monthly isoscape maps, monthly isoscape standard error
#' maps, and optionally a precipitation (amount) stack to provide weights,
#' into a single precipitation-weighted mean isoscape and a combined error
#' surface. Each stack must contain layers corresponding to each of the target
#' months.
#'
#' If precip_stack is NULL, equal precipitation amounts are assumed for each month.
#'
#' @param iso_stack A SpatRaster containing n layers corresponding to isoscape models for n months.
#' @param iso_se_stack A SpatRaster containing n layers corresponding to isoscape standard error maps for n months.
#' @param precip_stack Either a SpatRaster containing n layers corresponding to precipitation amounts for n months, or NULL (assumes equal precipitation amounts.)
#'   Only each month's share of the per-cell total enters the weights, so the units are
#'   arbitrary. Long-term monthly averages (a precipitation climatology) are generally the
#'   appropriate choice, matching the climatological basis of most monthly isoscape
#'   products; monthly precipitation is a separate data product that you supply.
#' @param iso_se_method Character; how the monthly standard-error layers are combined
#'   into a single error surface. One of \code{"weighted-correlated"} (the default),
#'   \code{"weighted-uncorrelated"}, or \code{"rss"}. See Details.
#'
#' @details
#' The combined error surface is the standard error of the same precipitation-weighted
#' mean returned as \code{mean_iso}. Writing \eqn{w_m} for the per-cell weight of month
#' \eqn{m} (the relativized precipitation, or \eqn{1/n} when \code{precip_stack} is
#' \code{NULL}) and \eqn{\sigma_m} for that month's standard error, the variance of the
#' weighted mean under a single cross-month error correlation \eqn{\rho} is
#'
#' \deqn{(1 - \rho) \sum_m w_m^2 \sigma_m^2 + \rho \left( \sum_m w_m \sigma_m \right)^2.}
#'
#' \code{iso_se_method} selects the endpoints of that expression:
#' \describe{
#'   \item{\code{"weighted-correlated"}}{\eqn{\rho = 1}, giving \eqn{\sum_m w_m \sigma_m}.
#'     Perfectly correlated monthly errors do not cancel, so this is the upper bound. It
#'     is the default: monthly isoscapes are typically built from a shared station network
#'     and a shared interpolation, so their errors are strongly positively correlated, and
#'     assuming otherwise lets the combined error shrink as \eqn{\sqrt{n}} purely by
#'     averaging in more months.}
#'   \item{\code{"weighted-uncorrelated"}}{\eqn{\rho = 0}, giving
#'     \eqn{\sqrt{\sum_m w_m^2 \sigma_m^2}}. Uncorrelated monthly errors partially
#'     cancel, so this is the lower bound. Appropriate only when the monthly errors are
#'     genuinely unrelated.}
#'   \item{\code{"rss"}}{The unweighted root-sum-of-square \eqn{\sqrt{\sum_m \sigma_m^2}}
#'     returned by isocat 1.0.1 and earlier. It ignores the precipitation weights entirely
#'     and is the standard error of the unweighted \emph{sum} of the months rather than of
#'     \code{mean_iso}, so it overstates the combined error: an amount-weighted
#'     combination returns the same error surface as an equal-weighted one. Retained only
#'     for reproducing results from earlier versions.}
#' }
#'
#' @return A list containing a precipitation-weighted mean isoscape (`mean_iso`) and the
#'   combined error map (`iso_se`), the latter computed according to `iso_se_method`.
#'
#' @importFrom methods is
#'
#' @examples
#' myiso <- rast(isoscape, type = "xyz")
#' myiso_sd <- rast(isoscape_sd, type = "xyz")
#' # Two example "months" derived from the bundled isoscape.
#' iso_stack    <- c(myiso, myiso + 5)
#' iso_se_stack <- c(myiso_sd, myiso_sd)
#' precip_stack <- c(myiso * 0 + 1, myiso * 0 + 3)   # relative weights 1:3
#' result <- makeMultiMonthIsoscape(iso_stack, iso_se_stack, precip_stack)
#' plot(result$mean_iso)
#' plot(result$iso_se)
#'
#' @export
makeMultiMonthIsoscape <- function(iso_stack, iso_se_stack, precip_stack = NULL,
                                  iso_se_method = c("weighted-correlated",
                                                    "weighted-uncorrelated",
                                                    "rss")) {

  iso_se_method <- match.arg(iso_se_method)

  if(is(iso_stack, "Raster"))    iso_stack    <- terra::rast(iso_stack)
  if(is(iso_se_stack, "Raster")) iso_se_stack <- terra::rast(iso_se_stack)
  if(!is.null(precip_stack) && is(precip_stack, "Raster"))
    precip_stack <- terra::rast(precip_stack)

  stopifnot(
    "iso_stack must be a SpatRaster"    = is(iso_stack, "SpatRaster"),
    "iso_se_stack must be a SpatRaster" = is(iso_se_stack, "SpatRaster"),
    "precip_stack must be a SpatRaster or NULL" =
      is.null(precip_stack) || is(precip_stack, "SpatRaster"),
    "iso_stack and iso_se_stack must have the same number of layers" =
      terra::nlyr(iso_stack) == terra::nlyr(iso_se_stack),
    "precip_stack must have the same number of layers as iso_stack" =
      is.null(precip_stack) || terra::nlyr(iso_stack) == terra::nlyr(precip_stack),
    "iso_stack and iso_se_stack must share the same geometry" =
      terra::compareGeom(iso_stack, iso_se_stack, stopOnError = FALSE)
  )

  if(!is.null(precip_stack)) {
    # Relativize precipitation so the monthly weights sum to 1 in each cell.
    precip_relativized <- precip_stack / sum(precip_stack)

    if(!terra::compareGeom(precip_relativized, iso_stack[[1]], stopOnError = FALSE)) {

      # Resample if the number of rows/columns does not match.
      if(!terra::compareGeom(precip_relativized, iso_stack[[1]],
                             crs = FALSE, ext = FALSE, rowcol = TRUE,
                             stopOnError = FALSE)) {
        precip_relativized <- terra::resample(precip_relativized, iso_stack[[1]])
      }

      # Crop/extend if the extents do not match.
      if(!terra::compareGeom(precip_relativized, iso_stack[[1]],
                             crs = FALSE, ext = TRUE, rowcol = FALSE,
                             stopOnError = FALSE)) {
        precip_relativized <- terra::crop(
          terra::extend(precip_relativized, iso_stack[[1]]),
          iso_stack[[1]]
        )
      }

      stopifnot(
        "precip_stack could not be aligned to iso_stack; compare them with terra::compareGeom()." =
          terra::compareGeom(precip_relativized, iso_stack, stopOnError = FALSE)
      )

    }

  } else {
    # Equal weights: each of the n months contributes 1/n.
    precip_relativized <- iso_stack * 0 + 1 / terra::nlyr(iso_stack)
  }

  # Precipitation-weighted mean isoscape. The weights sum to 1 per cell, so the
  # weighted average is the per-cell SUM across the weighted monthly layers.
  mean_iso <- sum(precip_relativized * iso_stack)

  # Combined error surface: the standard error of the weighted mean above, under
  # the cross-month error correlation implied by iso_se_method. See Details.
  if(iso_se_method == "weighted-correlated") {
    # rho = 1: errors move together and do not cancel.
    iso_se <- sum(precip_relativized * iso_se_stack)
  } else if(iso_se_method == "weighted-uncorrelated") {
    # rho = 0: errors are unrelated and partially cancel.
    iso_se <- sqrt(sum(precip_relativized^2 * iso_se_stack^2))
  } else {
    # Legacy (isocat <= 1.0.1) behavior: unweighted root-sum-of-square. Ignores
    # the precipitation weights; retained only for reproducing earlier results.
    iso_se <- sqrt(sum(iso_se_stack^2))
  }

  return(list(mean_iso = mean_iso, iso_se = iso_se))

}
