#' Make mean multi-month isoscape and error surfaces.
#'
#' Combines a stack of monthly isoscape maps, monthly isoscape standard error
#' maps, and optionally a precipitation (amount) stack into a single
#' precipitation-weighted mean isoscape and a combined error surface. Each stack
#' must contain layers corresponding to each of the target months.
#'
#' If precip_stack is NULL, equal precipitation amounts are assumed for each month.
#'
#' @param iso_stack A SpatRaster containing n layers corresponding to isoscape models for n months.
#' @param iso_se_stack A SpatRaster containing n layers corresponding to isoscape standard error maps for n months.
#' @param precip_stack Either a SpatRaster containing n layers corresponding to precipitation amounts for n months, or NULL (assumes equal precipitation amounts.)
#'
#' @return A list containing a precipitation-weighted mean isoscape (`mean_iso`)
#'   and a root-sum-of-square combined error map (`iso_se`).
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
makeMultiMonthIsoscape <- function(iso_stack, iso_se_stack, precip_stack = NULL) {

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

  # Combined error surface: root-sum-of-square of the monthly SE layers.
  iso_se <- sqrt(sum(iso_se_stack^2))

  return(list(mean_iso = mean_iso, iso_se = iso_se))

}
