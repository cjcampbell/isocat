#' Example monthly precipitation isoscape data
#'
#' Three months (July, August, September) of a d2H precipitation isoscape,
#' provided to demonstrate \code{\link{makeMultiMonthIsoscape}}.
#'
#' @format A data.frame with 43200 rows and 5 columns: \code{x} and \code{y}
#'   coordinates plus three monthly d2H layers \code{d2h_07}, \code{d2h_08},
#'   and \code{d2h_09}. Convert to a multi-layer SpatRaster with
#'   \code{rast(iso_monthly, type = "xyz")}.
#'
#' @references Bowen G. J., West J.B., Miller C. C., Zhao L. and Zhang T. (2018) IsoMAP: Isoscapes Modeling, Analysis and Prediction (version 1.0). The IsoMAP Project. http://isomap.org
#'
#' @seealso \code{\link{iso_monthly_se}}, \code{\link{makeMultiMonthIsoscape}}
#'
#' @examples
#' iso <- rast(iso_monthly, type = "xyz")
"iso_monthly"


#' Example monthly precipitation isoscape standard-error data
#'
#' Standard-error surfaces matching \code{\link{iso_monthly}}: three months
#' (July, August, September) of the d2H precipitation isoscape prediction error.
#'
#' @format A data.frame with 43200 rows and 5 columns: \code{x} and \code{y}
#'   coordinates plus three monthly standard-error layers \code{d2h_se_07},
#'   \code{d2h_se_08}, and \code{d2h_se_09}. Convert to a multi-layer SpatRaster
#'   with \code{rast(iso_monthly_se, type = "xyz")}.
#'
#' @references Bowen G. J., West J.B., Miller C. C., Zhao L. and Zhang T. (2018) IsoMAP: Isoscapes Modeling, Analysis and Prediction (version 1.0). The IsoMAP Project. http://isomap.org
#'
#' @seealso \code{\link{iso_monthly}}, \code{\link{makeMultiMonthIsoscape}}
#'
#' @examples
#' iso_se <- rast(iso_monthly_se, type = "xyz")
"iso_monthly_se"
