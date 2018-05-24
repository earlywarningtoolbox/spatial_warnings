# 
# This file contains functions that handle deprecation of functions
# 

#' @rdname generic_spews-deprecated
#' 
#' @title (DEPRECATED) Generic Spatial Early-Warning signals
#' 
#' @description Computation, significance assesment and display of spatial 
#'   generic early warning signals (Moran's I, variance and skewness)
#' 
#' @param mat A matrix (quantitative data), a binary matrix (TRUE/FALSE data), 
#'   or a list of those
#' 
#' @param subsize The subsize used for the coarse-graining phase (see Details)
#'   
#' @param abs_skewness Should the absolute skewness be used instead of its 
#'   raw values ? 
#' 
#' @param moranI_coarse_grain Should the input matrix be coarse-grained before
#'   computing the Moran's I indicator value ?
#' 
#' @details \code{generic_spews} has been renamed, please use 
#'   \code{\link{generic_sews}} instead.
#' 
#'@export
generic_spews <- function(mat, 
                          subsize = 4, 
                          abs_skewness = FALSE, 
                          moranI_coarse_grain = FALSE) { 
  .Deprecated(new = "generic_sews", package = "spatialwarnings")
  generic_sews(mat, subsize, abs_skewness, moranI_coarse_grain)
}

#' @rdname patchdistr_spews-deprecated
#' 
#' @title (DEPRECATED) Early-warning signals based on patch size distributions
#' 
#' @description Compute early-warnings based on 
#'   patch size distributions and review/plot the results
#' 
#' @param x A logical matrix (TRUE/FALSE values) or a list of these
#' 
#' @param merge The default behavior is to produce indicators values for each 
#'   matrix. If this parameter is set to TRUE then the patch size distributions 
#'   are pooled together for fitting. 
#' 
#' @param fit_lnorm When patch size distributions are compared, should we 
#'   consider lognormal type ? (see details)
#' 
#' @param best_by The criterion to use to select the best fit (one of "AIC", 
#'   "BIC" or "AICc")
#' 
#' @param xmin The xmin to be used to fit the patch size distributions. Use 
#'   the special value "estimate" to compute first the xmin that produces 
#'   the best power-law fit, then use this estimated value to fit all 
#'   distributions. 
#' 
#' @param xmin_bounds Bounds when estimating the xmin for power-law distributions
#' 
#' @param wrap Determines whether patches are considered to wrap around the 
#'  matrix when reaching the side 
#'
#' @details \code{patchdistr_spews} has been renamed, please use 
#'   \code{\link{patchdistr_sews}} instead.
#' 
#'@export
patchdistr_spews <- function(x, 
                             merge = FALSE,
                             fit_lnorm = FALSE,
                             best_by = "BIC", 
                             xmin = 1, # a number, or "estimate" option
                             xmin_bounds = NULL, 
                             wrap = FALSE) {
  .Deprecated(new = "patchdistr_sews", package = "spatialwarnings")
  patchdistr_sews(x, merge, fit_lnorm, best_by, xmin, xmin_bounds, 
                  wrap)
}

#' @rdname spectral_spews-deprecated
#' 
#' @title (DEPRECATED) Spectrum-based spatial early-warning signals. 
#' 
#' @description Computation of spatial early warning signals based on spectral
#'   properties.
#' 
#' @param mat The input matrix or a list of matrices. 
#' 
#' @param sdr_low_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio.
#'   For example, for the lowest 20% (default value), set sdr_low_range to 
#'   c(0, .2). See also the Details section. 
#'  
#' @param sdr_high_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio. For example, for 
#'   the higher 20% (default value), set sdr_high_range to 
#'   c(.8, 1). See also the Details section. 
#' 
#' @param quiet Do not display some warnings
#' 
#' @details \code{spectral_spews} has been renamed, please use 
#'   \code{\link{spectral_sews}} instead.
#' 
#'@export
spectral_spews <- function(mat, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL, 
                           quiet = FALSE) { 
  .Deprecated(new = "spectral_sews", package = "spatialwarnings")
  spectral_sews(mat, sdr_low_range, sdr_high_range, quiet)
}

