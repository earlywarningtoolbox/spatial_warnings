# 
#' @title Early-warning signals based on patch size distributions
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
#' @param xmin_bounds Bounds when estimating the xmin for power-law distributions
#'
#' @param wrap Whether patches are consdired to wraparound when reaching one 
#'   side of the matrix.  
#' 
#' @return A list object of class 'psdfit' containing among other things 
#'   - the observed patch size distribution data
#'   - the model outputs for the candidate distribution fits
#'   - the power-law range values 
#'   - the percolation values (if several matrices were provided and 
#'   `merge` was TRUE, then the average percolation value is returned)
#' 
#' @details 
#' 
#' Patterned ecosystems can exhibit a change in their spatial structure as they 
#' becom more and more stressed. It has been suggested that this should be 
#' reflected in changes in the observed patch size distributions (PSD). 
#' The following sequence is expected to occur (Kéfi et al. 2011) as patterned 
#' ecosystems become more and more degraded:
#' 
#'   - Percolation of vegetation patches occurs (a patch has a width or height 
#'   equal to the size of the system)
#'   
#'   - The patch-size distribution follows a power-law
#'   
#'   - The patch-size distribution deviates from a power-law as larger patches 
#'   break down
#'   
#'   - The patch-size distribution follows is closer to an exponential 
#'   distribution
#' 
#' @references 
#' 
#' Kefi, S., Rietkerk, M., Alados, C. L., Pueyo, Y., Papanastasis, 
#'   V. P., ElAich, A., & De Ruiter, P. C. (2007). Spatial vegetation patterns 
#'   and imminent desertification in Mediterranean arid ecosystems. 
#'   Nature, 449(7159), 213-217.
#' 
#' Kéfi, S., Rietkerk, M., Roy, M., Franc, A., de Ruiter, P.C. & Pascual, M. 
#'   (2011). Robust scaling in ecosystems and the meltdown of patch size 
#'   distributions before extinction: Patch size distributions towards 
#'   extinction. Ecology Letters, 14, 29–35.
#' 
#' Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). 
#'   Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
#' 
#' @examples
#' 
#' data(arid)
#' indicator_fitpsd(arid)
#' 
# 
#' @export
patchdistr_spews <- function(x, 
                             merge = FALSE,
                             fit_lnorm = FALSE,
                             best_by = "AIC", 
                             xmin_bounds = NULL, 
                             wrap = FALSE,
                             ...) {
  
  check_mat(x) # Check input matrix
  
  # If input is a list -> apply on each element
  if ( !merge & is.list(x)) { 
    results <- llply(x, patchdistr_spews, merge, fit_lnorm, 
                     best_by, xmin_bounds, wrap, ...)
    class(results) <- c('patchdistr_spews_list', 'patchdistr_spews', 
                        'spews_result', 'list')
    return(results)
  } 
  
  # Get patch size distribution
  psd <- patchsizes(x, merge = merge, wrap = wrap)
  
  # Set bounds to search for xmin
  if ( length(psd) > 0 && is.null(xmin_bounds) ) { 
    xmin_bounds <- range(psd)
  }
  
  # Compute percolation 
  if ( is.list(x) ) { 
    percol <- lapply(x, percolation)
    percol <- mean(unlist(percol))
    percol_empty <- lapply(x, function(x) percolation(!x))
    percol_empty <- mean(unlist(percol_empty))
  } else { 
    percol <- percolation(x)
    percol_empty <- percolation(!x)
  } 
  
  # Return object 
  result <- list(psd_obs = sort(psd), 
                 psd_type = psdtype(psd, best_by, fit_lnorm),
                 percolation = percol,
                 percolation_empty = percol_empty,
                 cover = mean(x),
                 plrange = plrange(psd, xmin_bounds), 
                 npatches = length(psd),
                 unique_patches = length(unique(psd)))
  class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                     'spews_result', 'list')
  
  return(result)
}


