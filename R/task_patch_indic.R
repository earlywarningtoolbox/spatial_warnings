#
#' @title Early-warning signals based on patch size distributions
#' 
#' @description Compute early-warnings based on patch size distributions 
#'   and review/plot the results
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
#' become more and more stressed. It has been suggested that this should be 
#' reflected in changes in the observed patch size distributions (PSD). 
#' The following sequence is expected to occur (Kefi et al. 2011) as patterned 
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
#'   - The patch-size distribution is closer to an exponential 
#'   distribution
#' 
#' Additionally, it has been suggested that these changes in patch size 
#' distribution shape should be reflected in the power-law range (PLR). This 
#' function carries out all the required computations and helps display 
#' the results in a convenient form. 
#' 
#' The fitting of PSDs is based on maximum-likelihood following Clauset et al.'s 
#' procedure. The best discrete distribution is estimated among these 
#' candidates: a power-law \eqn{x^\lambda}, an exponential 
#' \eqn{exp(\alpha x)}, a truncated power-law and \eqn{x^\lambda exp(\alpha x)},
#' and optionally, a log-normal. Each distribution parameter is estimated 
#' using maximum-likelihood, with a minimum patch size (xmin) fixed to one. 
#' The best distribution is selected based on BIC by default. In raw results, 
#' \code{expo} refers to the power-law exponent \eqn{\lambda} in the previous 
#' equations and \code{rate} referes to the exponential decay rate \eqn{\alpha}. 
#' 
#' To compute the Power-law range (PLR), power-laws are fitted with a variable 
#' minimum patch size (xmin) and the one with the lowest Kolmogorov-Smirnov
#' distance to the empirical distribution is retained. PLR is then computed 
#' using this best-fitting xmin: 
#' 
#' \deqn{\frac{log(x_{max}) - log(x_{min})}{log(x_{max}) - log(x_{smallest})}}{ (log(xmax) - log(xmin))/(log(xmax) - log(xsmallest))}
#' 
#' Results can be displayed using the text-based \code{summary} and \code{print}, 
#' but graphical options are also available to plot the trends (\code{plot}) and 
#' the fitted distributions (\code{plot_distr}). Plotting functions are 
#' documented in a \link[=patchdistr_sews_plot]{separate page}. Observed and 
#' fitted distributions can be produced using the \code{predict} function, 
#' as documented in \link[=patchdistr_sews_predict]{this page}. 
#' 
#' @seealso \code{\link{indicator_psdtype}}, \code{\link{patchsizes}}, 
#'   \code{\link[=patchdistr_sews_plot]{plot}}, 
#'   \code{\link[=patchdistr_sews_plot]{plot_distr}}, 
#'   \code{\link[=patchdistr_sews_predict]{predict}}
#' 
#' @references 
#' 
#' Kefi, S., Rietkerk, M., Alados, C. L., Pueyo, Y., Papanastasis, 
#'   V. P., ElAich, A., & De Ruiter, P. C. (2007). Spatial vegetation patterns 
#'   and imminent desertification in Mediterranean arid ecosystems. 
#'   Nature, 449(7159), 213-217.
#' 
#' Kefi, S., Rietkerk, M., Roy, M., Franc, A., de Ruiter, P.C. & Pascual, M. 
#'   (2011). Robust scaling in ecosystems and the meltdown of patch size 
#'   distributions before extinction: Patch size distributions towards 
#'   extinction. Ecology Letters, 14, 29-35.
#' 
#' Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). 
#'   Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
#' 
#' @examples
#' 
#' data(forestgap)
#' 
#' \dontrun{
#' psd_indic <- patchdistr_sews(forestgap)
#' 
#' summary(psd_indic)
#' plot(psd_indic)
#' 
#' # Plots can be modified using ggplot2 directives 
#' if ( require(ggplot2) ) { 
#'   plot(psd_indic) + 
#'     theme_minimal()
#' }
#' 
#' # Export results to a data.frame
#' psd_indic_export <- as.data.frame(psd_indic) 
#' head(psd_indic_export)
#' 
#' }
#' @export
patchdistr_sews <- function(x, 
                            merge = FALSE,
                            fit_lnorm = FALSE,
                            best_by = "BIC", 
                            xmin = 1, # a number, or "estimate" option
                            xmin_bounds = NULL, 
                            wrap = FALSE) {
  
  check_mat(x) # Check input matrix
  
  # If input is a list -> apply on each element
  if ( !merge & is.list(x)) { 
    results <- parallel::mclapply(x, patchdistr_sews, merge, fit_lnorm, 
                                   best_by, xmin, xmin_bounds, wrap)
    class(results) <- c('patchdistr_sews_list', 'patchdistr_sews', 
                        'sews_result_list', 'list')
    return(results)
  } 
  
  # Get patch size distribution
  psd <- patchsizes(x, merge = merge, wrap = wrap)
  
  # Set bounds to search for xmin
  if ( length(psd) > 0 && is.null(xmin_bounds) ) { 
    xmin_bounds <- range(psd)
  }
  
  # Estimate power-law range and set xmin to the estimated value if set to 
  # auto. 
  plr_est <- plrange(psd, xmin_bounds)
  if ( xmin == "estimate" ) { 
    xmin <- plr_est['xmin_est']
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
  
  # Compute the mean cover 
  if ( is.list(x) ) { 
    meancover <- mean(laply(x, mean))
  } else { 
    meancover <- mean(x)
  }
  
  # Return object 
  result <- list(psd_obs = sort(psd), 
                 psd_type = psdtype(psd, xmin, best_by, fit_lnorm),
                 percolation = percol,
                 percolation_empty = percol_empty,
                 cover = meancover,
                 plrange = plr_est, 
                 npatches = length(psd),
                 unique_patches = length(unique(psd)))
  class(result) <- c('patchdistr_sews_single', 'patchdistr_sews', 
                     'sews_result_single', 'list')
  
  return(result)
}


