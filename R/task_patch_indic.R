# 
#' @title fit candidate cumulative patch size distribution functions.
#'
#' @param x A binary matrix or a list of binary matrices
#' 
#' @references Kefi, S., Rietkerk, M., Alados, C. L., Pueyo, Y., Papanastasis, 
#' V. P., ElAich, A., & De Ruiter, P. C. (2007). Spatial vegetation patterns 
#' and imminent desertification in Mediterranean arid ecosystems. 
#' Nature, 449(7159), 213-217.
#' 
#' @references Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). 
#' Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
#' 
#' @return A list object of class 'psdfit' containing the pooled cumulative 
#'   patch size distribution data, as well as the AICs and model outputs of the 
#'   candidate models.
#' 
#' @details A S3 method for \code{summary()} does exist and only returns the 
#'   model summary of the most parsimonious model. 
#' 
#' @examples
#' 
#' data(arid)
#' indicator_fitpsd(arid)
#' 
# 
# 
# 
# 
# 
# A WORD ON IMPLEMENTATION DETAILS
# 
# The poweRlaw package provides ML-fitting for PL + LNORM + EXP but pli 
#   provides fitting for TPL
# 

#' @export
patchdistr_spews <- function(x, 
                             best_by = "AIC", 
                             xmin_bounds = NULL, 
                             merge = FALSE,
                             ...) {
  
  check_mat(x) # Check input matrix
  
  # If input is a list -> apply on each element
  if ( !merge & is.list(x)) { # FALSE for x = NULL
    results <- llply(x, patchdistr_spews, best_by, xmin_bounds, merge, ...) 
    class(results) <- c('patchdistr_spews_list', 'patchdistr_spews', 
                        'spews_result', 'list')
    return(results)
  } 
  
  # Get patch size distribution
  psd <- patchsizes(x, merge = merge)
  
  # Set bounds to search for xmin
  if ( is.null(xmin_bounds) ) { 
    xmin_bounds <- range(psd)
  }
  
  # Compute percolation 
  if ( is.list(x) ) { 
    perc <- unlist(lapply(x, percolation))
    perc <- mean(perc)
  } else { 
    perc <- percolation(x)
  } 
  
  # Return object 
  result <- list(psd_obs = sort(psd), 
                 psd_type = psdtype(psd, best_by), 
                 percolation = perc,
                 plrange = plrange(psd, xmin_bounds), 
                 unique_patches = length(psd))
  class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                     'spews_result', 'list')
  
  return(result)
}


