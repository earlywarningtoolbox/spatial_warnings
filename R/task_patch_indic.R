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
patchdistr_spews <- function(x, best_by = "AIC", ...) {
  
  check_mat(x) # Check input matrix
  
  # If input is a list -> apply on each element
  if ( is.list(x)) { # FALSE for x = NULL
    results <- llply(x, patchdistr_spews, ...) 
    class(results) <- c('patchdistr_spews_list', 'patchdistr_spews', 
                        'spews_result', 'list')
    return(results)

  } 
  
  
  # Input needs to be a binary matrix here
  if ( ! is.logical(x) ) { 
    stop('Computing patch-size distributions require a logical matrix (TRUE/',
         'FALSE values): please convert your data first.')
  }
  
  # Get patch size distribution
  psd <- patchsizes(x)
  
  # Return object 
  result <- list(psd_obs = psd, 
                 psd_shapes = psdtype(psd, best_by = best_by), 
                 psd_plfit = "To be implemented")
  class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 'spews_result', 'list')
  
  return(result)
}

plrange <- function(psd, bounds = c(1, max(psd)-1)) { 
  
  # If there are not enough patches to work with -> return NA early
  if ( length(unique(psd)) <= 2 ) { 
    warning('Not enough different patch sizes to estimate xmin: returning NA')
    return(NA)
  }
  
  # Range of xmins to scan
  xmins <- seq(min(bounds), max(bounds), by = 1)
  
  kss <- rep(NA_real_, length(xmins))
  # Loop over all xmins 
  for (i in seq_along(xmins)) { 
    xmin <- xmins[i]
    
    # Compute distance statistic
    # -- 
    
    # Fit PL with given xmin
    
  }
  
  browser()
  
  
  
}
