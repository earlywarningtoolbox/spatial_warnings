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
patchdistr_spews <- function(x, ...) {
  
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
  
  # Used to return NA when there
  model_nalist <- list(pl = NA, exp = NA, ln = NA, tpl = NA)
  
  # If there are not enough patches to work with -> return NA early
  if ( length(unique(psd)) <= 2 ) { 
    warning('Not enough different patch sizes to fit distribution: returning NA')
    result <- list(models = model_nalist, likelihoods = model_nalist, 
                   aics = model_nalist, bics = model_nalist)
    class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                       'spews_result', 'list')
    return(result)
  }
  
  # Return object 
  result <- list(psd_obs = psd, 
                 psd_shapes = psdtype(psd), 
                 psd_plfit = "To be implemented")
  class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 'spews_result', 'list')
  
  return(result)
}

psdtype <- function(psd, best_by = "AICc") { 

  # Fit a model for everyone
  models <- list(pl  =   pl_fit(psd), 
                 tpl =   tpl_fit(psd), 
                 exp =   exp_fit(psd), 
                 lnorm = lnorm_fit(psd)) 
  
  models <- lapply(models, as.data.frame)
  models <- do.call(rbind.fill, models)
  row.names(models) <- models[ ,'type']
  
  # Compute AICs
  models[ ,'AIC']  <- get_AIC(models[ ,'ll'],  models[ ,'npars'])
  models[ ,'AICc'] <- get_AICc(models[ ,'ll'], models[ ,'npars'], length(psd))
  models[ ,'BICc'] <- get_BIC(models[ ,'ll'],  models[ ,'npars'], length(psd))
  
  models[ ,'best'] <- models[ ,best_by] == max(models[ ,best_by])
  
  return(models)
}

get_AIC <- function(ll, k) { 
  2*k - 2*ll 
}

get_AICc <- function(ll, k, n) { 
  2*k - 2*ll + (2*k*(k+1))/(n-k-1)
}

get_BIC <- function(ll, k, n) { 
  2*k*log(n) - 2*ll 
}
