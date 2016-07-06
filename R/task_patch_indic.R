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
#' data(B)
#' indicator_fitpsd(B)
#' 
# 
# 
# 
# 
# 
# A WORD ON IMPLEMENTATION DETAILS
# 
# This function tests several distributions fits and chooses what is best 
#   according to AIC So far it does not do proper ll-ratio 
#   tests but just considers the distribution with best likelihood. 
# 
# The poweRlaw package provides ML-fitting for all of these functions except 
#   the truncated powerlaw. 
# 

# Used to return NA when there
model_nalist <- list(pl = NA, exp = NA, ln = NA, tpl = NA)

#' @export
patchdistr_spews <- function(x) {
  
  check_mat(x) # Check input matrix
  
  # If input is a list -> apply on each element
  if ( is.list(x)) { # FALSE for x = NULL
    results <- lapply(x, patchdistr_spews) 
    class(results) <- c('patchdistr_spews_list', 'patchdistr_spews', 
                        'spews_result', 'list')
    return(results)

  } 
  
  
  # Input needs to be a binary matrix here
  if ( ! is.binary_matrix(x) ) { 
    stop('Computing patch-size distributions require a binary matrix: please ', 
         'convert your data using as.binary_matrix() first.')
  }
  
  # Get patch size distribution
  psd <- patchsizes(x)
  
  
  
  # If there is only one big patch -> return NA early
  if ( length(unique(psd)) <= 2 ) { 
    warning('Not enough different patch sizes to fit distribution: returning NA')
    result <- list(models = model_nalist, likelihoods = model_nalist,
                   aics = model_nalist, best = 'none')
    class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                       'spews_result', 'list')
    return(result)
  }
  
  
  
  # We first fit a powerlaw. We do not provide an xmin: it will be 
  #   estimated
  pl_model <- estimate_dist_params(psd, poweRlaw::displ)
  
  # Now fit an exponential and a lognormal
  exp_model <- estimate_dist_params(psd, poweRlaw::disexp)
  ln_model  <- estimate_dist_params(psd, poweRlaw::dislnorm)
  
  # Now we fit an truncated powerlaw using pli source code
  tpl_model <- powerexp.fit(psd, threshold = 1) # xmin
  
  # Format results
  models <- list(pl = pl_model, exp = exp_model, ln = ln_model, tpl = tpl_model)
  
  likelihoods <- lapply(models, wrap, dist_ll, function(x) { x[['loglike']]})
  no_parms <- lapply(models, wrap, 
                      function(x) x$no_pars, 
                      function(x) 2) # a tpl always has 2 params
  
  aics <- Map(function(ll,k) 2*k - 2 * ll, likelihoods, no_parms)
  
  best <- names(models)[unlist(aics) == min(unlist(aics))]
  
  # Return 
  result <- list(models = models, likelihoods = likelihoods, 
                 aics = aics, best = best)
  class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                      'spews_result', 'list')
  return(result)
}


estimate_dist_params <- function(distrib_vec, disttype) { 
  dist <- disttype$new(distrib_vec)
  dist$setXmin(1)
  
  # Estimate parms
  parms_estimate <- poweRlaw::estimate_pars(dist)
  dist$setPars(parms_estimate$pars)
  
  dist
}

# We have to types of data that include distributions: we need to apply the 
# right function to each of them so here is a generic wrapper
wrap <- function(obj, powerLawf, plif) { 
  if ( class(obj) %in% c('displ', 'disexp', 'dislnorm') ) { 
    return( powerLawf(obj) )
  } else { 
    return( plif(obj) )
  }
}
