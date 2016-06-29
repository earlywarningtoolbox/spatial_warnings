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
#   according to maximum likelihood. So far it does not do proper ll-ratio 
#   tests but just considers the distribution with best likelihood. 
# 
# The poweRlaw package provides ML-fitting for all of these functions except 
#   the truncated powerlaw. 
# 


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
    result <- list(models = list(pl = NA, exp = NA, ln = NA),
                   likelihoods = list(pl = NA, exp = NA, ln = NA),
                   xmin = NA,
                   best = 'none')
    class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                       'spews_result', 'list')
    return(result)
  }
  
  
  
  # We first fit a powerlaw. We do not provide an xmin: it will be 
  #   estimated
  pl_model <- estimate_dist_params(psd, poweRlaw::displ, xmin = NULL)
  
  # Retrieve xmin from the estimation
  xmin <- pl_model$getXmin()
  
  # Now fit an exponential and a lognormal
  exp_model <- estimate_dist_params(psd, poweRlaw::disexp,   xmin = xmin)
  ln_model  <- estimate_dist_params(psd, poweRlaw::dislnorm, xmin = xmin)
  
  # Format results
  models <- list(pl = pl_model, exp = exp_model, ln = ln_model)
  likelihoods <- lapply(models, poweRlaw::dist_ll)
  best <- names(models)[unlist(likelihoods) == max(unlist(likelihoods))]
  
  # Return 
  result <- list(models = models, likelihoods = likelihoods, best = best, 
                 xmin = xmin)
  class(result) <- c('patchdistr_spews_single', 'patchdistr_spews', 
                      'spews_result', 'list')
  return(result)
  
}


estimate_dist_params <- function(distrib_vec, disttype, xmin = NULL) { 
  dist <- disttype$new(distrib_vec)
  
  # Set xmin if unspecified
  if ( is.null(xmin) ) { 
    xmin <- poweRlaw::estimate_xmin(dist)
  }  
  dist$setXmin(xmin)
  
  # Estimate PL parms
  parms_estimate <- poweRlaw::estimate_pars(dist)
  dist$setPars(parms_estimate$pars)
  
  dist
}

