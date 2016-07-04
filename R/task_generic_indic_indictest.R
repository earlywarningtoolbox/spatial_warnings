# 
# 
# This file contains the indictest functions for generic spews
# 

#' @title Generic Spatial Early-Warnings
#' 
#' @description Assessment of the significance of generic early-warning signals
#'   values
#' 
#' @param obj A \code{generic_spews} object as produced by 
#'   \code{\link{generic_spews}}. 
#' 
#' @param null_replicates The number of replicates to use to compute a null 
#'   distribution
#' 
#' @return An object of class \code{generic_spews_test} (actually, a 
#'   data.frame). 
#' 
#' @details 
#' 
#' The significance of generic early-warning signals can be estimated by 
#'   reshuffling the original matrix randomly. Indicators are then recomputed 
#'   on the shuffled matrices and the values obtained are used as a null 
#'   distribution. P-values are obtained based on the rank of the observered
#'   value in the null distribution. 
#' 
#' @references
#' 
#'   Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#'   Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#'   Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' @export
indictest.generic_spews_single <- function(obj, null_replicates = 999, ...) { 
  
  # We do not support low numbers of replicates
  if ( null_replicates < 3 ) { 
    stop('The number of null replicates should be above 3 to ', 
         'assess significance')
  }
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(obj[["orig_data"]],
                                             detrending = obj[["detrend"]], 
                                             nreplicates = null_replicates, 
                                             indicf = obj[["indicf"]])
  
  results <- as.data.frame(null_values)
  
  # Format output. Note that we always add a "replicate" column even if there 
  # is only one so code works on both
  indic_list <- c('Variance', 'Skewness', 'Moran\'s I', 'Mean')
  results <- data.frame(replicate = 1, 
                        indicator = indic_list, 
                        results)
  rownames(results) <- indic_list
  
  attr(results, 'nreplicates') <- null_replicates
  class(results) <- c('generic_spews_test', 'spews_test', 'data.frame')
  results
}

# Summary function for many replicates
#'@export
indictest.generic_spews_list <- function(obj, null_replicates = 999, ...) { 
  
  results <- plyr::llply(obj, indictest.generic_spews_single, null_replicates, ...)
  
  # Replace replicate column with correct number
  for ( n in seq_along(results) ) { 
    results[[n]][ ,'replicate'] <- n
  }
  results <- do.call(rbind, results)
  
  attr(results, 'nreplicates') <- null_replicates
  class(results) <- c('generic_spews_test', 'spews_test', 'data.frame')
  return(results)
}

