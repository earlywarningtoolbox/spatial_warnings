# 
# 
# This file contains the indictest functions for generic sews
# 

#' @rdname generic_sews
# /!\ Do not document x here as it already document in plot()
#' 
#' @param nulln The number of replicates to use to compute a null 
#'   distribution
#'   
#' @param ... Additional arguments passed onto methods
#' 
#' @export
indictest.generic_sews <- function(x, nulln = 999, ...) { 
  NextMethod('indictest')
}

#'@export 
indictest.generic_sews_single <- function(x, nulln = 999, ...) { 
  
  # We do not support low numbers of replicates
  if ( nulln < 3 ) { 
    stop('The number of null replicates should be above 3 to ', 
         'assess significance')
  }
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(x[["orig_data"]],
                                             nreplicates = nulln, 
                                             indicf = x[["indicf"]])
  
  results <- as.data.frame(null_values)
  
  # Format output. Note that we always add a "replicate" column even if there 
  # is only one so summary() code works on both
  indic_list <- c('variance', 'skewness', 'moran', 'mean')
  results <- data.frame(replicate = 1, 
                        indicator = indic_list, 
                        results)
  rownames(results) <- indic_list
  
  attr(results, 'nreplicates') <- nulln
  class(results) <- c('generic_sews_test', 'sews_test', 'data.frame')
  results
}

#'@export
indictest.generic_sews_list <- function(x, nulln = 999, ...) { 
  
  results <- parallel::mclapply(x, indictest.generic_sews_single, 
                                nulln, ...)
  
  # Replace replicate column with correct number
  for ( nb in seq_along(results) ) { 
    results[[nb]][ ,'replicate'] <- nb
  }
  results <- do.call(rbind, results)
  
  attr(results, 'nreplicates') <- nulln
  class(results) <- c('generic_sews_test', 'sews_test', 'data.frame')
  return(results)
}

