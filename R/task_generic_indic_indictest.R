# 
# 
# This file contains the indictest functions for generic spews
# 

#' @rdname generic_spews
# /!\ Do not document x here as it already document in plot()
#' 
#' @param nperm The number of replicates to use to compute a null 
#'   distribution
#'   
#' @param ... Additional arguments passed onto methods
#' 
#' @export
indictest.generic_spews <- function(x, nperm = 999, ...) { 
  NextMethod('indictest')
}

#'@export 
indictest.generic_spews_single <- function(x, nperm = 999, ...) { 
  
  # We do not support low numbers of replicates
  if ( nperm < 3 ) { 
    stop('The number of null replicates should be above 3 to ', 
         'assess significance')
  }
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(x[["orig_data"]],
                                             nreplicates = nperm, 
                                             indicf = x[["indicf"]])
  
  results <- as.data.frame(null_values)
  
  # Format output. Note that we always add a "replicate" column even if there 
  # is only one so summary() code works on both
  indic_list <- c('variance', 'skewness', 'moran', 'mean')
  results <- data.frame(replicate = 1, 
                        indicator = indic_list, 
                        results)
  rownames(results) <- indic_list
  
  attr(results, 'nreplicates') <- nperm
  class(results) <- c('generic_spews_test', 'spews_test', 'data.frame')
  results
}

# Summary function for many replicates
#'@export
indictest.generic_spews_list <- function(x, nperm = 999, ...) { 
  
  results <- parallel::mclapply(x, indictest.generic_spews_single, 
                                nperm, ...)
  
  # Replace replicate column with correct number
  for ( nb in seq_along(results) ) { 
    results[[nb]][ ,'replicate'] <- nb
  }
  results <- do.call(rbind, results)
  
  attr(results, 'nreplicates') <- nperm
  class(results) <- c('generic_spews_test', 'spews_test', 'data.frame')
  return(results)
}

