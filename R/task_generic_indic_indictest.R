# 
# 
# This file contains the indictest functions for generic spews
# 

#'@export
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

