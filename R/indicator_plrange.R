# 
# This file contains the indicator function based on the power-law range 
#   (how much of the data behaves like a power law?)
# 
# 

indicator_plrange <- function(mat, 
                              merge = FALSE, 
                              xmin_range = NULL) { 
  
  if ( !merge && is.list(mat) ) { 
      # Returns a list of lists
    return( lapply(mat, indicator_plrange, merge, xmin_range) )
  }
  
  psd <- patchsizes(mat, merge = merge)
  
  if ( is.null(xmin_range) ) { 
    xmin_range <- range(psd)
  }
  
  # If there are not enough patches to work with -> return NA
  if ( length(unique(psd)) <= 2 ) { 
    warning('Not enough different patch sizes to estimate xmin: returning NA')
    result <- data.frame(NA, NA, NA)
  } else { 
    # Compute xmin and range
    plrange_result <- plrange(psd, xmin_range) # returns xmin also
    result <- data.frame(min(psd), max(psd), plrange_result)
  }
  
  names(result) <- c("minsize", "maxsize", "xmin_est", "plrange")
  return(result)
} 

plrange <- function(psd, xmin_range) { 
  xsmallest <- min(psd)
  xmax <- max(psd)
  xmin <- xmin_estim(psd, bounds = xmin_range)
  data.frame(xmin_est = xmin, 
             plrange = 1 - (log10(xmin) - log10(xsmallest)) / (log10(xmax) - log10(xsmallest)))
}
