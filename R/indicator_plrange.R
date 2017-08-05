# 
# This file contains the indicator function based on the power-law range 
#   (how much of the data behaves like a power law?)
# 
# 
#'@export
indicator_plrange <- function(mat, 
                              merge = FALSE, 
                              xmin_bounds = NULL) { 
  
  if ( !merge && is.list(mat) ) { 
      # Returns a list of lists
    return( lapply(mat, indicator_plrange, merge, xmin_bounds) )
  }
  
  psd <- patchsizes(mat, merge = merge)
  
  if ( is.null(xmin_bounds) ) { 
    xmin_bounds <- range(psd)
  }
  
  # If there are not enough patches to work with -> return NA
  if ( length(unique(psd)) <= 2 ) { 
    warning('Not enough different patch sizes to estimate xmin: returning NA')
    result <- data.frame(NA, NA, NA, NA)
  } else { 
    # Compute xmin and range
    plrange_result <- plrange(psd, xmin_bounds) # returns xmin also
    result <- data.frame(min(psd), max(psd), plrange_result)
  }
  
  names(result) <- c("minsize", "maxsize", "xmin_est", "plrange")
  return(result)
} 

plrange <- function(psd, xmin_bounds) { 
  
  # If psd is empty, then return NA
  if ( length(unique(psd)) <= 1) { 
    return( data.frame(xmin_est = NA_real_, plrange = NA_real_) )
  } 
  
  xsmallest <- min(psd)
  xmax <- max(psd)
  xmin <- xmin_estim(psd, bounds = xmin_bounds)
  
  if ( is.na(xmin) ) { # finding xmin failed
    result <- data.frame(NA_real_, NA_real_)
  } else { 
    result <- data.frame(xmin, 1 - (log10(xmin) - log10(xsmallest)) / 
                                (log10(xmax) - log10(xsmallest)))
  }
  
  names(result) <- c('xmin_est', 'plrange')
  
  return(result)
}
