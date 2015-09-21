#' @title Spatial Warnings Indicators
#'
#' @description This functions computes all the indicators of critical 
#'   transitions in a convenient form. 
#'   
#'   
#' @param mat A binary matrix or a list of square binary matrices
#' 
#'@export
indicator_all <- 
  function(mat,
           indicators = list(variance = spatialwarnings::indicator_variance,
                             skewness = spatialwarnings::indicator_skewness,
                             moranI   = spatialwarnings::indicator_moran,
                             psdfit   = spatialwarnings::indicator_fitpsd,
                             largestpatch = spatialwarnings::indicator_largestpatch,
                             fracgeo  = spatialwarnings::indicator_fracgeo,
                             cumpsd   = spatialwarnings::indicator_cumpsd),
           args = list(), 
           verbose = FALSE) { 
  
  # Not included yet: indicator_corrfunc, indicator_powerspectrum
  
  check_mat(mat) # checks if binary and sensible
  
  if ( is.list(mat)) { 
    return( lapply(mat, indicator_all) )
  }
  
  # Compute indicators
  indic_results <- list()
  for (i in seq.int(length(indicators))) { 
    if (verbose) message('Computing ', names(indicators)[i], "...")
    
    if (names(indicators)[i] %in% names(args)) { 
      # /!\ args[[i]] here should be a LIST ! (even one of one element)
      indic_results[[i]] <- do.call(indicators[[i]], c(mat, args[[i]]))
    } else { 
      indic_results[[i]] <- do.call(indicators[[i]], list(mat))
    }
  }
  names(indic_results) <- names(indicators)
  
  # Format and output results
  result <- list(call = match.call(),
                 indicators = indic_results)
  class(result) <- c('spindic','list')
  
  return(result)
}

