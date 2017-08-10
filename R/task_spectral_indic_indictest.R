# 
# 
# This file contains the indictest functions for spectral spews
# 

#' @rdname spectral_spews
#' 
#' @param nperm The number of replicates to use to compute use in the 
#'   null distribution
#' 
#' @export
indictest.spectral_spews <- function(x, nperm = 999, ...) { 
  NextMethod('indictest')
}

# 
# Indictest functions for spectral_spews objects.
#' @method indictest spectral_spews_list
#' @export
indictest.spectral_spews_list <- function(x, nperm = 999, ...) { 
  
  # Compute a distribution of null values for SDR
  results <- parallel::mclapply(x, indictest.spectral_spews_single, 
                                nperm, ...)
  
  # Transfer names 
  names(results) <- names(x)
  
  # Add a replicate column with replicate number
  results <- Map(function(x, df) { df[ ,'replicate'] <- x; df }, 
                 seq.int(length(results)), results)
  
  # Bind all of it in a single df
  results <- do.call(rbind, results)
  
  # Format and return output
  attr(results, "nreplicates") <- nperm
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}

#' @method indictest spectral_spews_single
#' @export
indictest.spectral_spews_single <- function(x, nperm = 999, ...) { 
  
  # Build closure passed to compute_indicator_with_null that uses the correct
  #   high and low ranges, and is compatible with the use of replicate(). 
  sdr_indicf <- function(mat) { 
    spectrum <- rspectrum(mat)
    
    c(sdr = indicator_sdr_do_ratio(spectrum, x[['low_range']], 
                                   x[['high_range']]), 
      spectrum = spectrum[ ,'rspec'])
  }
  
  # Compute a distribution of null values for SDR
  null_values_sdr <- 
    compute_indicator_with_null(x[['orig_data']], 
                                # We do not make use of detrending for SDR
                                detrending = FALSE,
                                nreplicates = nperm, 
                                indicf = sdr_indicf)
  
  results <- rbind(
    data.frame(type = 'sdr', dist = NA,
               lapply(null_values_sdr, `[`, 1)), # first element of each list element
    data.frame(type = 'rspectrum', 
               dist = x[['results']][['spectrum']][ ,'dist'], 
               lapply(null_values_sdr, `[`, -1)) # all but first elem
  )
  # Add replicate column and discard row names
  results <- data.frame(replicate = 1, results)
  row.names(results) <- NULL
  
  # Format output
  attr(results, "nreplicates") <- nperm
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}
