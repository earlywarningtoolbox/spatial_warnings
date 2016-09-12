# 
# 
# This file contains the indictest functions for spectral spews
# 

#' @title Spectral Spatial Early-Warnings
#' 
#' @rdname spectral_spews
#' 
#' @param null_replicates The number of replicates to use to compute use in the 
#'   null distribution
#' 
#' @param ... Further arguments passed onto methods
#' 
#' @export
indictest.spectral_spews <- function(obj, null_replicates = 999, ...) { 
  NextMethod('indictest')
}

# 
# Indictest functions for spectral_spews objects.
#' @method indictest spectral_spews_list
#' @export
indictest.spectral_spews_list <- function(obj, null_replicates = 999, ...) { 
  
  # Compute a distribution of null values for SDR
  results <- plyr::llply(obj, indictest.spectral_spews_single, 
                             null_replicates, ...)
  
  # Add a replicate column with replicate number
  results <- Map(function(obj, df) { df[ ,'replicate'] <- obj; df }, 
                 seq.int(length(results)), results)
  
  # Bind all of it in a single df
  results <- do.call(rbind, results)
  
  # Format and return output
  attr(results, "nreplicates") <- null_replicates
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}

#' @method indictest spectral_spews_single
#' @export
indictest.spectral_spews_single <- function(obj, null_replicates = 999, ...) { 
  
  # Build closure passed to compute_indicator_with_null that uses the correct
  #   high and low ranges.
  sdr_indicf <- function(mat) { 
    spectrum <- rspectrum(mat)
    
    c(sdr = indicator_sdr_do_ratio(spectrum, obj[['sdr_low_range']], 
                                   obj[['sdr_high_range']]), 
      spectrum = spectrum[ ,'rspec'])
  }
  
  # Compute a distribution of null values for SDR
  null_values_sdr <- 
    compute_indicator_with_null(obj[['orig_data']], 
                                # We do not make use of detrending for SDR
                                detrending = FALSE,
                                nreplicates = null_replicates, 
                                indicf = sdr_indicf)
  results <- rbind(
    data.frame(type = 'sdr', dist = NA,
               lapply(null_values_sdr, `[`, 1)), # first element of each list element
    data.frame(type = 'rspectrum', 
               dist = obj[['results']][['spectrum']][ ,'dist'], 
               lapply(null_values_sdr, `[`, -1)) # all but first elem
  )
  
  # Add replicate column and discard row names
  results <- data.frame(replicate = 1, results)
  row.names(results) <- NULL
  
  # Format output
  attr(results, "nreplicates") <- null_replicates
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}
