# 
# 
# This file contains function that compute, assess and display results from 
#   spectrum-based indicators: reddening of powerspectrum, spectrum density
#   ratio.
# 
# @title Spectrum-based spatial early-warning signals. 
# 
# @description Computation of spatial early warning signals based on spectral
#   properties.
# 
# @param input The input matrix or a list of matrices. 
# 
# @param sdr_low_range, sdr_high_range The range of values (in proportion) to 
#   use for the 
#   
#'@export
spectral_spews <- function(input, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL) { 
  
  # Check if input is suitable
  check_mat(input)
  
  orig_input <- input # Save original data for null models later

  
  if ( is.null(sdr_low_range) || is.null(sdr_high_range)) { 
    warning('Choosing default values of lower and higher 20% for spectral ',
            'density ratio. Use parameters sdr_low_range and sdr_high_range ',
            'to choose a better value')
    if ( is.null(sdr_low_range) )  sdr_low_range  <- c(0, .2)
    if ( is.null(sdr_high_range) ) sdr_high_range <- c(.8, 1)
  }
  
  # Handle list case
  if ( is.list(input) ) { 
    results <- lapply(input, spectral_spews, sdr_low_range, sdr_high_range)
    class(results) <- c('spectral_spews_list', 'spews_result', 'list')
    return(results)
  }
  
  # Now the input is always a matrix
  warn_if_not_square(input)
  
  # Compute powerspectrum
  spectrum <- rspectrum(input)
  
  # Compute SDR
  maxdist <- max(spectrum[ ,'dist'])
  low_range_absolute  <- sdr_low_range * maxdist
  high_range_absolute <- sdr_high_range * maxdist
  sdr_value <- indicator_sdr_do_ratio(spectrum, 
                                      low_range_absolute, high_range_absolute)
  
  # Return list containing both
  output <- list(results = list(spectrum = spectrum, 
                                sdr = sdr_value), 
                 orig_data = orig_input, 
                 call = match.call(), 
                 sdr_low_range = low_range_absolute, 
                 sdr_high_range = high_range_absolute)
  class(output) <- c('spectral_spews_single', 'spews_result', 'list')
  return(output)
}

# 
# Print methods for spectral_spews objects
# 
print.spectral_spews_list <- function(x, ...) { 
  print.default(x) # Not implemented yet
}

print.spectral_spews_single <- function(x, ...) { 
  print.default(x) # Not implemented yet
}


# # Define as.data.frame methods
# as.data.frame.spectral_spews_list <- function(x, ...


# 
# 
# Indictest functions for spectral_spews objects.
# 
#'@export
indictest.spectral_spews_list <- function(x, null_replicates = 999, ...) { 
  
  # Compute a distribution of null values for SDR
  null_values <- plyr::ldply(x, indictest.spectral_spews_single, null_replicates)
  
  # Format and return output
  results <- data.frame(replicate = seq.int(nrow(null_values)), 
                        null_values)
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  return(results)
  
}

#'@export
indictest.spectral_spews_single <- function(x, null_replicates = 999, ...) { 
  
  # Build closure passed to compute_indicator_with_null that uses the correct
  #   high and low ranges.
  sdr_indicf <- function(mat) { 
    indicator_sdr_core(mat, 
                       low_range  = x[['sdr_low_range']], 
                       high_range = x[['sdr_high_range']])
  }
  
  # Compute a distribution of null values for SDR
  null_values <- compute_indicator_with_null(x[['orig_data']], 
                                             # We do not make use of detrending
                                             #   for SDR (built-in already)
                                             detrending = FALSE,
                                             nreplicates = null_replicates, 
                                             indicf = sdr_indicf)
  # Format and return result
  results <- as.data.frame(null_values)
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}

