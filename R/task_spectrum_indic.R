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
# 

spectrum_spews <- function(input, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL ) { 
  
  # Check if input is suitable
  check_mat(input)
  
  orig_input <- input # Save original data for null models later

  
  if ( is.null(sdr_low_range) || is.null(sdr_high_range)) { 
    warning('Choosing default values of lower and higher 20% for spectral ',
            'density ratio. Use paramaeters sdr_low_range and sdr_high_range ',
            'to choose a better value')
    if ( is.null(sdr_low_range) )  sdr_low_range  <- c(0, .2)
    if ( is.null(sdr_high_range) ) sdr_high_range <- c(.8, 1)
  }
  

  # Handle list case
  if ( is.list(input) ) { 
    results <- lapply(input, spectrum_spews, sdr_low_range, sdr_high_range)
    class(results) <- c('spectrum_spews', 'spectrum_spews_list', 
                        'spews_result', 'list')
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
  return( list(spectrum = spectrum, sdr = sdr_value) )
  
}
