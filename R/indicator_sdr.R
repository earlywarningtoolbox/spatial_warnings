# 
#' @title Spectral Density Ratio (SDR) indicator
#' 
#' @description Compute the ratio of low frequencies over high frequencies
#'   of the r-spectrum. It also computes a null value obtained by 
#'   randomizing the matrix. 
#' 
#' @param mat A matrix or a binary_matrix, or a list of these. 
#' 
#' @param low_range A numeric vector of the form \code{c(min, max)} describing the 
#'   range of values considered as low frequencies
#' 
#' @param high_range A numeric vector of the form \code{c(min, max)} describing the 
#'   range of values considered as high frequencies
#' 
#' @param nreplicates The number of replicates to compute for the null 
#'   distribution
#' 
#' @return A single value containing the SDR value 
#' 
#' @references ?
#'
#'
indicator_sdr <- function(input, low_range, high_range, 
                          nreplicates = 499) { 
  
  check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_sdr, low_range, high_range, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      warning('The matrix is not square: indicator_sdr will only use a square ', 
              'subset centered around the middle point.')
    } 
    
    if ( any( max(high_range) > dim(input)/2 ) ) { 
      warning('Your maximum correlation distance is higher than half of the ',
              'matrix size')
    }
    
    return( 
      compute_indicator_with_null(input, 
                                  detrending = FALSE, 
                                  nreplicates = nreplicates, 
                                  do_coarse_graining = FALSE,
                                  indicator_function = 
                                  function(mat) { 
                                    indicator_sdr_core(mat, low_range, 
                                                       high_range)
                                  } ) 
    )
    
  }
  
}

indicator_sdr_core <- function(mat, low_range, high_range) { 
  
  # Compute r-spectrum
  spectrum <- indicator_powerspectrum(mat)[['r_spectrum']]
  
  # Compute subsets
  low_subset  <- with(spectrum, dist <= max(low_range)  & 
                                  dist >= min(low_range))
  high_subset <- with(spectrum, dist <= max(high_range) & 
                                  dist >= min(high_range))
  
  # Return ratio of means
  return( with(spectrum, mean(rspec[low_subset]) / mean(rspec[high_subset])) )
}
