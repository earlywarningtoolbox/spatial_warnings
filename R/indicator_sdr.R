# 
#' @title Spectral Density Ratio (SDR) indicator
#' 
#' @description Compute the ratio of low frequencies over high frequencies
#'   of the r-spectrum. It also computes a null value obtained by 
#'   randomizing the matrix. 
#' 
#' @param input A matrix or a logical matrix (TRUE/FALSE), or a list of these. 
#' 
#' @param sdr_low_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio.
#'   For example, for the lowest 20\% (default value), set \code{sdr_low_range}
#'   to \code{c(0, .2)}.
#'  
#' @param sdr_high_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio. For example, for 
#'   the highest 20\% (default value), set \code{sdr_high_range} to 
#'   \code{c(.8, 1)}. 
#' 
#' @param nreplicates The number of replicates to compute for the null 
#'   distribution
#' 
#' @return A list (or a list of lists if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `value`: SDR of the matrix
#'     }
#'   If nreplicates is above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean SDR of the null distribution
#'       \item `null_sd`: SD of SDR in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution 
#'                          (value minus the null mean and divided by null 
#'                          standard deviation)
#'       \item `pval`: p-value based on the rank of the observed SDR
#'                       in the null distribution. A low p-value means that 
#'                       the indicator value is significantly higher than the 
#'                       null values. 
#'     }
#' 
#' @references 
#' 
#' Carpenter, S.R. & Brock, W.A. (2010). Early warnings of regime shifts in 
#' spatial dynamics using the discrete Fourier transform. Ecosphere, 1, art10
#' 
#'
#' @export
indicator_sdr <- function(input, 
                          sdr_low_range  = NULL, 
                          sdr_high_range = NULL, 
                          nreplicates = 999) { 
  
  check_mat(input) # checks if binary and sensible
  
  if ( is.null(sdr_low_range) ) { 
    warning("Choosing the 20% lowest frequencies for spectral density ratio ",
            "as none was specified. Use parameter sdr_low_range to choose ", 
            "a different value.")
    sdr_low_range <- c(0, .2)
  }
  
  if ( is.null(sdr_high_range) ) { 
    warning("Choosing the 20% highest frequencies for spectral density ratio ",
            "as none was specified. Use parameter sdr_high_range to choose ", 
            "a different value.")
    sdr_high_range <- c(.8, 1)
  }
  
  if (is.list(input)) {
    # Returns a list of lists
    return( llply(input, indicator_sdr, sdr_low_range, sdr_high_range, 
                   nreplicates) )
  } 
  
  # Now the input is always a mmatrix
  warn_if_not_square(input)
  
  ranges_absolute <- convert_ranges_to_absolute(input, sdr_low_range, 
                                                sdr_high_range)
  
  indicf <- function(mat) { 
    indicator_sdr_core(mat, ranges_absolute[["low"]], ranges_absolute[["high"]])
  }
  
  
  return( 
    compute_indicator_with_null(input, 
                                nreplicates = nreplicates, 
                                indicf = indicf)
  )
  
}

indicator_sdr_core <- function(mat, low_range, high_range) { 
  
  # Compute r-spectrum
  spectrum <- rspectrum(mat)
  
  # Compute ratio
  return( indicator_sdr_do_ratio(spectrum, low_range, high_range) )
}

indicator_sdr_do_ratio <- function(spectrum, low_range, high_range) { 
  
  # Compute subsets
  low_subset  <- with(spectrum, dist <= max(low_range)  & 
                                  dist >= min(low_range))
  high_subset <- with(spectrum, dist <= max(high_range) & 
                                  dist >= min(high_range))
  
  # Return ratio of means
  return( with(spectrum, mean(rspec[low_subset]) / mean(rspec[high_subset])) )
  
}

# Convert ranges from proportional values to absolute distance values
convert_ranges_to_absolute <- function(mat, 
                                       sdr_low_range  = NULL, 
                                       sdr_high_range = NULL) { 
    
  maxdist <- 1 + floor(min(dim(mat)) / 2)
  low_range_absolute  <- sdr_low_range * maxdist
  high_range_absolute <- sdr_high_range * maxdist
  
  return( list(low = low_range_absolute, 
               high = high_range_absolute) )
}
