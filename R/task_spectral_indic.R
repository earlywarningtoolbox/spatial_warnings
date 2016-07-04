# 
# 
# This file contains function that compute, assess and display results from 
#   spectrum-based indicators: reddening of powerspectrum, spectrum density
#   ratio.
# 



#' @title Spectrum-based spatial early-warning signals. 
#' 
#' @description Computation of spatial early warning signals based on spectral
#'   properties.
#' 
#' @param input The input matrix or a list of matrices. 
#' 
#' @param sdr_low_range, sdr_high_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio (default: lower and 
#'   higher 20%). For example, for the lowest 20%, set sdr_low_range to 
#'   c(0, .2). See also the Details section. 
#' 
#' @param quiet Do not display some warnings
#' 
#' @return An object of class \code{spectral_spews_list} or 
#'   \code{spectral_spews_single} depending on whether the input was a list of 
#'   matrices or a single matrix. 
#' 
#' @details
#'   
#'   Spectral early warning signals are based on the fact that some dynamical 
#'     systems can exhibit an increase in their spatial structure when 
#'     approaching a transition (e.g. by forming patches or scale-free patterns).
#'     This is expected to be reflected in the spectrum of the spatial structure
#'     by an increase in lower frequencies ("reddening" of the spectrum). 
#'     
#'   This functions allows computing the radial-spectrum which gives the relative 
#'     dominance of each space-scale over a range of distances, from 1 to 
#'     \code{N/2} (N being the minimum between the number of rows and columns). 
#'     If the matrix is not square, then it is cropped to biggest square that 
#'     fits on the left side of the matrix. 
#'   
#'   Additionnaly, it summarizes this spectrum into a Spectral 
#'     Density Ratio (SDR), which is the ratio of low frequencies over 
#'     high frequencies of the r-spectrum. 
#' 
#' @references 
#' 
#'   Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#'   Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#'   Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' @examples
#' 
#' data(forestdat) 
#' 
#' spec_indic <- spectral_spews(forestdat[['matrices']], 
#'                              sdr_low_range  = c(0, .2), 
#'                              sdr_high_range = c(.2, 1))
#' 
#' summary(spec_indic)
#' 
#' # Display SDR trend
#' plot(spec_indic)
#' 
#' # Display spectra
#' plot_spectrum(spec_indic)
#' 
#' @export
spectral_spews <- function(input, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL, 
                           quiet = FALSE) { 
  
  # Check if input is suitable
  check_mat(input)
  
  orig_input <- input # Save original data for null models later
  
  if ( !quiet && is.null(sdr_low_range) ) { 
    warning("Choosing the 20% lowest frequencies for spectral density ratio ",
            "as none was specified. Use parameter sdr_low_range to choose ", 
            "a different value.")
    sdr_low_range <- c(0, .2)
  }
  
  if ( !quiet && is.null(sdr_high_range) ) { 
    warning("Choosing the 20% highest frequencies for spectral density ratio ",
            "as none was specified. Use parameter sdr_high_range to choose ", 
            "a different value.")
    sdr_high_range <- c(.8, 1)
  }
  
  # Handle list case
  if ( is.list(input) ) { 
    results <- lapply(input, spectral_spews, sdr_low_range, sdr_high_range, quiet)
    class(results) <- c('spectral_spews_list', 'spews_result', 'list')
    return(results)
  }
  
  # Now the input is always a matrix -> process it
  # Check and warn if not square
  if ( !quiet && nrow(input) != ncol(input) ) { 
    warning('The matrix is not square: only a squared subset in the left part ', 
            'will be used.')
    mindim <- min(dim(input))
    input <- input[seq.int(mindim), seq.int(mindim)]
  }
      
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
  class(output) <- c('spectral_spews_single', 'spectral_spews', 
                     'spews_result', 'list')
  return(output)
}
