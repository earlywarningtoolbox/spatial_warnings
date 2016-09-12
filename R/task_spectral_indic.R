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
#' @param mat The input matrix or a list of matrices. 
#' 
#' @param sdr_low_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio.
#'   For example, for the lowest 20% (default value), set sdr_low_range to 
#'   c(0, .2). See also the Details section. 
#'  
#' @param sdr_high_range The range of values (in proportion) to 
#'   use for the computation of the spectral density ratio. For example, for 
#'   the higher 20% (default value), set sdr_high_range to 
#'   c(.8, 1). See also the Details section. 
#' 
#' @param quiet Do not display some warnings
#' 
#' @return 
#' 
#' Function \code{spectral_spews} object of class \code{spectral_spews_list} or 
#'   \code{spectral_spews_single} depending on whether the input was a list of 
#'   matrices or a single matrix. 
#' 
#' Function \code{indictest} 
#' 
#' The \code{plot} methods returns a ggplot object (usually displayed 
#' immediately when called interactively). 
#' 
#' @details
#'   
#' Spectral early warning signals are based on the fact that some dynamical 
#'   systems can exhibit an change in some characteristics of their spatial 
#'   structure when approaching a transition. In particular, long-range 
#'   correlations are expected to have an increased importance. 
#' 
#' This is expected to be reflected in the spectrum of the spatial structure
#'   in the form of an increase of the relative importance of lower 
#'   frequencies ("reddening" of the spectrum) over higher frequencies.
#'   
#' This functions allows computing the radial-spectrum which gives the relative 
#'   importance of each space scale as a function of distance, from 1 to 
#'   \code{N/2} (N being the minimum between the number of rows and columns). 
#'   If the matrix is not square, then it is cropped to biggest square that 
#'   fits within the left side of the matrix. 
#' 
#' Additionally, it summarizes this spectrum into a Spectral 
#'   Density Ratio (SDR), which is the ratio of low frequencies over 
#'   high frequencies of the r-spectrum. The SDR value is expected to increase
#'   before a transition point.
#' 
#' The significance of spectral early-warning signals can be estimated by 
#'   reshuffling the original matrix (function \code{indictest}). Indicators 
#'   are then recomputed on the shuffled matrices and the values obtained are 
#'   used as a null distribution. P-values are obtained based on the rank of 
#'   the observered value in the null distribution. 
#' 
#' 
#' @references 
#' 
#'   Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#'   Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#'   Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' 
#' @examples
#' 
#' data(forestdat) 
#' 
#' spec_indic <- spectral_spews(forestdat[['matrices']], 
#'                              sdr_low_range  = c(0, .2), 
#'                              sdr_high_range = c(.8, 1))
#' 
#' summary(spec_indic)
#' 
#' # Display trends along the varying model parameter
#' plot(spec_indic, along = forestdat[['parameters']][ ,'delta'])
#' 
#' # Assess significance
#' spec_test <- indictest(spec_indic)
#' summary(spec_test)
#' 
#' # Display trends, now with a grey 5%-95% quantiles of the null distribution
#' plot(spec_test, along = forestdat[['parameters']][ ,'delta'])
#' 
#' # Display radial-spectra
#' plot_spectrum(spec_test, along = forestdat[['parameters']][ ,'delta'])
#' 
#' @export
spectral_spews <- function(mat, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL, 
                           quiet = FALSE) { 
  
  # Check if mat is suitable
  check_mat(mat)
  
  orig_input <- mat # Save original data for null models later
  
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
  if ( is.list(mat) ) { 
    results <- lapply(mat, spectral_spews, sdr_low_range, sdr_high_range, quiet)
    class(results) <- c('spectral_spews_list',  'spectral_spews', 
                        'spews_result', 'list')
    return(results)
  }
  
  # Now the mat is always a matrix -> process it
  # Check and warn if not square
  if ( !quiet && nrow(mat) != ncol(mat) ) { 
    warning('The matrix is not square: only a squared subset in the left part ', 
            'will be used.')
    mindim <- min(dim(mat))
    mat <- mat[seq.int(mindim), seq.int(mindim)]
  }
      
  # Compute powerspectrum
  spectrum <- rspectrum(mat)
  
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
