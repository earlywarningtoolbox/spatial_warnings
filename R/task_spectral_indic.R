# 
# 
# This file contains a function that compute, assess and display results from 
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
#' @details Spectral early warning signals are based on the fact that some 
#'   dynamical systems can exhibit an change in some characteristics of their 
#'   spatial structure when approaching a transition. In particular, long-range 
#'   correlations are expected to have an increased importance. 
#' 
#' This is expected to be reflected in the spectrum of the spatial structure
#'   by an increase of the relative importance of lower frequencies over higher 
#'   frequencies ("reddening" of the spectrum).
#'   
#' This task allows computing the radial-spectrum which gives the relative 
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
#' The trend of SDR values can be plotted using the \code{plot()} method. 
#'   Alternatively, the spectrum itself can be plotted (with facets 
#'   if multiple input matrices were used) using the \code{plot_spectrum} 
#'   method.
#' 
#' 
#' @references 
#' 
#'   Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#'   Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#'   Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' @examples
#' 
#' data(serengeti) 
#' data(serengeti.rain) 
#' 
#' 
#' spec_indic <- spectral_spews(serengeti, 
#'                              sdr_low_range  = c(0, .2), 
#'                              sdr_high_range = c(.8, 1))
#' 
#' summary(spec_indic)
#' 
#' # Display trends along the varying model parameter
#' plot(spec_indic, along = serengeti.rain)
#' 
#' # Computing spectra many times is expensive, consider setting parallel 
#' # computing using: options(mc.cores = n)
#' 
#' # Assess significance
#' spec_test <- indictest(spec_indic, nperm = 499)
#' summary(spec_test)
#' 
#' # Display the SDR trend, now with a grey ribbon representing 5%-95% 
#' # quantiles of the null distribution
#' plot(spec_test, along = serengeti.rain)
#' 
#' # Add a line highlighting the shift 
#' if (require(ggplot2)) {
#'   plot(spec_test, along = serengeti.rain) + 
#'     geom_vline(xintercept = 590, color = "red", linetype = "dashed")
#' }
#' 
#' 
#' # Display radial-spectra
#' plot_spectrum(spec_test, along = serengeti.rain)
#' 
#' # Graphics can be modified using ggplot2 functions
#' if (require(ggplot2)) { 
#'   plot_spectrum(spec_test, along = serengeti.rain) + 
#'     scale_y_log10()
#' }
#' 
#' @export
spectral_spews <- function(mat, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL, 
                           quiet = FALSE) { 
  
  # Check if mat is suitable
  check_mat(mat)
  
  if ( is.null(sdr_low_range) ) { 
    if ( !quiet ) { 
      warning("Choosing the 20% lowest frequencies for spectral density ratio ",
              "as no range was specified. Use parameter sdr_low_range to choose ", 
              "a different value.")
    }
    sdr_low_range <- c(0, .2)
  }
  
  if ( is.null(sdr_high_range) ) { 
    if ( !quiet ) { 
      warning("Choosing the 20% highest frequencies for spectral density ratio ",
              "as no range was specified. Use parameter sdr_high_range to choose ", 
              "a different value.")
    }
    sdr_high_range <- c(.8, 1)
  }
  
  # Handle list case
  if ( is.list(mat) ) { 
    results <- lapply(mat, spectral_spews, sdr_low_range, sdr_high_range, quiet)
    names(results) <- names(mat)
    class(results) <- c('spectral_spews_list',  'spectral_spews', 
                        'spews_result_list', 'list')
    return(results)
  }
  
  orig_input <- mat # Save original data for null models later
  
  # Now the mat is always a matrix -> process it
  # Check and warn if not square
  if ( nrow(mat) != ncol(mat) ) { 
    if ( !quiet ) { 
      warning('The matrix is not square: only a squared subset in the left part ', 
              'will be used.')
    }
    
    mindim <- min(dim(mat))
    mat <- mat[seq.int(mindim), seq.int(mindim)]
  }
  
  # Compute powerspectrum
  spectrum <- rspectrum(mat)
  
  # Compute SDR
  ranges_absolute <- convert_ranges_to_absolute(mat, sdr_low_range, 
                                                sdr_high_range)
  
  sdr_value <- indicator_sdr_do_ratio(spectrum, 
                                      ranges_absolute[["low"]], 
                                      ranges_absolute[["high"]])
  
  # Return list containing both
  output <- list(results = list(spectrum = spectrum, sdr = sdr_value), 
                 orig_data = orig_input, 
                 call = match.call(), 
                 low_range = ranges_absolute[['low']], 
                 high_range = ranges_absolute[['high']])
  class(output) <- c('spectral_spews_single', 'spectral_spews', 
                     'spews_result_single', 'list')
  return(output)
}
