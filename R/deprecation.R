# 
# This file contains deprecated functions 
# 

#' @rdname generic_spews-deprecated
#' 
#' @title (DEFUNCT) Generic Spatial Early-Warning signals
#' 
#' @description Computation, significance assesment and display of spatial 
#'   generic early warning signals (Moran's I, variance and skewness)
#' 
#' @param mat A matrix (quantitative data), a binary matrix (TRUE/FALSE data), 
#'   or a list of those
#' 
#' @param subsize The subsize used for the coarse-graining phase (see Details)
#'   
#' @param abs_skewness Should the absolute skewness be used instead of its 
#'   raw values ? 
#' 
#' @param moranI_coarse_grain Should the input matrix be coarse-grained before
#'   computing the Moran's I indicator value ?
#' 
#' @details \code{generic_spews} has been renamed, please use 
#'   \code{\link{generic_sews}} instead.
#' 
#'@export
generic_spews <- function(mat, 
                          subsize = 4, 
                          abs_skewness = FALSE, 
                          moranI_coarse_grain = FALSE) { 
  .Defunct(new = "generic_sews", package = "spatialwarnings")
}

#' @rdname patchdistr_spews-deprecated
#' 
#' @title (DEFUNCT) Early-warning signals based on patch size distributions
#' 
#' @description Compute early-warnings based on 
#'   patch size distributions and review/plot the results
#' 
#' @param x A logical matrix (TRUE/FALSE values) or a list of these
#' 
#' @param merge The default behavior is to produce indicators values for each 
#'   matrix. If this parameter is set to TRUE then the patch size distributions 
#'   are pooled together for fitting. 
#' 
#' @param fit_lnorm When patch size distributions are compared, should we 
#'   consider lognormal type ? (see details)
#' 
#' @param best_by The criterion to use to select the best fit (one of "AIC", 
#'   "BIC" or "AICc")
#' 
#' @param xmin The xmin to be used to fit the patch size distributions. Use 
#'   the special value "estimate" to compute first the xmin that produces 
#'   the best power-law fit, then use this estimated value to fit all 
#'   distributions. 
#' 
#' @param xmin_bounds Bounds when estimating the xmin for power-law distributions
#' 
#' @param wrap Determines whether patches are considered to wrap around the 
#'  matrix when reaching the side 
#'
#' @details \code{patchdistr_spews} has been renamed, please use 
#'   \code{\link{patchdistr_sews}} instead.
#' 
#'@export
patchdistr_spews <- function(x, 
                             merge = FALSE,
                             fit_lnorm = FALSE,
                             best_by = "BIC", 
                             xmin = 1, # a number, or "estimate" option
                             xmin_bounds = NULL, 
                             wrap = FALSE) {
  .Defunct(new = "patchdistr_sews", package = "spatialwarnings")
}

#' @rdname spectral_spews-deprecated
#' 
#' @title (DEFUNCT) Spectrum-based spatial early-warning signals. 
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
#' @details \code{spectral_spews} has been renamed, please use 
#'   \code{\link{spectral_sews}} instead.
#' 
#'@export
spectral_spews <- function(mat, 
                           sdr_low_range  = NULL, 
                           sdr_high_range = NULL, 
                           quiet = FALSE) { 
  .Defunct(new = "spectral_sews", package = "spatialwarnings")
}

#' @title (DEPRECATED) Moran's Index at lag of 1
#'
#' @description This functions computes the Moran's spatial correlation index 
#'   (with lag one). It also computes a null value obtained by randomizing 
#'   the matrix.
#'
#' @references 
#'
#' Dakos, V., van Nes, E. H., Donangelo, R., Fort, H., & 
#' Scheffer, M. (2010). Spatial correlation as leading indicator of 
#' catastrophic shifts. Theoretical Ecology, 3(3), 163-174.
#'
#' Legendre, P., & Legendre, L. F. J. (2012). Numerical Ecology.
#' Elsevier Science.
#'
#' @param input An matrix or a list of matrix object. It should 
#'   be a square matrix 
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix (set to 1 for no coarse-graining).
#' 
#' @param nulln Number of replicates to produce to estimate null 
#'   distribution of index (default: 999).
#' 
#' @return A list (or a list of those if input is a list of matrix 
#'   object) of:
#'     \itemize{
#'       \item `value`: Spatial autocorrelation of the matrix
#'     }
#'   If nulln is above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean autocorrelation of the null distribution
#'       \item `null_sd`: SD of autocorrelation in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution
#'       \item `pval`: p-value based on the rank of the observed autocorrelation
#'                       in the null distribution.
#'     }
#'
#' @examples 
#' 
#' \donttest{
#' data(serengeti)
#' 
#' # One matrix
#' indicator_moran(serengeti[[1]])
#' 
#' # Several matrices
#' indicator_moran(serengeti)
#' }
#' 
#'@export
indicator_moran <- function(input, 
                            subsize = 1, # default = no cg
                            nulln = 999) {
  .Deprecated(new = "compute_indicator(input, f = raw_moran)", 
              package = "spatialwarnings")
  
  check_mat(input) # checks if binary and sensible
  # We do not check for binary status as moran's I can be computed on both. 
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_moran, subsize, nulln) )
  } else { 
    
    # We alter the moran function to do coarse_graining if the user asked for it
    #   (and not whether the matrix is binary or not). 
    if ( subsize > 1 ) { 
      indicf <- with_coarse_graining(raw_moran, subsize)
    } else { 
      indicf <- raw_moran
    }
    
    return( compute_indicator_with_null(input, nulln, indicf, 
                                        null_method = "perm", 
                                        null_control = NULL) )
  }
}

#' @title (DEPRECATED) Skewness indicator
#'
#' @description Compute the spatial skewness of spatial data. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
#'   
#' @param input A matrix or a list of matrices. The matrix 
#'   values can be logical, with \code{FALSE} (empty) or \code{TRUE} (occupied) 
#'   values. The entries can also be continuous (like NDVI or EVI data). 
#' 
#' @param subsize  Dimension of the submatrix used to coarse-grain the 
#'   original matrix. This must be an integer less than size of the full 
#'   matrix. Coarse-graining reduces the size of the matrix by a factor 
#'   \code{subsize} in each dimension of the matrix. Skewness is calculated 
#'   on the coarse-grained matrix. 
#' 
#' @param absolute Should the function return the absolute value or raw value 
#'   of skewness ?
#' 
#' @param nulln Number of simulations used to produce the null 
#'   distribution of indicator values.
#' 
#' @return A list (or a list of lists if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `value`: Spatial skewness of the matrix
#'     }
#'   If nulln is above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean skewness of the null distribution
#'       \item `null_sd`: SD of skewness in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution 
#'                          (value minus the null mean and divided by null 
#'                          standard deviation)
#'       \item `pval`: p-value based on the rank of the observed skewness
#'                       in the null distribution. A low p-value means that 
#'                       the indicator value is significantly higher than the 
#'                       null values. 
#'     }
#' 
#' @details
#' 
#' Spatial skewness is a measure of fluctuations in space; specifically, it 
#' measures if fluctuations are getting biased (skewed) in one direction. Based 
#' on the theory of critical slowing down, when systems approach critical 
#' points they are expected to show increased fluctuations in space. Thus, 
#' increasing spatial skewness is proposed as an early warning signal of 
#' impending critical transitions. 
#' 
#' Computing spatial skewness is straightforward. However, detecting trends of 
#' skewness that correspond to critical slowing down can be tricky, especially 
#' if data come from discrete classification of state variable.
#' 
#' For example, many high resolution spatial data are classified as FALSE (empty) 
#' or TRUE (occupied by plant). In such cases, spatial skewness captures just 
#' the skewness in data, but not that of spatial structure. 
#' To resolve the issue, this function employs a method called coarse-graining, 
#' proposed in Kefi et al (2014), and described in detail in 
#' Sankaran et al. (2017). One must specify a subsize above one for 
#' binary valued data sets to obtain meaningful values. 
#' 
#' \code{subsize} has to be an integer. It has to be less than or equal to 
#' half of matrix size (N). \code{subsize} must also be preferably a 
#' divisor of N. If it is not a divisor of N, the remainder rows and columns 
#' are discarded when computing coarse-graining matrices. 
#' 
#' Null model evaluations are also done on coarse-grained matrices. 
#' 
#' @references 
#' 
#' Guttal, V., and Jayaprakash, C. (2009). Spatial variance and 
#' spatial skewness: leading indicators of regime shifts in spatial 
#' ecological systems. Theoretical Ecology, 2(1), 3-12.
#' 
#' Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#' Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#' Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' Sankaran, S., Majumder, S., Kefi, S., and Guttal, V. (2017). Implication of 
#' being discrete and spatial in detecting early warning signals of regime 
#' shifts. Ecological indicators. 
#' 
#' @examples 
#' 
#' \donttest{
#' data(serengeti)
#' indicator_skewness(serengeti)
#' }
#' 
#'@export
indicator_skewness <- function(input, 
                               subsize     = 5, 
                               absolute = TRUE,
                               nulln = 999) {
  .Deprecated(new = "compute_indicator(input, f = raw_cg_skewness)", 
              package = "spatialwarnings")
  
  check_mat(input) # checks if binary and sensible
  
  if ( is.list(input) ) {
    # Returns a list of lists
    return( lapply(input, indicator_skewness, 
                   subsize, absolute, nulln) )
  } else { 
    
    indicf <- function(mat) { 
      raw_cg_skewness(mat, subsize, absolute)
    }
    
    # Compute and return the indicator
    return( compute_indicator_with_null(input, nulln, indicf, 
                                        null_method = "perm", 
                                        null_control = NULL) ) 
    
  }
}

#' @title (DEPRECATED) Spatial variance indicator
#'
#' @description This functions computes the spatial variance of spatial data. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
#' 
#' @param input A matrix or a list of matrices. The matrix 
#'   values can be logical, with \code{FALSE} (empty) or \code{TRUE} (occupied) 
#'   values. The entries can also be continuous (like NDVI or EVI data). 
#' 
#' @param subsize  Dimension of the submatrix used to coarse-grain the 
#'   original matrix. This must be an integer less than size of the full 
#'   matrix. Coarse-graining reduces the size of the matrix by a factor 
#'   \code{subsize} in each dimension of the matrix. Variance is calculated 
#'   on the coarse-grained matrix. 
#' 
#' @param nulln Number of simulations used to produce the null 
#'   distribution of indicator values.
#' 
#' @return A list (or a list of lists if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `value`: Spatial variance of the matrix
#'     }
#'   If nulln is above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean spatial variance of the null distribution
#'       \item `null_sd`: SD of spatial variance in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution 
#'                          (value minus the null mean and divided by null 
#'                          standard deviation)
#'       \item `pval`: p-value based on the rank of the observed spatial variance
#'                       in the null distribution. A low p-value means that 
#'                       the indicator value is significantly higher than the 
#'                       null values. 
#'     }
#' 
#' @details
#' 
#' Spatial variance is a measure of fluctuations in space. Based on the theory 
#' of critical slowing down, when systems approach critical points
#' they are expected to show increased fluctuations in space. Thus, increasing 
#' spatial variance is proposed as an early warning signal of impending 
#' critical transitions. 
#' 
#' For example, many high resolution spatial data are classified as FALSE (empty) 
#' or TRUE (occupied by plant). In such cases, spatial variance captures just 
#' the variance in data, but not that of spatial structure. 
#' To resolve the issue, this function employs a method called coarse-graining, 
#' proposed in Kefi et al (2014), and described in detail in 
#' Sankaran et al. (2017). One must specify a subsize above one for 
#' binary valued data sets to obtain meaningful values. 
#' 
#' \code{subsize} has to be an integer. It has to be less than or equal to 
#' half of matrix size (N). \code{subsize} must also be preferably a 
#' divisor of N. If it is not a divisor of N, the remainder rows and columns 
#' are discarded when computing coarse-graining matrices. 
#'
#' Null model evaluations are also done on coarse-grained matrices. 
#' 
#' @references 
#' 
#' Guttal, V., and Jayaprakash, C. (2009). Spatial variance and 
#' spatial skewness: leading indicators of regime shifts in spatial 
#' ecological systems. Theoretical Ecology, 2(1), 3-12.
#' 
#' Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#' Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#' Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' Sankaran, S., Majumder, S., Kefi, S., and Guttal, V. (2017). Implication 
#' of being discrete and spatial in detecting early warning signals
#' of regime shifts. Ecological Indicators. 
#' 
#' @examples
#' 
#' data(serengeti)
#' \donttest{
#' indicator_variance(serengeti, nulln = 499)
#' }
#' 
#'@export
indicator_variance <- function(input, 
                               subsize = 5, 
                               nulln = 999) {
  .Deprecated(new = "compute_indicator(input, f = raw_cg_variance)", 
              package = "spatialwarnings")
  
  check_mat(input) # checks data input
  
  if (is.list(input)) { 
    # Returns a list of lists
    return( lapply(input, indicator_variance, subsize, nulln) )
  } else { 
    
    indicf <- function(mat) { 
      raw_cg_variance(mat, subsize = subsize) 
    }
    # Compute and return indicator
    return( compute_indicator_with_null(input, nulln, indicf, 
                                        null_method = "perm", 
                                        null_control = NULL) ) 
    
  }
}

# 
#' @title (DEPRECATED) Density Ratio (SDR) indicator
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
#' @param nulln The number of simulations to compute for the null 
#'   distribution
#' 
#' @return A list (or a list of lists if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `value`: SDR of the matrix
#'     }
#'   If nulln is above 2, then the list has the following additional 
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
#' @details 
#' 
#' SDR measures the increase in long-range correlations before a critical point. 
#'   It is the ratio of the average low frequency value over high frequency 
#'   values. In this implementation, an increase in SDR implies a "reddening" 
#'   of the \link[=rspectrum]{r-spectrum}. See also \code{\link{spectral_sews}} for 
#'   a more complete description. 
#' 
#' Low and high frequencies are averaged in order to compute the SDR. The 
#'   parameters \code{sdr_low_range} and \code{sdr_high_range} control which 
#'   frequencies are selected for averaging. For example 
#'   \code{sdr_low_range = c(0, .2)} (default) uses the lower 20% to compute 
#'   the average of low frequencies. \code{sdr_high_range = c(.8, 1)} uses the 
#'   higher 20% for the average of high frequencies. 
#' 
#' @seealso spectral_sews, rspectrum
#' 
#' @references 
#' 
#' Carpenter, S.R. & Brock, W.A. (2010). Early warnings of regime shifts in 
#'   spatial dynamics using the discrete Fourier transform. Ecosphere
#' 
#' @examples 
#' 
#' \donttest{ 
#' serengeti.sdr <- indicator_sdr(serengeti, nulln = 499)
#' do.call(rbind, serengeti.sdr) # convert results to data.frame
#' }
#' 
#' @export
indicator_sdr <- function(input, 
                          sdr_low_range  = NULL, 
                          sdr_high_range = NULL, 
                          nulln = 999) { 
  
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
                   nulln) )
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
                                nulln = nulln, 
                                indicf = indicf, 
                                null_method = "perm", 
                                null_control = NULL)
  )
  
}
