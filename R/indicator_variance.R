#' @title Spatial variance indicator
#'
#' @description This functions computes the spatial variance of spatial data. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
#' 
#' @references 
#' Guttal, V., and Jayaprakash, C. (2009). Spatial variance and 
#' spatial skewness: leading indicators of regime shifts in spatial 
#' ecological systems. Theoretical Ecology, 2(1), 3-12.
#' 
#' Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#' Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#' Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#' 
#' Sankaran, S., Majumder, S., Kefi, S., and Guttal, V. (2017). Implication of being discrete and spatial in detecting early warning signals
#' of regime shifts
#' 
#' @param input A square matrix or a list of square matrices. The matrix entires can be binary, representing 0 (empty) or 1 (occupied). 
#' The entries can also be continuous (like NDVI or EVI data). 
#' 
#' @param subsize Dimension of the submatrix used to coarse-grain the 
#'   original matrix. This must be an integer less than size of the full matrix. Coarse-graining reduces the size
#' of the matrix by a factor subsize in each dimension of the matrix. Variance is calculated on the coarse-grained matrix. 
#' 
#' @param detrending If TRUE data are detrended by removing the spatial mean. 
#'   (Default is FALSE). 
#' 
#' @param nreplicates Number of replicates to produce to estimate null 
#'   distribution of index.
#' 
#' @return A list (or a list of list if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `mean`: Landscape mean cover
#'       \item `value`: Spatial variance of the matrix
#'     }
#'   If nreplicates was above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean autocorrelation of the null distribution
#'       \item `null_sd`: SD of autocorrelation in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution
#'       \item `pval`: p-value based on the rank of the observed autocorrelation
#'                       in the null distribution.
#'     }
#' 
#' @details
#' Spatial variance is a measure of fluctuations in space. Based on the theory of critical slowing down, when systems approach critical points
#' they are expected to show increased fluctuations in space. Thus, increasing spatial variance is proposed as an early warning signal of impending critical transitions. 
#' 
#' Computing spatial variance is straightforward. The function uses moments package to calcualte variance. However, detecting trends of variance that correspond to critical slowing down can be tricky, espeically if data come from discrete classification of state variable.
#' For example, many high resolution spatial data are classified are 0 (empty) or 1 (occupied by plant). In such cases, spatial variance captures just the variance in data, but not that of spatial structure. 
#' To resolve the issue, this function employs a method called coarse-graining, proposed in Kefi et al 2014 and described in detail in Sanakaran et al 2017. 
#' Therefore, one must specify subsize for binary valued data sets. 
#' 
#' Input matrices can be either binary valued, or continuous data (numeric). 
#'
#' subsize has to be an integer. It has to be less than or equal to half of matrix size (N). subsize must also be preferably a divisor of N. 
# If it is not a divisor of N, the remainde rows and columns are discarded when computing coarse-graining matrices. 
#'
#' Null model evaluations are also done on coarse-grained matrices. 
#' 
#' @examples
#' 
#' data(forestgap)
#' indicator_variance(forestgap)
#' 
#' 
#'@export
indicator_variance <- function(input, 
                               subsize     = 5, 
                               detrending  = FALSE, 
                               nreplicates = 999) {
  
  check_mat(input) # checks data input
  
  if (is.list(input)) { 
    # Returns a list of lists
    return( lapply(input, indicator_variance, 
                   subsize, detrending, nreplicates) )
  } else { 
    
    # We alter the raw_variance function so it includes coarse_graining 
    #   in case subsize is above 1. 
    indicf <- raw_variance
    if ( subsize > 1 ) { 
      indicf <- with_coarse_graining(raw_variance, subsize)
    } 
    
    # Compute and return indicator
    return( compute_indicator_with_null(input, detrending, 
                                        nreplicates, indicf) ) 
    
  }
}

raw_variance <- function(mat) { var(as.vector(mat)) }

