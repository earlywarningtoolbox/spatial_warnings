 #' @title Skewness indicator
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
#' @param nreplicates Number of replicates to produce to estimate null 
#'   distribution of index.
#' 
#' @return A list (or a list of lists if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `value`: Spatial skewness of the matrix
#'     }
#'   If nreplicates is above 2, then the list has the following additional 
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
#' data(serengeti)
#' \dontrun{
#' indicator_skewness(serengeti)
#' }
#' 
#'@export
indicator_skewness <- function(input, 
                               subsize     = 5, 
                               absolute = TRUE,
                               nreplicates = 999) {
  
  check_mat(input) # checks if binary and sensible
  
  if ( is.list(input) ) {
    # Returns a list of lists
    return( lapply(input, indicator_skewness, 
                   subsize, absolute, nreplicates) )
  } else { 
    
    indicf <- function(mat) { 
      raw_cg_skewness(mat, subsize, absolute)
    }
    
    # Compute and return the indicator
    return( compute_indicator_with_null(input, nreplicates, indicf) ) 
    
  }
}
#' @title Skewness of a coarse-grained matrix 
#' 
#' @description Compute the skewness of a given matrix after coarse-graining 
#' 
#' @param mat A matrix that can contain logical (with TRUE/FALSE values)
#' 
#' @param subsize The size of the submatrices used for coarse-graining
#' 
#' @param absolute Whether to return the unmodified skewness, or its 
#'   absolute value
#' 
raw_cg_skewness <- function(mat, subsize, absolute) { 
  a <- cpp_skewness( coarse_grain(mat, subsize) )
  if (absolute) { 
    return( abs(a) ) 
  } else { 
    return(a) 
  }
}


