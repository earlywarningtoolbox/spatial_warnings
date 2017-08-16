#' @title Spatial variance indicator
#'
#' @description This functions computes the spatial variance of spatial data. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
#' 
#' @references Guttal, V., and Jayaprakash, C. (2009). Spatial variance and 
#' spatial skewness: leading indicators of regime shifts in spatial 
#' ecological systems. Theoretical Ecology, 2(1), 3-12.
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
  
  check_mat(input) # checks if binary and sensible
  
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

