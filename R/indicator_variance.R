#' @title Spatial variance indicator
#'
#' @description This functions computes the spatial variance critical point indicator. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.

#' @references Guttal, V., and Jayaprakash, C. (2009). Spatial variance and 
#' spatial skewness: leading indicators of regime shifts in spatial 
#' ecological systems. Theoretical Ecology, 2(1), 3-12.
#' 
#' @param input A square binary matrix or a list of square binary matrices. 
#' 
#' @param discrete logical. If TRUE the data represent discrete variables (like 
#'   presence/absense), otherwise continuous data (like biomass density). 
#'   Defaults to FALSE.
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix.
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
#'       \item `value`: Spatial autocorrelation of the matrix
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
#' data(B)
#' indicator_variance(B)
#' 
#' 
#'@export
indicator_variance <- function(input, 
                               subsize     = 5, 
                               detrending  = FALSE, 
                               discrete    = TRUE,
                               nreplicates = 499) {
  
  check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_variance, 
                   subsize, detrending, discrete, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the variance indicator requires a square matrix')
    } 
    
    return( 
      compute_indicator_with_null(input, subsize, detrending, 
                                  discrete, nreplicates, 
                                  indicator_function = 
                                    function(input) var(as.vector(input)) )
    )      
    
  }
}
