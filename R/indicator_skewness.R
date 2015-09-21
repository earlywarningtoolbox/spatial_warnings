#' @title Skewness indicator
#'
#' @description This functions computes the skewness critical point indicator. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
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
#'       \item `value`: Spatial skewness of the matrix
#'     }
#'   If nreplicates was above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean skewness of the null distribution
#'       \item `null_sd`: SD of skewness in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution
#'       \item `pval`: p-value based on the rank of the observed skewness
#'                       in the null distribution.
#'     }
#' 
#' @examples 
#' data(B)
#' indicator_skewness(B)
#' 
#' 
#'@export
indicator_skewness <- function(input, 
                               subsize     = 2, 
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
      stop('Computation of the skewness indicator requires a square matrix')
    } 
    
    return( 
      compute_indicator_with_null(
        input, subsize, detrending, 
        discrete, nreplicates, 
        indicator_function = function(input) moments::skewness(as.vector(input))
    )) 
    
  }
}
