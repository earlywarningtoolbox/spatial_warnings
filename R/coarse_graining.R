# 
#' @title Matrix coarse-graining
#' 
#' @description This function averages the spatial data locally. It divides 
#'   the input matrix into submatrices of dimension \code{subsize} and 
#'   averages the spatial data in these submatrices. By doing this, the 
#'   dimension of resultant matrix is reduced by a factor of 
#'   \code{subsize}. 
#' 
#' @details If the data is classified into discrete units, the calculation of 
#' variance and skewness can give spurious results irrelevant to the proximity 
#' to transition. Therefore, discrete data should be 'coarse-grained' before 
#' calculating the spatial early warning signals. However, this can also be 
#' applied to continuous state data. 
#' 
#' @references 
#' 
#'   Sankaran, S., Majumder, S., Kefi, S. and Guttal, V. (2017). Implications 
#'   of being discrete and spatial for detecting early warning signals of 
#'   regime shifts. Ecological Indicators. 
#' 
#' @param mat A matrix
#' 
#' @param subsize Dimension of the submatrix. This has to be a positive 
#' integer smaller than the dimension of input matrix.  
#' 
#' @return A matrix of reduced dimension.
#' 
#' @seealso \code{\link{generic_spews}}
#' 
#' @examples 
#' rmat <- matrix(runif(20*10) > .5, 
#'                ncol = 20, nrow = 10)
#' rmat.cg <- coarse_grain(rmat, subsize = 2)
#' 
#' par(mfrow = c(1, 2))
#' image(rmat)
#' title('Raw matrix') 
#' image(rmat.cg) 
#' title('Coarse-grained matrix')
#' 
#'@export
coarse_grain <- function(mat, subsize) { 
  
  if ( subsize < 1 ) { 
    warning('Cannot coarse-grain a matrix with a subsize argument under 1, ', 
            'returning the matrix unchanged')
    return(mat)
  }
  
  coarse_grain_cpp(mat, subsize)
  
}
  
