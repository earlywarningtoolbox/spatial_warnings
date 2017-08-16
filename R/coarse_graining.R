# 
# 
# This file is a wrapper around the .coarse_grain_unsafe routine to catch 
#   the case when subsize = 0 that crashes R otherwise. 
# 

#' @title Matrix coarse-graining
#' 
#' @description This function coarse-grains the matrix. 
#' The matrix is divided into submatrices of dimention subsize and the spatial data in these submatrices is averaged.
#' The dimention of resultant matrix is reduced by a factor of subsize.
#' 
#' @param mat A square matrix
#' 
#' @param subsize Dimetion of the submatrix. This has to be a positive integer smaller than the dimention of input matrix. 
#' 
#' @return A square matrix of reduced dimention.
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
  
