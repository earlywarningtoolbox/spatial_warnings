
#' @title Spatial warning indicators: largest patch
#' 
#' @description Returns the size of the largest patch of the matrix. 
#'
#' @param mat A binary matrix or a list of binary matrices.
#' 
#' @param patchvec A patch vector obtained from \code{\link{patchsizes}}.
#' 
#' 
#' @return An object of class <myclass>, or a list of objects of class <myclass>
#'         if `mat` was a list of binary matrices.
#'
#'@export
#'
indicator_largestpatch <- function(mat = NULL, patchvec = patchsizes(mat)) { 
  
  # This part of the function implements checks and handles the case of a list
  # of matrices
  # --------------------------------
  if ( ! is.null(mat) ) { 
    check_mat(mat) # checks if binary and sensible
    if ( is.list(mat) ) { # We passed a list a matrices
    return( lapply(mat, indicator_largestpatch) ) # add all the other function arguments here
    }
  }
  
  # Actual computation of the indicator begins here
  # --------------------------------
  
  max(patchvec)
}
