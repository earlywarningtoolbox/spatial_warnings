#' @title Get patch sizes.
#' 
#' @description Get the distribution of patch sizes
#' 
#' @param x A binary matrix or a list of binary matrices.
#' 
#' @return A vector of patch sizes or a list of vectors if the input was a list
#'   of binary matrices.
#' 
#' @examples
#' data(B)
#' patchsizes(B)
#' @export
patchsizes <- function(x) { 
  
  # This part of the function implements checks and handles the case of a list
  # of matrices
  # --------------------------------
  check_mat(x) # checks if binary and sensible
  
  if ( is.list(x)) { 
    return( lapply(x, patchsizes) )
  }
  
  # Actual computation of the indicator begins here
  # --------------------------------
  
  map <- label(x) 
  patchvec <- sapply(seq.int(max(map, na.rm=TRUE)),
                     function(i) sum(map == i, na.rm = TRUE) ) 
  patchvec <- sort(patchvec)
  
  return(patchvec)
}

