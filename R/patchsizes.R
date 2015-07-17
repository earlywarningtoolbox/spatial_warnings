#'@title Get patch sizes.
#'  
#'@description Labels patches and counts patch size. 
#'  
#'@param x A binary matrix or a list of binary matrices.
#'  
#'@return A vector of patch sizes. An object of class 'patchvec', or a list of
#'  objects of class 'patchvec' if `x` was a list of binary matrices.
#'  
#'@export

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
  
  map <- spatialwarnings::label(x) 
  patchvec <- as.vector(sapply(levels(as.factor(map)), function(i) length(which(map == i) ) )) 
  
  out <- vector()
  if(length(patchvec) > 0) out <- sort(patchvec) else out <- NA
  #out <- data.frame(size = unique(out), freq = sapply(unique(out), function(i) length(which(out >= i)) ))
  return(out)
  
}

