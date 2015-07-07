#' Labelling of unique patches.
#' 
#' @param x A binary matrix or a list of binary matrices.

#' @return A matrix containing ID numbers for each connected patch. Assuming
#'   4-cell neighborhood and periodic boundaries.
#'   
#' @import caspr
#' @details The function is written in R and depends on the \code{mapping()}
#'   function of package caspr.
#'   
#' @export

label <- function(mat) {

	library(caspr)
  if("list" %in% class(mat)){ 
    lapply(mat, label) 
  } else {
    
    width <- dim(mat)[1]
    height <- dim(mat)[2]
    caspr::mapping(width, height)
    map <- matrix(rep(NA, times = prod(dim(mat))), ncol = width)
    old <- matrix(rep(99, times = prod(dim(mat))), ncol = width) 
    
    while(!identical(old, map)) {
      old <- map
      count = as.integer(1)
      for(i in which(mat)) {
        neighbors <- map[x_with_border][x_to_evaluate[i]+interact]
        if(all(is.na(neighbors)) ) { 
          map[i] <- count
        } else {
          map[i] <- min(neighbors, na.rm = TRUE)
        }
        count <- count +1
      }
      
    }
    
    return(map)
    
  }
} 



 
#'@title Get patch sizes.
#'  
#'@description G
#'  
#'@param mat A binary matrix or a list of binary matrices.
#'  
#'@return A vector of patch sizes. An object of class 'patchvec', or a list of
#'  objects of class 'patchvec' if `mat` was a list of binary matrices.
#'  
#'@export

patches <- function(x) { 
  
  # This part of the function implements checks and handles the case of a list
  # of matrices
  # --------------------------------
  check_mat(mat) # checks if binary and sensible
  if ( is.list(mat)) { 
    return( lapply(mat, patches) )
  }

  # Actual computation of the indicator begins here
  # --------------------------------
  
  map <- label(x) 
  patchvec <- as.vector(sapply(levels(as.factor(map)), function(i) length(which(map == i) ) )) 
  
  out <- vector()
  if(length(patchvec) > 0) out <- sort(patchvec) else out <- NA
  #out <- data.frame(size = unique(out), freq = sapply(unique(out), function(i) length(which(out >= i)) ))
  return(out)

}
  
  
