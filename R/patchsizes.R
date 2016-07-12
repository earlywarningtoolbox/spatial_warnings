#' @title Get patch sizes.
#' 
#' @description Get the distribution of patch sizes
#' 
#' @param x A binary matrix or a list of binary matrices.
#' 
#' @return A vector of patch sizes or a list of vectors if the input was a list
#'   of binary matrices. If input contained no patches (all values to FALSE), 
#'   then NA is returned.
#' 
#' @examples
#' data(forestdat)
#' patchsizes(forestdat[['matrices']][[1]])
#'
#' @export
patchsizes <- function(x) { 
  
  # This part of the function implements checks and handles the case of a list
  # of matrices
  # --------------------------------
  check_mat(x) # checks if binary and sensible
  
  if ( is.list(x)) { 
    return( lapply(x, patchsizes) )
  }
  
  # If there is no patch at all -> return NA
  if ( ! any(x) ) { 
    return( numeric() )
  }
  
  # Actual computation of the indicator begins here
  # --------------------------------
  
  map <- label(x) 
  patchvec <- sapply(seq.int(max(map, na.rm=TRUE)),
                     function(i) sum(map == i, na.rm = TRUE) ) 
  patchvec <- sort(patchvec)
  
  return(patchvec)
}

# Get the higher tail cumulative distribution of something (P(x >= k))
cumpsd <- function(dat) { 
  x <- sort(unique(dat))
  N <- length(dat)
  y <- sapply(x, function(k) { sum(dat >= k) / N })
  return( data.frame(x = x, y = y) )
}

