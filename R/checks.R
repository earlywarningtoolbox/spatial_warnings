#' @title Check for binary matrix
#' @description A function that checks the arguments passed to the indicator 
#'   functions.
#' 
check_mat <- function(mat) { 
  
  # If a list is passed then we do list-level cheks and check all elements
  if ( is.list(mat) ) { 
    check_list(mat)
    lapply(mat, check_mat)
    
    return(TRUE)
  }
  
  # Not a matrix ? 
  if ( ! inherits(mat, 'matrix') ) {
    stop('I don\'t know what to do with an object of class ', class(mat))
  }
  
  # Has NA values
  if ( any( is.na(mat) ) ) { 
    stop('NAs in provided matrix.')
  }
  
  # Not a *binary* matrix ? 
  if ( length(table(mat)) > 2 ) { # More than 2 unique elements
    stop('The matrix is not binary, please check input')
  }
  
  return(TRUE)
}

check_list <- function(l) { 
  
  # Check if matrices have the same dimension. We compare them sequentially 
  # and check for a change in dimensions.
  dims <- do.call(rbind, lapply(l, dim))
  ddims <- apply(dims, 2, diff)
  if ( any(ddims > 0) ) { 
    warning('Matrices in the provided list do not have the same size.')
  }
  
}
