# 
# A function that checks the arguments passed to the indicator functions.
# 

check_mat <- function(mat) { 
  
  # If a list is passed then we check all elements
  if ( is.list(mat) ) { 
    return( lapply(mat, check_mat) )
  }
  
  # Not a matrix ? 
  if ( ! inherits(mat, 'matrix') ) {
    stop('I don\'t know what to do with an object of class ', class(mat))
  }
  
  # Has NA values
  if ( any( is.na(mat) ) ) { 
    warning('NAs in matrix')
  }
  
  # Not a *binary* matrix ? 
  if ( length(table(mat)) > 2 ) { # More than 2 unique elements
    stop('The matrix is not binary, please check input')
  }
  
  # Add more testing: 
  #   - what if matrices have different sizes ? 
}