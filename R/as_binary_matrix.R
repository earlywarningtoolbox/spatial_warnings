# 
# This file contains all the function that take input and convert it to 
# something understandable by the indicator functions.
# 
# THIS IS STILL VERY INCOMPLETE
# 

#'@export
as.binary_matrix <- function(obj) {
  UseMethod("as.binary_matrix", obj)
}

# Trivial function that returns the same object when it is already a BM
as.binary_matrix.binary_matrix <- identity 

# Convert a matrix to a binary matrix object
as.binary_matrix.matrix <- function(mat) { 
  
  # This function checks that the input matrix is well-formed. 
  check_mat(mat) 
  
  class(mat) <- c('binary_matrix', 'matrix')
  return(mat)
}

# Convert a list to a binary matrix object
as.binary_matrix.list <- function(list) { 
  
  # Check if the object is coercible to binary_matrix
  check_list(list) 
  check_mat(list)
  
  # If so, then do it
  new_obj <- lapply(list, as.binary.matrix)
  class(new_obj) <- c('binary_matrix', 'matrix')
  return(new_obj)
}

as.binary_matrix.data.frame <- function(df) { 
  if ( !( all(sapply(df, class) == 'factor')  || 
          all(sapply(df, class) == 'numeric') || 
          all(sapply(df, class) == 'logical') ) ) { 
    stop('Cannot convert data.frame with mixed column classes to a 
          binary matrix.')
  }
  
  # Convert df to matrix
  new_obj <- as.matrix(df)
  check_mat(new_obj)
  class(new_obj) <- c('binary_matrix', 'matrix')
  return(new_obj)  
}

