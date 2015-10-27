# 
# This file contains all the function that take input and convert it to 
# something understandable by the indicator functions.
# 
# 

#' @title Coerce to a binary matrix
#' 
#' @param x Any R object
#' @param state A reference state to use when converting to a binary matrix
#' 
#' @details `as.binary_matrix` is a generic function that tries to convert 
#'   object to binary matrices. It can be applied to lists of objects: in this 
#'   case it will try to convert each object individually and return a list. 
#' 
#' @examples
#' 
#' # Matrix example: convert all cells to binary data using state 3 as reference
#' mat <- matrix(sample(c(1,2,3), 100, replace = TRUE), nrow = 10, ncol = 10)
#' as.binary_matrix(mat, state = 3) 
#' 
#'@export
as.binary_matrix <- function(x, state = NULL) {
  UseMethod("as.binary_matrix")
}

#'@export
#'@rdname as.binary_matrix
is.binary_matrix <- function(x) inherits(x, 'binary_matrix')

# Trivial function that returns the same object when it is already a BM
as.binary_matrix.binary_matrix <- identity 

# Convert a matrix to a binary matrix object
as.binary_matrix.matrix <- function(mat, state = NULL) { 
  
  if ( is.numeric(mat) || is.integer(mat) || is.character(mat) ) { 
    if ( is.null(state) ) { 
      stop('Input object is not of logical type: I don\'t know how to convert it ', 
           'to a binary matrix without a reference state (state = ... argument)') 
    }
    mat <- mat == state
  }
  
  
  # This function checks that the input matrix is well-formed. 
  check_mat(mat) 
  
  class(mat) <- c('binary_matrix', 'matrix')
  return(mat)
}

# Convert a list to a binary matrix object
as.binary_matrix.list <- function(list, state = NULL) { 
  
  new_obj <- lapply(list, as.binary_matrix, state)
  class(new_obj) <- c('binary_matrix', 'list')
  return(new_obj)
}
  
# Convert a data.frame
as.binary_matrix.data.frame <- function(df, state = NULL) { 
  
  if ( length(unique(sapply(df, class))) > 1 )  { 
    stop('Cannot convert data.frame with mixed column classes to a ',
          'binary matrix.')
  }
  
  # We passed a factor-only df -> convert to character then handle it as 
  # a character df
  if ( allcols_are(df, 'factor') ) { 
    df <- do.call(data.frame, c(lapply(df, as.character), 
                                stringsAsFactors = FALSE))
  }
  
  # We have now a df with all logical (binary) columns: the df is basically a 
  # representation of a matrix, just in the wrong format. We just convert it
  # and pass it to as.binary_matrix.matrix
  new_obj <- as.binary_matrix(as.matrix(df), state = state)
  
  return(new_obj) 
}

# helper
allcols_are <- function(df, class_test) { 
  all(sapply(df, class) == class_test) 
}
