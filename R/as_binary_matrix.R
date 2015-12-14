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
#'@export
as.binary_matrix.binary_matrix <- identity 

# Convert a matrix to a binary matrix object
#'@export
#'@rdname as.binary_matrix
as.binary_matrix.matrix <- function(x, state = NULL) { 
  
  if ( any(is.na(x)) ) { 
    stop("Object of class `binary_matrix` cannot contain NAs.")
  }
  
  if ( is.numeric(x) || is.integer(x) || is.character(x) ) { 
    if ( is.null(state) ) { 
      stop('Input object is not of logical type: I don\'t know how to convert it ', 
           'to a binary matrix without a reference state (state = ... argument)') 
    }
    x <- x == state
  }
  
  
  # This function checks that the input matrix is well-formed. 
  check_mat(x) 
  
  class(x) <- c('binary_matrix', 'matrix')
  return(x)
}

# Convert a list to a binary matrix object
#'@export
#'@rdname as.binary_matrix
as.binary_matrix.list <- function(x, state = NULL) { 
  
  new_obj <- lapply(x, as.binary_matrix, state)
  class(new_obj) <- c('binary_matrix', 'list')
  return(new_obj)
}
  
# Convert a data.frame
#'@export
#'@rdname as.binary_matrix
as.binary_matrix.data.frame <- function(x, state = NULL) { 
  
  if ( length(unique(sapply(x, class))) > 1 )  { 
    stop('Cannot convert data.frame with mixed column classes to a ',
          'binary matrix.')
  }
  
  # We passed a factor-only x -> convert to character then handle it as 
  # a character x
  if ( allcols_are(x, 'factor') ) { 
    x <- do.call(data.frame, c(lapply(x, as.character), 
                                stringsAsFactors = FALSE))
  }
  
  # We have now a x with all logical (binary) columns: the x is basically a 
  # representation of a matrix, just in the wrong format. We just convert it
  # and pass it to as.binary_matrix.matrix
  new_obj <- as.binary_matrix(as.matrix(x), state = state)
  
  return(new_obj) 
}

# helper
allcols_are <- function(df, class_test) { 
  all(sapply(df, class) == class_test) 
}
