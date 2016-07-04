
# We add the two next lines so we do not export the functions written in this 
# file, unless explicitely specified.
#'@export
NULL

# @title Check for binary matrix
# @description A function that checks the arguments passed to the indicator 
#   functions.
#
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
  # NOTE: we do not test for this now as functions can accept non-binary
  #   matrices !
#   if ( length(table(mat)) > 2 ) { # More than 2 unique elements
#     warning('The matrix is not binary')
#   }
  
  return(TRUE)
}

check_list <- function(l) { 
  
  # Check that lists all contain the same elements
  are_matrix <- sapply(l, function(x) is.matrix(x) | is.binary_matrix(x) )
  if ( ! all(are_matrix) ) { 
    stop('The provided list does not contain only matrices or binary_matrices ',
         'to compute indicators')
  }
  
  
  # We select the first class which is either binary_matrix or matrix (if it is 
  #   just a simple matrix)
  unique_classes <- unique(as.vector(sapply(l, function(x) class(x)[1])))
  if ( length(unique_classes) > 1 ) { 
    warning('The provided list contains matrices and binary matrices: this is ',
            'likely an error')
  }
  
  # Check if matrices have the same dimension. We compare them sequentially 
  # and check for a change in dimensions.
  dims <- do.call(rbind, lapply(l, dim))
  ddims <- apply(dims, 2, diff)
  if ( any(ddims > 0) ) { 
    warning('Matrices in the provided list do not have the same size.')
  }
  
}

warn_if_not_square <- function(mat) { 
  if (diff(dim(mat)) != 0) { 
    warning('The matrix is not square: indicator_sdr will only use a square ', 
            'subset centered around the middle point.')
  } 
}

check_binary_status <- function(mat) { 
  
  N_unique_values <- length(unique(as.vector(mat)))
  
  if ( N_unique_values <= 2 && !is.binary_matrix(mat)) { 
    warning('The matrix looks binary but has not been converted to a ',
            'binary_matrix object. ', 
            'This will change the results of the indicator functions and ', 
            'is likely to be an error. ', 
            'Please use function as.binary_matrix to convert objects to binary
            matrices.')
  }
  
}


# Check whether some variables are suited to make plots : used in task_generic
#   and task_spectral spews
check_suitable_for_plots <- function(obj, 
                                     along, 
                                     display_null) { 
  
  if ( ! 'replicate' %in% colnames(obj) || 
        !is.null(along) && length(along) <= 1 ) { 
    stop('I cannot plot a trend with only one value')
  }
  
  if ( length(unique(obj[ ,'replicate'])) != length(along) ) { 
    stop('External data length (along = ...) does not match ',
         'the number of replicates !')
  }
  
}
