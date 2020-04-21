
# We add the two next lines so we do not export the functions written in this 
# file, unless explicitely specified.
#'@export
NULL

# A function that checks the arguments passed to the indicator functions.
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
    
  return(TRUE)
}

check_list <- function(l) { 
  
  # Check that lists all contain the same elements
  are_matrix <- sapply(l, function(x) is.matrix(x) )
  if ( ! all(are_matrix) ) { 
    stop('The provided list does not contain only matrices')
  }
  
  
  unique_types <- unique(sapply(l, function(x) typeof(x)))
  if ( length(unique_types) > 1 ) { 
    warning('The provided list contains matrices of different data types: this ',
            'is most likely an error')
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

# Check whether some variables are suited to make plots : used in task_generic
#   and task_spectral sews
check_suitable_for_plots <- function(obj, 
                                     along) { 
  
  if ( ! 'matrixn' %in% colnames(obj) || 
        ( !is.null(along) && length(along) <= 1 ) ) { 
    stop('I cannot plot a trend with only one value')
  }
  
  if ( !is.null(along) && max(obj[ ,'matrixn']) != length(along) ) { 
    stop('External data length (along = ...) does not match ',
         'the number of matrices !')
  }
  
}
