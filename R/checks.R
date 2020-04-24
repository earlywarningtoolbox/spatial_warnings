
# We add the two next lines so we do not export the functions written in this 
# file, unless explicitely specified.
#'@export
NULL

# A function that checks the arguments passed to the indicator functions.
check_mat <- function(mat) { 
  
  # Not a matrix or something we can handle ? 
  if ( ! inherits(mat, "matrix") ) {
    stop('I don\'t know what to do with an object of class ', class(mat))
  }
  
  # Has NA values
  if ( any( is.na(mat) ) ) { 
    stop('NAs in provided matrix.')
  }
  
  # Has only two unique values but it is not a logical matrix
  if ( length(unique(as.vector(mat))) == 2 && (!is.logical(mat) ) ) { 
    warning("The matrix has only two unique values, but it is not of logical ", 
            "type. Did you mean to use TRUE/FALSE values?")
  }
  
  return(TRUE)
}

warn_if_not_square <- function(mat) { 
  if ( diff(dim(mat)) != 0 ) { 
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
    warning('Trying to display a trend with only one value')
  }
  
  if ( !is.null(along) && max(obj[ ,'matrixn']) != length(along) ) { 
    stop('External data length (along = ...) does not match ',
         'the number of matrices !')
  }
  
}
