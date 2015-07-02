#  
# This file contains two example functions that compute an indicator given
#   a binary matrix, with or without specifying a required intermediate result.
# 


# 
#' @title Spatial warning indicators: example
#' 
#' @description This indicator is a simple example. Please modify it ! :)
#' 
#' @param mat A binary matrix or a list of binary matrices.
#' 
#' @param myparam1 <Explanation>
#' 
#' @param myparam2 <Explanation>
#' 
#' 
#' @return An object of class <myclass>, or a list of objects of class <myclass>
#'         if `mat` was a list of binary matrices.
#'
#' @references Thisguy et al., 2012 A new spatial indicator for critical points#'   
#'
#'@export
indicator_example <- function(mat) { 
  
  # This part of the function implements checks and handles the case of a list
  # of matrices
  # --------------------------------
  check_mat(mat) # checks if binary and sensible
  if ( is.list(mat)) { 
    return( lapply(mat, indicator_example) )
  }
  
  # Actual computation of the indicator begins here
  # --------------------------------
  
  
  result <- sum( mat == 1 ) # 
  
  
  warning('This is just a demo and needs to be modified.')
  return(result)
}



#' @title Spatial warning indicators: fancy example
#' 
#' @description This indicator is a simple example. Please modify it ! :)
#'
#' @param mat A binary matrix or a list of binary matrices.
#' 
#' @param interm_result My intermediate result (like patch_size distribution)
#' 
#' @param myparam2 <Explanation>
#' 
#' 
#' @return An object of class <myclass>, or a list of objects of class <myclass>
#'         if `mat` was a list of binary matrices.
#'
#' @references Thisguy et al., 2012 A new spatial indicator for critical points
#'    
#'@export
indicator_example_with_interm_result <- function(mat = NULL, 
                                                 interm_result = NULL) { 
  
  # This part of the function implements checks and handles the case of a list
  # of matrices
  # --------------------------------
  if ( ! is.null(mat) ) { 
    check_mat(mat) # checks if binary and sensible
    if ( is.list(mat) ) { # We passed a list a matrices
    return( lapply(mat, indicator_example_with_interm_result, 
                   interm_result) ) # add all the other function arguments here
    }
  }
  
  # No intermediate result passed -> compute it
  if ( is.null(interm_result) ) { 
    interm_result <- compute_it(mat) 
  }
  
  # Actual computation of the indicator begins here
  # --------------------------------
  
  result <- max( interm_result ) 

  
  warning('This is just a demo and needs to be modified.')
  return(result)
}

# NB: we might need to complexify a bit the handling of arguments if we want 
#   the function to be able to handle a list of interm_results.

# NB2: Check out the doc by doing 
# document() # then 
# ?indicator_example 

