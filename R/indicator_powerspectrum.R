#' 
#' @title Spatial warning indicators: Power Spectrum
#'
#' @description Function to calculate theta and r spectrum.
#' 
#' @param mat A binary matrix or a list of binary matrices
#' 
#' @return A list of lists(of theta and R spectrum values for distances 1 to 
#'   SystemSize/2 for each matrix in the list) if input is a list (OR) a single 
#'   list (of theta and R spectrum values for distances 1 to SystemSize/2) if 
#'   input is a single matrix
#' 
#' @details writeme
#' 
#' @export
indicator_powerspectrum <- function(mat) {
  
  # Handles mat if it is a list
  check_mat(mat)
  if ( is.list(mat) ) { 
    return( lapply(mat, indicator_powerspectrum) ) 
  }
  
  rspectrum(mat)
  
}
