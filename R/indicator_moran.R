# This function computes Moran's I which is a measure of spatial autocorrelation  
# 

# TODO: Find the original paper of this Moran's index formulation.
# 
#' @title Moran's I
#' 
#' @description This function computes the Moran's I, i.e. a measure of the 
#'              spatial autocorrelation.
#' 
#' @param input A binary matrix or a list of binary matrix. 
#' 
#' @return An object of class numeric, or a list of objects of class numeric
#'         if `mat` was a list of binary matrices.
#'
#' @references P.A.P. Moran (1950) Notes on Continuous Stochastic Phenomena. 
#'             Biometrika 37(1-2):17-23  
#'
#'@export
# 
indicator_moran <- function(input) {
  check_mat(input) # checks if binary and sensible
  if ( is.list(input)) { 
    return( lapply(input, indicator_moran) )
  }
  
  if ( any(dim(input) <= 2 ) ) { 
    warning('The Moran\'s I computation requires a matrix with a size ',
         'greater than 3, returning NA.')
    return(NA)
  }
  
  m <- mean(input)            # Mean
  v <- var(as.vector(input))  # Variance
  n <- (nrow(input) - 1)      # Degrees of freedom ?
  
  moranI <- 0
  for (i in 2:n) {
    for (j in 2:n) {
      moranI <- moranI + (input[i,j]-m) * (input[i,j-1] + input[i,j+1] + 
                                           input[i-1,j] + input[i+1,j] - 4*m)
    }
  }
  
  moranI <- moranI / (4 * v * (n-2) * (n-2)) 
  return(moranI)
}

# Explicit deprecation for compatibility purposes
morancorrelation_ews <- function(input) { 
  warning("morancorrelation_ews is deprecated ! Please use indicator_moran
           instead")
  indicator_moran(input)
}
