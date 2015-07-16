##  Generic spatial early warning signals : moran correlation
## Code originally written by V. Guttal and modified by S. Majumder

#' @title Moran's Index at lag of 1
#'
#' @description This functions computes the Moran's spatial correlation index 
#'   (with lag one). It also computes a null value obtained by randomizing 
#'   the matrix.
#' 
#' @param input An matrix or a list of matrix object. It should 
#'   be a square matrix 
#' 
#' @param discrete logical. If TRUE the data represent discrete variables (like 
#'   presence/absense), otherwise continuous data (like biomass density). 
#'   Defaults to FALSE.
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix.
#' 
#' @param detrending If TRUE data are detrended by removing the spatial mean. 
#'   (Default is FALSE)
#' 
#' @return A list (or a list of list if input was a list of matrix 
#' object) of:
#'   \itemize{
#'     \item `Mean`: Landscape mean cover
#'     \item `MoranNullCorr`: Autocorrelation computed in the randomized matrix.
#'     \item `MoranCorr`: Spatial autocorrelation of the matrix
#'   }
#'
#'@export  
indicator_moran <- function(input, 
                            subsize = 2, 
                            detrending = FALSE, 
                            discrete = TRUE) {

  check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, moran_withnull, subsize, detrending, discrete) )
  } else { 
    return( moran_withnull(input, subsize, detrending, discrete) )
  }
}


# This function should not be use alone ----------------------------------
# This function computes the moran correlation with a random matrix.
moran_withnull <- function(input, 
                           subsize = 2, 
                           detrending = FALSE, 
                           discrete = TRUE) {
  
  mat  <- input
  m <- mean(input) # Compute the mean value of the matrix 
  
  # Check options and apply modifications --------------------
  
  if (detrending) {
    mat  <- input - m 
  }
  
  if (discrete) {
    mat <- coarse_grain(mat, subsize)  # Coarse-grain the matrix
  }
  
  # Compute the Moran indicators -----------------------------
  # Randomize matrix
  rdmat    <- matrix(sample(mat), ncol = ncol(mat), nrow = nrow(mat)) 
  nullcorr <- moran_correlation(rdmat) # Null
  corr     <- moran_correlation(mat)   # Actual
  result   <- list(Mean = m, 
                   MoranNullCorr = nullcorr, 
                   MoranCorr = corr)
  
  return(result)
}

moran_correlation <- function(input) {
  
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
