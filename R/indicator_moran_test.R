##  Generic spatial early warning signals : moran correlation
## Code originally written by V. Guttal and modified by S. Majumder
# The dependencies are packages: moments, plotrix and fields.
# This function calls another function called "reducedmatrix_ews.R" and "morancorrelation_ews.R" which should be saved in the same folder.

#' @title Moran's Index at lag of 1
#'
#' @description This functions computes the Moran's Index with a lag of one. It computes the actual spatial autocorrelation and the autocorrelation expected if the spatial structure is random. 
#'
#'    @param input An object of matrix class or a list of matrix object. It should be a square matrix 
#'
#'    @param discrete logical. If TRUE the data represent discrete variables (like presence/absense), otherwise continuous data (like biomass density). Default is FALSE.
#'
#'    @param subsize is the dimension of the submatrix used to subsample the rawmatrix either to reduce the size of the original matrix
#'
#'    @param detrending logical. If TRUE data are detrended by removing the spatial mean. (Default is FALSE)
#'
#'   @return It returns a list (or a list of list if input was a list of matrix object) of:
#'\itemize{
#'\item `Mean`: Landscape mean cover
#'\item `MoranNullCorr`: Autocorrelation computed in the randomized matrix.
#'\item `MoranCorr`: Spatial autocorrelation of the matrix
#'}
#'
#'   @export  
#'

Moran1 <- function(input, subsize = 2, detrending = FALSE, discrete = TRUE) {

  check_mat(input) # checks if binary and sensible

  if (is.list(input)) {

  out <- lapply(input, moran(x, subsize, detrending, discrete)) # Moran Index and null model computation 
  return(out)                          #Returns a list of lists

  } else {

  out  <- moran(input, subsize, detrending, discrete)
  return(out)                          # Returns a list

  }
}

# The other function should not be use alone ----------------------------------

moran <- function(input, subsize = 2, detrending = FALSE, discrete = TRUE){

  m <- mean(as.vector(input))          #Compute the mean value of the matrix 

# Check options and apply modifications --------------------

  if (detrending == TRUE) {
  mat  <- input - m 

  } else {
  mat  <- input
  }

  if (discrete==TRUE){
  mat <- reducedmatrix_ews(mat,subsize)  # Coarsed grain the matrix
    }

# Compute the Moran indicator -----------------------------

  rdmat    <- matrix(sample(mat), ncol = ncol(mat), nrow = nrow(mat)) # For random matrix
  nullcorr <- moran_computation(rdmat) # 
  corr     <- moran_computation(mat)    # For the data
  result   <- list(Mean = m, MoranNullCorr = nullcorr, MoranCorr = corr)

  return(result)
}

moran_computation <- function(input){

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
