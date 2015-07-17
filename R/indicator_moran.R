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
#' @param nreplicates Number of replicates to produce to estimate null 
#'   distribution of index (default: 999).
#' 
#' @return A list (or a list of list if input was a list of matrix 
#'   object) of:
#'     \itemize{
#'       \item `mean`: Landscape mean cover
#'       \item `corr`: Spatial autocorrelation of the matrix
#'     }
#'   If nreplicates was above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean autocorrelation of the null distribution
#'       \item `null_sd`: SD of autocorrelation in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution
#'       \item `pval`: p-value based on the rank of the observed autocorrelation
#'                       in the null distribution.
#'     }
#'
#' @examples 
#' data(B)
#' indicator_moran(B)
#'
#' 
#'@export  
indicator_moran <- function(input, 
                            subsize     = 2, 
                            detrending  = FALSE, 
                            discrete    = TRUE,
                            nreplicates = 999) {

  check_mat(input) # checks if binary and sensible
  
  if (diff(dim(input)) != 0) { 
    stop('Computation of the Moran\'s I index requires a square matrix')
  } 
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, moran_withnull, subsize, detrending, discrete, 
                   nreplicates) )
  } else { 
    return( moran_withnull(input, subsize, detrending, discrete, nreplicates) )
  }
}


# This function should not be use alone ----------------------------------
# This function computes the moran correlation with a random matrix.
moran_withnull <- function(input, subsize, detrending, discrete, nreplicates) {
  
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
  corr     <- moranCpp(mat) # Actual
  
  result <- list(mean = m, corr = corr)
  
  if (nreplicates > 2) { 
    nulldistr <- replicate(nreplicates,
                           moranCpp(matrix(sample(mat), nrow=nrow(mat))))
    result <- c(result,
                list(null_mean = mean(nulldistr), 
                     null_sd   = sd(nulldistr),
                     z_score   = (corr - mean(nulldistr)) / sd(nulldistr),
                     pval      = rank(c(corr, nulldistr))[1] / (nreplicates+1)))
  }
  
  return(result)
}

# Not used (converted to c++), but kept here for reference.

# moran_correlation <- function(input) {
#   
#   if ( any(dim(input) <= 2 ) ) { 
#     warning('The Moran\'s I computation requires a matrix with a size ',
#          'greater than 3, returning NA.')
#     return(NA)
#   }
#   
#   m <- mean(input)            # Mean
#   v <- var(as.vector(input))  # Variance
#   n <- (nrow(input) - 1)      # Degrees of freedom ?
#   
#   moranI <- 0
#   for (i in 2:n) {
#     for (j in 2:n) {
#       moranI <- moranI + (input[i,j]-m) * (input[i,j-1] + input[i,j+1] + 
#                                            input[i-1,j] + input[i+1,j] - 4*m)
#     }
#   }
#   
#   moranI <- moranI / (4 * v * (n-2) * (n-2)) 
#   return(moranI)
# 
# }
