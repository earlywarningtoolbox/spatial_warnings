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
                            nreplicates = 499) {

  check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, moran_withnull, subsize, detrending, discrete, 
                   nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the Moran\'s I index requires a square matrix')
    } 
    
    return( moran_withnull(input, subsize, detrending, discrete, nreplicates) )
    
  }
}

# This function should not be use alone ----------------------------------
# This function computes the moran correlation with a random matrix.
moran_withnull <- function(input, subsize, detrending, discrete, nreplicates) {
  
  m <- mean(input) # Compute the mean value of the matrix 
  
  # Check options and apply modifications --------------------
  
  if (detrending) {
    input  <- input - m  # overwrites input locally
  }
  
  # We assign either moranCpp (no coarse graining) or moranCpp_with_cg (with
  # coarse graining) here so we test only once whether we want to coarse grain
  # or not. Otherwise a test needs to be added within the null-distribution
  # generation code (in `replicate` below), which is a big performance killer
  # and makes things less readable.
  if (discrete) {
    moran_fun <- moranCpp_with_cg # coarse-grain before computing the index
  } else { 
    moran_fun <- moranCpp         # do not
  }
  
  # Compute the Moran indicators -----------------------------
  # Note: subsize is passed but it is ignored if moran_fun is set to call 
  #   moranCpp directly.
  corr <- moran_fun(input, subsize)
  result <- list(mean = m, value = corr)
  
  if (nreplicates > 2) { 
    # Compute the index on a randomized matrix
    nulldistr <- replicate(nreplicates, 
                           moran_fun(matrix(sample(input), nrow = nrow(input)), 
                                     subsize) )
    result <- c(result,
                list(null_mean = mean(nulldistr), 
                     null_sd   = sd(nulldistr),
                     z_score   = (corr - mean(nulldistr)) / sd(nulldistr),
                     pval      = 1 - rank(c(corr, nulldistr))[1] / (nreplicates+1)))
  }
  
  return(result)
}

moranCpp <- function(mat, ...) { 
  # Arguments beyond the first are accepted but ignored
  .moranCpp(mat)
}

moranCpp_with_cg <- function(mat, subsize) { 
  .moranCpp(coarse_grain(mat, subsize))
}
