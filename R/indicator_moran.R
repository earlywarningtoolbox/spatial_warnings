#' @title Moran's Index at lag of 1
#'
#' @description This functions computes the Moran's spatial correlation index 
#'   (with lag one). It also computes a null value obtained by randomizing 
#'   the matrix.
#'
#' @references 
#'
#' Dakos, V., van Nes, E. H., Donangelo, R., Fort, H., & 
#' Scheffer, M. (2010). Spatial correlation as leading indicator of 
#' catastrophic shifts. Theoretical Ecology, 3(3), 163-174.
#'
#' Legendre, P., & Legendre, L. F. J. (2012). Numerical Ecology.
#' Elsevier Science.
#'
#' @param input An matrix or a list of matrix object. It should 
#'   be a square matrix 
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix (set to 1 for no coarse-graining).
#' 
#' @param nreplicates Number of replicates to produce to estimate null 
#'   distribution of index (default: 999).
#' 
#' @return A list (or a list of those if input is a list of matrix 
#'   object) of:
#'     \itemize{
#'       \item `value`: Spatial autocorrelation of the matrix
#'     }
#'   If nreplicates is above 2, then the list has the following additional 
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
#' 
#' data(serengeti)
#' indicator_moran(serengeti)
#'
#' 
#'@export
indicator_moran <- function(input, 
                            subsize     = 1, # default = no cg
                            nreplicates = 999) {
  
  check_mat(input) # checks if binary and sensible
  # We do not check for binary status as moran's I can be computed on both. 
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_moran, subsize, nreplicates) )
  } else { 
    
    # We alter the moran function to do coarse_graining if the user asked for it
    #   (and not whether the matrix is binary or not). 
    if ( subsize > 1 ) { 
      indicf <- with_coarse_graining(raw_moran, subsize)
    } else { 
      indicf <- raw_moran
    }
    
    return( compute_indicator_with_null(input, nreplicates, indicf) )
    
    
  }
}

