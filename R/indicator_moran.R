#' @title Moran's Index at lag of 1
#'
#' @description This functions computes the Moran's spatial correlation index 
#'   (with lag one). It also computes a null value obtained by randomizing 
#'   the matrix.
#'
#' @references Dakos, V., van Nes, E. H., Donangelo, R., Fort, H., & 
#' Scheffer, M. (2010). Spatial correlation as leading indicator of 
#' catastrophic shifts. Theoretical Ecology, 3(3), 163-174.
#' 
#' @param input An matrix or a list of matrix object. It should 
#'   be a square matrix 
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix.
#' 
#' @param detrending If TRUE data are detrended by removing the spatial mean. 
#'   (Default is FALSE)
#' 
#' @param do_coarse_graining If TRUE then the matrix is coarse_grained before 
#'   computing the indicator. 
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
                            subsize     = 5, 
                            detrending  = FALSE, 
                            do_coarse_graining = FALSE,
                            nreplicates = 499) {
  
  check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_moran, 
                   subsize, detrending, do_coarse_graining, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the Moran\'s I index requires a square matrix')
    } 
    
    if ( do_coarse_graining ) { 
      indicf <- with_coarse_graining(raw_moran, subsize)
    } else { 
      indicf <- raw_moran
    }
    
    return( compute_indicator_with_null(input, detrending, 
                                        nreplicates, indicf) ) 
    
    
  }
}

