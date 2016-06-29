#' @title Skewness indicator
#'
#' @description This functions computes the skewness critical point indicator. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
#'   
#' @references Guttal, V., and Jayaprakash, C. (2009). Spatial variance and 
#' spatial skewness: leading indicators of regime shifts in spatial 
#' ecological systems. Theoretical Ecology, 2(1), 3-12.
#' 
#' @param input A square binary matrix or a list of square binary matrices. 
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix.
#' 
#' @param detrending If TRUE data are detrended by removing the spatial mean. 
#'   (Default is FALSE).
#' 
#' @param absolute Should the function return the absolute value or raw value 
#'   of skewness ?
#' 
#' @param nreplicates Number of replicates to produce to estimate null 
#'   distribution of index.
#' 
#' @return A list (or a list of list if input was a list of matrices) with 
#'   components:
#'     \itemize{
#'       \item `mean`: Landscape mean cover
#'       \item `value`: Spatial skewness of the matrix
#'     }
#'   If nreplicates was above 2, then the list has the following additional 
#'   components : 
#'     \itemize{
#'       \item `null_mean`: Mean skewness of the null distribution
#'       \item `null_sd`: SD of skewness in the null distribution
#'       \item `z_score`: Z-score of the observed value in the null distribution
#'       \item `pval`: p-value based on the rank of the observed skewness
#'                       in the null distribution.
#'     }
#' 
#' @examples 
#' 
#' data(B)
#' indicator_skewness(B)
#' 
#' 
#'@export
indicator_skewness <- function(input, 
                               subsize     = 5, 
                               detrending  = FALSE, 
                               absolute = TRUE,
                               nreplicates = 999) {
  
  check_mat(input) # checks if binary and sensible
  # Check whether the matrix looks binary but was not declared as such
  check_binary_status(input) 
  
  if ( is.list(input) ) {
    # Returns a list of lists
    return( lapply(input, indicator_skewness, 
                   subsize, detrending, absolute, nreplicates) )
  } else { 
    
    # We choose the function depending on whether we want the absolute value 
    #   of skewness or its raw value.
    if ( absolute ) { 
      indic_tmp <- raw_abs_skewness
    } else { 
      indic_tmp <- raw_skewness
    }
    
    # We alter the chosen function above depending on whether we want 
    #   coarse_graining or not.
    if ( subsize > 1 ) { 
      indicf <- with_coarse_graining(indic_tmp, subsize)
    } else { 
      indicf <- indic_tmp
    }
    
    # Compute and return the indicator
    return( compute_indicator_with_null(input, detrending, 
                                        nreplicates, indicf) ) 
    
  }
}

raw_skewness <- function(mat) { 
  if ( length(mat) < 2 || sd(mat) == 0 ) { 
    return(NA)
  } else { 
    return( moments::skewness(as.vector(mat)) )
  }
}

raw_abs_skewness <- function(mat) { 
  abs(raw_skewness(mat)) 
}


