#' @title Skewness indicator
#'
#' @description This functions computes the skewness critical point indicator. 
#'   It also computes a null value obtained by randomizing 
#'   the matrix.
#' 
#' @param input A square binary matrix or a list of square binary matrices. 
#' 
#' @param discrete logical. If TRUE the data represent discrete variables (like 
#'   presence/absense), otherwise continuous data (like biomass density). 
#'   Defaults to FALSE.
#' 
#' @param subsize logical. Dimension of the submatrix used to coarse-grain the 
#'   original matrix.
#' 
#' @param detrending If TRUE data are detrended by removing the spatial mean. 
#'   (Default is FALSE).
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
#' data(B)
#' indicator_skewness(B)
#' 
#' 
#'@export
indicator_skewness <- function(input, 
                               subsize     = 2, 
                               detrending  = FALSE, 
                               discrete    = TRUE,
                               nreplicates = 499) {
  
  check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, skewness_withnull, subsize, detrending, discrete, 
                   nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the skewness indicator requires a square matrix')
    } 
    
    return( skewness_withnull(input, subsize, detrending, discrete, nreplicates) )
    
  }
}

# This function should not be used alone ----------------------------------
# This function computes the skewness indicator and computes a null model 
# if requested (random matrix). 
skewness_withnull <- function(input, 
                              subsize, 
                              detrending, 
                              discrete, 
                              nreplicates) {
  
  m <- mean(input) # Compute the mean value of the matrix 
  
  # Check options and apply modifications --------------------
  
  if (detrending) {
    input  <- input - m  # overwrites input locally
  }
  
  if (discrete) {
    # coarse-grain before computing the index
    indic_fun <- indic_skewness_with_cg  
  } else { 
    indic_fun <- indic_skewness
  }
  
  # Compute the indicator -----------------------------
  # Note: subsize is passed but it is ignored if indic_fun is set to call 
  #   indic_skewness.
  skew <- indic_fun(input, subsize)
  result <- list(mean = m, value = skew)
  
  if (nreplicates > 2) { 
    # Compute the index on a randomized matrix
    nulldistr <- replicate(nreplicates, 
                           indic_fun(matrix(sample(input), nrow = nrow(input)), 
                                     subsize) )
    result <- c(result,
                list(null_mean = mean(nulldistr), 
                     null_sd   = sd(nulldistr),
                     z_score   = (skew - mean(nulldistr)) / sd(nulldistr),
                     pval      = 1 - rank(c(skew, nulldistr))[1] / (nreplicates+1)))
  }
  
  return(result)
}

indic_skewness <- function(mat, ...) { 
  # Arguments beyond the first are accepted but ignored
  skewness(as.vector(mat))
}

indic_skewness_with_cg <- function(mat, subsize) { 
  indic_skewness(as.vector(coarse_grain(mat, subsize)))
}
