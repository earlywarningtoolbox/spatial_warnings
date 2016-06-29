# 
# 
# Functions providing the global workflow for generic early-warning-signals
# 
# 





# ----------------------------
# COMPUTATION 
# ----------------------------

#' @title Generic spatial warning signals
#' 
#' @description Computation of spatial generic early warning signals (Moran's I, variance and skewness)
#' 
#' @param mat A matrix (quantitative data), a binary matrix (qualitative data), 
#'   or a list of those
#' 
#' @param subsize The subsize for the coarse-graining in case the passed matrix
#'   is qualitative
#'   
#' @param detrend Whether to substract the mean or not to the input matrix 
#'   (detrend)
#' 
#' @param moranI_coarse_grain Should the matrix be coarse-grained before 
#'   computing the moran's I neighbour correlation ?
#'   
#' @return An object of class \code{generic_spews_single} if the passed object
#'   was a single matrix or an object of class \code{generic_spews_list} if 
#'   it was a list.
#' 
#' @details Before a critical point, a spatial dynamical system is expected to 
#'   show an increase in spatial correlation at lag-1 (measured by Moran's I), 
#'   in variance and in skewness. These functions provides a workflow to 
#'   compute and test those indicators (see 
#'   \code{\link{indicator_moran}}, \code{\link{indicator_variance}} and 
#'   \code{\link{indicator_skewness}} details about null models). 
#' 
#' @references 
#'   Dakos, V., van Nes, E. H., Donangelo, R., Fort, H., & 
#'   Scheffer, M. (2010). Spatial correlation as leading indicator of 
#'   catastrophic shifts. Theoretical Ecology, 3(3), 163-174.
#'   
#'   Guttal, V., & Jayaprakash, C. (2008). Spatial variance and spatial 
#'   skewness: leading indicators of regime shifts in spatial ecological 
#'   systems. Theoretical Ecology, 2(1), 3–12. 
#'
#' @seealso \code{\link{indicator_moran}}, \code{\link{indicator_variance}} and 
#'   \code{\link{indicator_skewness}}
#'
#'@export
generic_spews <- function(mat, 
                          subsize = 4,
                          detrend = FALSE,
                          moranI_coarse_grain = FALSE) {
  
  check_mat(mat)
  
  orig_mat <- mat
  
  if ( is.list(mat) ) { 
    results <- lapply(mat, generic_spews, subsize, detrend, 
                      moranI_coarse_grain)
    class(results) <- c('generic_spews_list', 'generic_spews', 
                        'spews_result', 'list')
    return(results)
  }
  
  if (detrend) { 
    mat <- mat - mean(mat)
  }
  
  # Build the right indicator function (closure) depending on whether or not 
  #   moran's I should be computed on coarse-grained matrices.
  if ( moranI_coarse_grain ) { 
    indicf <- function(mat) { 
      mat_cg <- coarse_grain(mat, subsize)
      c(variance = var(as.vector(mat_cg)),
        skewness = raw_skewness(mat_cg),
        moran    = raw_moran(mat_cg), # CG ! 
        mean     = mean(mat))
    }
  } else { 
    indicf <- function(mat) { 
      mat_cg <- coarse_grain(mat, subsize)
      c(variance = var(as.vector(mat_cg)),
        skewness = raw_skewness(mat_cg),
        moran    = raw_moran(mat), # not CG ! 
        mean     = mean(mat))
    }
  }
  
  # Compute the indicators and store the parameters used
  results <- list(results = as.list(indicf(mat)), 
                  orig_data = orig_mat,
                  call = match.call(),
                  subsize = subsize, 
                  indicf  = indicf,
                  detrend = detrend)
  
  class(results) <- c('generic_spews_single', 'generic_spews',
                      'spews_result', 'list')
  return(results)
}


