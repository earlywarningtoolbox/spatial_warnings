# 
# 
# This file contains the main function to compute the Generic Spews
# 
# 


#' @title Generic spatial warning signals
#' 
#' @description Computation of spatial generic early warning signals (Moran's I,
#'   variance and skewness)
#' 
#' @param mat A matrix (quantitative data), a binary matrix (qualitative data), 
#'   or a list of those
#' 
#' @param subsize The subsize used for the coarse-graining phase (see Details)
#'   
#' @param detrend Should the values be detrended by removing the spatial mean
#'   of the matrix ?
#' 
#' @param moranI_coarse_grain Should the input matrix be coarse-grained before
#'   computing the Moran's I indicator value ?
#'   
#' @return An object of class \code{generic_spews_single} (actually a list) 
#'   if mat is a single matrix or an object of class \code{generic_spews_list} 
#'   if mat is list.
#' 
#' @details 
#' 
#' The Generic Early warning signal are based on the property of a 
#'   dynamical system to "slow down" when approaching a critical point, 
#'   that is take more time to return to equilibrium after a perturbation. This 
#'   is expected to be reflected in several spatial characteristics: the 
#'   variance, the spatial autocorrelation (at lag-1) and the skewness. This 
#'   function provides a convenient workflow to compute these indicators, 
#'   assess their significance and display the results. 
#' 
#' Before computing the actual indicators, the matrix can be "coarse-grained". 
#'   This process reduces the matrix by averaging the nearby cells using 
#'   a square window defined by the \code{subsize} parameter. This helps 
#'   removing artefactual trends in variance and skewness due to binary (1/0) 
#'   data but is completely optional when using continous data.
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
#' @seealso 
#'   \code{\link{indicator_moran}}, \code{\link{indicator_variance}} and 
#'   \code{\link{indicator_skewness}} for individual indicators. 
#'   \code{\link{indictest}} to assess the significance of indicator values.
#'
#' @examples
#' 
#' # An example using a list of matrices
#' data(forestdat)
#' gen_indic <- generic_spews(forestdat[['matrices']], subsize = 2)
#' 
#' # Display results
#' summary(gen_indic)
#' 
#' # Display trends along the varying model parameter
#' plot(gen_indic, along = forestdat[['parameters']][ ,'delta'])
#' 
#' @export
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


