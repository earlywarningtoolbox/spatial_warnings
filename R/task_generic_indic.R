# 
# 
# This file contains the workflow function for generic EWS
# 

#' @title Generic Spatial Early-Warning signals
#' 
#' @description Computation, significance assesment and display of spatial 
#'   generic early warning signals (Moran's I, variance and skewness)
#' 
#' @param mat A matrix (quantitative data), a binary matrix (TRUE/FALSE data), 
#'   or a list of those
#' 
#' @param subsize The subsize used for the coarse-graining phase (see Details)
#'   
#' @param abs_skewness Should the absolute skewness be used instead of its 
#'   raw values ? 
#' 
#' @param moranI_coarse_grain Should the input matrix be coarse-grained before
#'   computing the Moran's I indicator value ?
#'   
#' @return 
#' 
#' \code{generic_sews} returns an object of class \code{generic_sews_single}
#'   (actually a list) if mat is a single matrix or an object of class 
#'   \code{generic_sews_list} if mat is a list. 
#' 
#' \code{indictest} returns an object of class \code{generic_test} (actually 
#'   a data.frame). 
#' 
#' \code{plot} methods return ggplot objects, usually immediately displayed 
#'   when used interactively.
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
#'   a square window defined by the \code{subsize} parameter. This makes spatial  
#'   variance and skewness reflect actual spatial patterns when working with 
#'   binary (\code{TRUE}/\code{FALSE} data), but is optional when using 
#'   continous data. Keep in mind that it effectively reduces the size of 
#'   the matrix by approximately \code{subsize} on each dimension. 
#'   
#' The significance of generic early-warning signals can be estimated by 
#'   reshuffling the original matrix (function \code{indictest}). Indicators 
#'   are then recomputed on the shuffled matrices and the values obtained are 
#'   used as a null distribution. P-values are obtained based on the rank of 
#'   the observered value in the null distribution. A small P-value means 
#'   that the indicator is significantly above the null values, as expected 
#'   before a critical point. 
#'
#' The \code{plot} method can displays the results graphically. A text summary 
#'   can be obtained using the \code{summary} method. 
#' 
#' @references 
#' 
#'   Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., 
#'   Livina, V.N., et al. (2014). Early Warning Signals of Ecological 
#'   Transitions: Methods for Spatial Patterns. PLoS ONE, 9, e92097.
#'   
#'   Dakos, V., van Nes, E. H., Donangelo, R., Fort, H., & 
#'   Scheffer, M. (2010). Spatial correlation as leading indicator of 
#'   catastrophic shifts. Theoretical Ecology, 3(3), 163-174.
#'   
#'   Guttal, V., & Jayaprakash, C. (2008). Spatial variance and spatial 
#'   skewness: leading indicators of regime shifts in spatial ecological 
#'   systems. Theoretical Ecology, 2(1), 3-12. 
#'   
#' @seealso 
#'   \code{\link{indicator_moran}}, \code{\link{indicator_variance}} and 
#'   \code{\link{indicator_skewness}} for individual indicators. 
#'
#' @examples
#' 
#' data(serengeti)
#' gen_indic <- generic_sews(serengeti, subsize = 5, 
#'                            moranI_coarse_grain = TRUE)
#' 
#' # Display results
#' summary(gen_indic)
#' 
#' # Display trends along the varying model parameter
#' plot(gen_indic, along = serengeti.rain)
#' 
#' # Compute significance (long)
#' \dontrun{
#' gen_test <- indictest(gen_indic)
#' 
#' print(gen_test)
#' 
#' # Display the trend, now with a grey ribbon indicating the 5%-95% quantile
#' # range of the null distribution
#' plot(gen_test, along = serengeti.rain)
#' 
#' # Display the effect size compared to null distribution 
#' plot(gen_test, along = serengeti.rain, what = "z_score")
#' 
#' # Note that plot() method returns a ggplot object that can be modified
#' # for convenience
#' if ( require(ggplot2) ) { 
#'   plot(gen_test, along = serengeti.rain) + 
#'     geom_vline(xintercept = 733, color = "red", linetype = "dashed") +
#'     xlab('Annual rainfall') + 
#'     theme_minimal()
#' }
#' 
#' }
#' 
#' @export
generic_sews <- function(mat, 
                          subsize = 4,
                          abs_skewness = FALSE,
                          moranI_coarse_grain = FALSE) {
  
  check_mat(mat)
  
  orig_mat <- mat
  
  if ( is.list(mat) ) { 
    results <- lapply(mat, generic_sews, subsize, abs_skewness,
                      moranI_coarse_grain)
    names(results) <- names(mat) # import list names
    class(results) <- c('generic_sews_list', 'generic_sews', 
                        'sews_result_list', 'list')
    return(results)
  }
  
  # Warn if the matrix is continous but we will coarse grain anyway
  if ( is.numeric(mat) && subsize > 1 ) { 
    warning(paste('Input matrix has continous values but will be coarse-grained', 
                  'anyway. Set subsize=1 to disable coarse graining.'))
  }
  
  if ( is.logical(mat) && subsize == 1 ) { 
    warning(paste('Input matrix is binary but no coarse-graining will be',
                  'performed.'))
  }
  
  # Build the right indicator function (closure) that take into accounts the 
  #   above options. 
  indicf <- function(mat) { 
    
    mat_cg <- coarse_grain(mat, subsize)
    
    if ( sd(as.vector(mat_cg)) == 0 ) { 
      return( c(variance = var(as.vector(mat_cg)), 
                skewness = NA_real_, 
                moran    = NA_real_, 
                mean     = mean(mat)) )
    }
    
    if (moranI_coarse_grain) { 
      moran_value <- raw_moran(mat_cg) 
    } else { 
      moran_value <- raw_moran(mat)
    }
    
    skewness_value <- raw_skewness(mat_cg)
    if (abs_skewness) { 
      skewness_value <- abs(skewness_value)
    }
    
    c(variance = var(as.vector(mat_cg)),
      skewness = skewness_value,
      moran    = moran_value,
      mean     = mean(mat))
  }
  
  # Compute the indicators and store the parameters used
  results <- list(results = as.list(indicf(mat)), 
                  orig_data = orig_mat,
                  call = match.call(),
                  subsize = subsize, 
                  indicf  = indicf, 
                  abs_skewness = abs_skewness, 
                  moranI_coarse_grain = moranI_coarse_grain)
  
  class(results) <- c('generic_sews_single', 'generic_sews',
                      'sews_result_single', 'list')
  return(results)
}


