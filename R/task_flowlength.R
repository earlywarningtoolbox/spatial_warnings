# A function that computes the "simple" flowlength as described in Rodriguez 
#   et al. 
# 
#' @title Flow length indicator
#' 
#' @description Computes the flow length of a given matrix
#' 
#' @details 
#'   
#'   This function computes the flow length of a given matrix, using a 
#'     planar approximation (the slope is constant across the whole matrix), 
#'     as per Rodriguez et al. (2017). 
#'   
#'   <Describe succintly (~100 wds) how this can be used as an indicator>
#'   
#' @references
#' 
#' Rodríguez, F., Á. G. Mayor, M. Rietkerk, and S. Bautista. 2017. A null model 
#'   for assessing the cover-independent role of bare soil connectivity as 
#'   indicator of dryland functioning and dynamics. Ecological Indicators.
#' 
#' <Mayor et al. 2013 would probably be a good reference here too>
#' 
#' @param mat The input matrix (must be a logical matrix)
#' 
#' @param slope The slope of the area documented by the matrix (in degree). 
#' 
#' @param cell_size The horizontal size of a cell in the matrix (as viewed 
#'   from above). 
#' 
#' @param method The method to use to compute the flow length (for now only 
#'   the method "planar", using a single slope approximation, is implemented)
#' 
#' @return The flow length numerical value. 
#' 
#' @examples 
#' 
#' fl_result <- flowlength_sews(arizona, slope = 20, cell_size = 1)
#' 
#' \dontrun{ 
#' # Compute the Z-score (standardized deviation to null distribution) and plot 
#' #   its variations along the gradient. This Z-score is suggested by 
#' #   Rodriguez et al. (2017) as an indicator of degradation. 
#' fl_test <- indictest(fl_result, nperm = 19)
#' plot(fl_test, what = "z_score")
#' }
#' 
#'@export
flowlength_sews <- function(mat, 
                            slope = 20, 
                            cell_size = 1, 
                            method = "planar") { 
  
  # This is a formatted function to compute the kbdm
  flfun <- function(mat, slope, cell_size) { 
    result <- list(value     = raw_flowlength_planar(mat, slope, cell_size), 
                   orig_data = mat, 
                   fun.args  = list(slope, cell_size), 
                   indicf    = raw_flowlength_planar)
    
    class(result) <- c('flowlength_sews', 'simple_sews_single', 'list')
    attr(result, "indicname") <- paste0("Flow length (", method, ")")
    return(result)
  }
  
  if ( is.list(mat) ) { 
    result <- parallel::mclapply(mat, flfun, slope, cell_size)
    names(result) <- names(mat)
    class(result) <- c('flowlength_sews', 'simple_sews_list', 'list')
    attr(result, "indicname") <- paste0("Flow length (", method, ")")
  } else { 
    result <- flfun(mat, slope, cell_size)
  }
  
  return(result)
  
}

#
# A function that computes the "simple" flowlength as described in Rodriguez 
#   et al. 
# 
#' @title Flow length (planar approximation)
#' 
#' @description Compute a simple approximation of the flow length assuming a 
#'   constant slope 
#' 
#' @details 
#'   
#'   This function computes the flow length of a given matrix, using a 
#'     planar approximation (the slope is constant across the whole matrix), 
#'     as per Rodriguez et al. (2017). See \code{\link{flowlength_sews}} for 
#'     more details. 
#'   
#' @references
#' 
#' Rodríguez, F., Á. G. Mayor, M. Rietkerk, and S. Bautista. 2017. A null model 
#'   for assessing the cover-independent role of bare soil connectivity as 
#'   indicator of dryland functioning and dynamics. Ecological Indicators.
#' 
#' <Mayor et al. 2013 ?>
#' 
#' @param mat The input matrix (must be a logical matrix)
#' 
#' @param slope The slope of the area documented by the matrix (in degree). 
#' 
#' @param cell_size The horizontal size of a cell in the matrix (as viewed 
#'   from above). 
#' 
#' @return The flow length numerical value. 
#' 
#' @seealso \code{\link{flowlength_sews}}
#' 
#' @examples 
#' 
#' raw_flowlength_planar(arizona[[1]], slope = 20, cell_size = 1)
#' 
#' 
#' 
#' 
#'@export
raw_flowlength_planar <- function(mat,        # Input matrix
                                  slope,      # Slope (in degrees)
                                  cell_size) { # Cell size
  
  if ( is.vector(mat) ) { 
    mat <- matrix(mat, ncol = 1, nrow = length(mat))
  }
  
  # Mat must be a logical matrix 
  if ( ! is.matrix(mat) ) { 
    stop('The supplied object is not a matrix')
  }
  
  # Mat must be a logical matrix 
  if ( ! is.logical(mat) ) { 
    stop('The supplied matrix must be logical to compute the flow length')
  }
  
  # Take the negative of mat, so that ones represent empty cells
  nmat <- ! mat 
  
  # Cell size along the slope 
  p_slope <- cell_size / cos(slope*pi/180)
  
  # Compute flow length without taking slope into account (C++ function)
  flmean <- fl_internal(nmat)
  
  fl <- flmean * p_slope
  
  return(fl)
}

#FIXME: Does it make sense to provide default values ? 
#FIXME: Is "planar" a good name ? 
