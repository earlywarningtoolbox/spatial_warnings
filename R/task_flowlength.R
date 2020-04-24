# A function that computes the "simple" flowlength as described in Rodriguez 
#   et al. 
# 
#' @title Flowlength connectivity indicator (uniform topography)
#' 
#' @description Measures the connectivity of runoff-source areas as determined 
#'   by vegetation patterns and (uniform) topography
#' 
#' @details 
#'   
#' This function computes Flowlength, a simple metric that measures the 
#'   potential hydrological connectivity of runoff-source areas 
#'   (e.g., bare soil) considering vegetation cover, vegetation patterns and 
#'   topography. Flowlength is defined as the average length of all the 
#'   potential runoff pathways in the target area. Thus, a higher value of 
#'   the index indicates a higher hydrologic connectivity of runoff source 
#'   areas. This function is designed for an idealized uniform hillslope 
#'   (e.g., with constant slope angle, the direction of maximum slope being 
#'   from the top to the bottom of the input matrices). 
#' 
#' The deviations of Flowlength from its expected values under random or 
#'   aggregated-pattern null models can be used as an indicator of imminent 
#'   transition to a degraded state (Rodriguez et al. 2017) in the context 
#'   of arid drylands. An increased deviation of flowlength compared to its 
#'   null values is expected as a possible transition gets closer. 
#' 
#' In general, Flowlength can be used as indicator of dryland functional status 
#'   by assessing potential water and soil losses in patchy landscapes 
#'   (Mayor et al. 2008, Moreno-de las Heras et al. 2012, Mayor et al. 2013, 
#'   Okin et al. 2015). Finally, the combination of observed and expected 
#'   Flowlength under null models for random or aggregated vegetation cover 
#'   can be used for assessing the cover-independent role of bare-
#'   soil connectivity (Rodriguez et al. 2018).
#'   
#' @references
#' 
#' Rodriguez, F., A. G. Mayor, M. Rietkerk, and S. Bautista. 2017. A null model 
#'   for assessing the cover-independent role of bare soil connectivity as 
#'   indicator of dryland functioning and dynamics. Ecological Indicators.
#' 
#' Mayor, A.G., Bautista, S., Small, E.E., Dixon, M., Bellot, J., 2008. 
#'   Measurement of the connectivity of runoff source areas as determined by 
#'   vegetation pattern and topography: a tool for assessing potential water 
#'   and soil losses in drylands. Water Resour. Res. 44, W10423.
#' 
#' Mayor, A.G., Kefi, S., Bautista, S., Rodriguez, F., Carteni, F., Rietkerk, 
#'   M., 2013. Feedbacks between vegetation pattern and resource loss 
#'   dramatically decrease ecosystem resilience and restoration potential in 
#'   a simple dryland model. Landsc. Ecol. 28, 931-942.
#' 
#' Moreno-de las Heras, M., Saco, P.M., Willgoose, G.R., Tongway, D.J., 2012. 
#'   Variations in hydrological connectivity of Australian semiarid landscapes 
#'   indicate abrupt changes in rainfall-use efficiency of vegetation. 
#'   J. Geophys. Res. 117, G03009.
#' 
#' Okin, G.S., Moreno-de las Heras, M., Saco, P.M., Throop, H.L., Vivoni, E.R., 
#'   Parsons, A.J., Wainwright, J., Peters, D.P.C., 2015. Connectivity in 
#'   dryland landscapes: shifting concepts of spatial interactions. 
#'   Front. Ecol. Environ. 13 (1), 20-27.
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
#' @examples 
#' 
#' \dontrun{ 
#' fl_result <- flowlength_sews(arizona, slope = 20, cell_size = 1)
#' 
#' # Compute the Z-score (standardized deviation to null distribution) and plot 
#' #   its variations along the gradient. This Z-score is suggested by 
#' #   Rodriguez et al. (2017) as an indicator of degradation. 
#' fl_test <- indictest(fl_result, nulln = 19)
#' plot(fl_test, what = "z_score")
#' }
#' 
#'@export
flowlength_sews <- function(mat,        # Input matrix
                            slope = 20, # Slope (in degrees)
                            cell_size = 1) { # Cell size
  
  compute_indicator(mat, raw_flowlength_uniform, 
                    slope = slope, 
                    cell_size = cell_size, 
                    taskname = paste0("Flow length (uniform topography)"))
}

#
# A function that computes the "simple" flowlength as described in Rodriguez 
#   et al. 
# 
#' @title Flow length (uniform slope)
#' 
#' @description Compute a simple approximation of the flow length assuming a 
#'   constant slope 
#' 
#' @details 
#'   
#'   This function computes the Flowlength of a given matrix, using a 
#'     uniform approximation (the slope is constant across the whole matrix, 
#'     with maximum slope being from the top of the matrix to its bottom), 
#'     as per Rodriguez et al. (2017). See \code{\link{flowlength_sews}} for 
#'     more details. 
#'   
#' @references
#' 
#' Rodriguez, F., A. G. Mayor, M. Rietkerk, and S. Bautista. 2017. A null model 
#'   for assessing the cover-independent role of bare soil connectivity as 
#'   indicator of dryland functioning and dynamics. Ecological Indicators.
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
#' \dontrun{ 
#' raw_flowlength_uniform(arizona[[1]], slope = 20, cell_size = 1)
#' }
#' 
#' 
#'@export
raw_flowlength_uniform <- function(mat,        # Input matrix
                                   slope, # Slope (in degrees)
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
  
  return(c(fl_uniform = fl))
}
