# 
# A function that computes the "simple" flowlength as described in Rodriguez 
#   et al. 
# 
#'@export
raw_flowlength_planar <- function(mat,        # Input matrix
                                  slope,      # Slope (in degrees)
                                  cell_size) { # Cell size
  
  if ( is.vector(mat) ) { 
    mat <- matrix(mat, ncol = 1, nrow = length(mat))
  }
  
  # Mat must be a logical matrix 
  if ( ! is.logical(mat) ) { 
    stop('The supplied matrix must be logical to compute the flow length')
  }
  
  # Get the cover
  cover <- mean(mat)
  
  # Take the negative of mat, so that ones represent empty cells
  nmat <- ! mat 
  
  # Cell size along the slope 
  p_slope <- cell_size / cos(slope*pi/180)
  
  # Height/Width of the matrix
  nx <- nrow(mat)
  ny <- ncol(mat)
  
  # Maximum value of flow length
  # fl_max <- (nx*(nx+1)/2) * p_slope / nx
  # We compute now the flow length. 
  flcol <- rep(0, ny)
  # Iterate over rows of mat
  for ( r in seq(1, nx) ) { 
    flcol <- flcol + col_sumcumprod(nmat[seq(r, nx), , drop = FALSE])
  }
  
  # Average flow length for a column
  flmean <- sum(flcol/nx)/ny
  
  fl <- flmean * p_slope
  
  return(fl)
}

