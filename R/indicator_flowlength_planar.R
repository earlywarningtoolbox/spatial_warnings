# 
# A function that computes the "simple" flowlength as described in Rodriguez 
#   et al. 
# 
#'@export
raw_flowlength_planar <- function(mat,             # Input matrix
                                  slope = .5,      # Slope (in degrees)
                                  cell_size = 1) { # Cell size
  
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
  fl_max <- (nx*(nx+1)/2) * p_slope / nx
  
  # We compute now the flow length. Iterate over rows of mat
  flcol <- rep(0, ny)
  for ( r in seq(1, nx-1) ) { 
    flcol <- flcol + sum(cumprod(nmat[seq(r, nx), ]))
  }
  flcol <- flcol + nmat[nx, ]
  
  # Average flow length for each column
  flsum <- sum(flcol/nx)/ny
  fl <- flsum * p_slope
  
  return(fl)
}

