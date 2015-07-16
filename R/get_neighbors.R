#' 
#' @title Get the xy coords of neighboring cells
#' 
#' @param mat A 2D matrix
#' 
#' @param X   A vector of coordinates (x,y) of the target cell
#' 
#' @param nbmask A neighboring "mask" that describes the neighbors to take into
#'               into account around the target cell (see examples).
#'               
#' @param wrap Whether to wrap or not when a point falls outside boundaries
#' 
#' @return A two-columns matrix with the x,y coordinates of neighbors.
#' 
#' @examples
#' 
#' nbmat4 <- matrix(c(0, 1, 0, # neighbor mask
#'                    1, 0, 1,
#'                    0, 1, 0), nrow=3, byrow=TRUE)
#' size <- 100
#' letmat <- matrix(sample(c(0,1,2), size, replace=TRUE), nrow=sqrt(size))
#' 
#' get_nb_coords(letmat, c(3,1), nbmat4, wrap = TRUE)
#' 
#' # Contrast with
#' get_nb_coords(letmat, c(3,1), nbmat4, wrap = FALSE)
#' 
get_nb_coords <- function(mat, X, nbmask, wrap) {
  
  .check_get_nb_args(mat, X, nbmask, wrap)
  
  # NOTE: The +1/-1 are here to translate R-style indexing (starts at 1) from 
  # C/C++-style indexing (starts at 0)
  .get_nb_coords(mat, X-1, nbmask, wrap) + 1 
}

#' 
#' @title Get the values in neighboring cells
#' 
#' @param mat A 2D matrix
#' 
#' @param X  A vector of coordinates (x,y) of the target cell
#' 
#' @param nbmask A neighboring "mask" that describes the neighbors to take into
#'               into account around the target cell (see examples).
#'               
#' @param wrap Whether to wrap or not when a point falls outside boundaries
#' 
#' @return A vector containing the values of the cell's neighbors, in the same 
#'         order as the rows of the output of `get_nb_coords`.
#' 
#' @examples
#' 
#' nbmat4 <- matrix(c(0, 1, 0, # neighbor mask
#'                    1, 0, 1,
#'                    0, 1, 0), nrow=3, byrow=TRUE)
#' size <- 100
#' letmat <- matrix(sample(c(0,1,2), size, replace=TRUE), nrow=sqrt(size))
#' 
#' get_nb_values(letmat, c(3,1), nbmat4, wrap = TRUE)
#' 
#' # Contrast with
#' get_nb_coords(letmat, c(3,1), nbmat4, wrap = FALSE)
#' 
#'@export
get_nb_values <- function(mat, X, nbmask, wrap) {  
  
  .check_get_nb_args(mat, X, nbmask, wrap)
  
  .get_nb_values(mat, X, nbmask, wrap)
}

# Do sanity checks for the above functions
.check_get_nb_args <- function(mat, X, nbmask, wrap) { 
  
  if ( any( dim(nbmask) %% 2 == 0 ) ) { 
    stop('The neighbor mask must have odd dimensions')
  }
  
  if (length(X) > 2) { 
    warning('The coordinates provided has a length above 2: only the first ',
            'two values will be used.')
  }
  
  return(NULL)
}
