#' @title Labelling of unique patches.
#' 
#' @param mat A binary matrix
#' 
#' @param nbmask a "neighboring mask": a matrix with odd dimensions describing
#'        which neighbors are to be considered around a cell (see examples).
#' 
#' @param wrap Whether to wrap around lattice boundaries (`TRUE`/`FALSE`).
#'
#' @return A matrix containing ID numbers for each connected patch. Assuming
#'   4-cell neighborhood and periodic boundaries.
#'   
#' 
#' @examples 
#' data(B)
#' image(label(B))
#' 
#' 
#' # With 8-way neighborhood mask
#' nbmask8 <- matrix(c(1,1,1,
#'                     1,0,1,
#'                     1,1,1), ncol=3)
#' image(label(B, nbmask8, wrap = FALSE))
#' 
#' @export
label <- function(mat, 
                  nbmask = matrix(c(0,1,0,
                                    1,0,1,
                                    0,1,0), ncol=3), # 4way NB 
                  wrap = TRUE) {
  check_mat(mat)
  
  .label(mat, nbmask, wrap)
}
