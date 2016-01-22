#' @title Labelling of unique patches.
#' 
#' @param mat A binary matrix
#' 
#' @param nbmask a "neighboring mask": a matrix with odd dimensions describing
#'        which neighbors are to be considered around a cell (see examples).
#' 
#' @param wrap Whether to wrap around lattice boundaries (`TRUE`/`FALSE`), 
#'   effectively using periodic boundaries.
#'
#' @return A matrix containing ID numbers for each connected patch. Default 
#'   parameters assume 4-cell neighborhood and periodic boundaries.
#' 
#' @details This function "labels" the patches of a binary (1/0) matrix. It 
#'   returns the same matrix, with an integer representing the ID of the 
#'   patch instead of 1/0.
#' 
#' @examples 
#' 
#' data(B)
#' par(mfrow=c(1, 2))
#' image(B)
#' image(label(B))
#' 
#' # With 8-way neighborhood mask and no wrapping around borders
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
