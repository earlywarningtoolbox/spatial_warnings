#' Get cumulative patch sizes.
#'
#' @param mat (optional) A binary matrix or a list of binary matrices.
#'
#' @param lblmat A labelled matrix as returned by `label`
#' 
#' @return A dataframe of the unique patch sizes \code{size}, the number of 
#'         patches of equal or larger size than each value in \code{size}, and 
#'         a probability that any given patch is equal or larger size than each 
#'         value in \code{size}. 
#' 
#' @export
indicator_cumpsd <- function(mat = NULL, 
                             lblmat = label(mat) ) {
  
  if ( ! is.null(mat) ) {
    check_mat(mat) # sanity checks for the passed matrix
  } 
  if ( ! is.null(mat) && is.list(mat)) { 
    return( lapply(mat, indicator_cumpsd) )
  }
  
  return( psd(lblmat) )
}
