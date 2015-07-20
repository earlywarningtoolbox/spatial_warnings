#' Get cumulative patch sizes.
#'
#' @param x A vector of patch sizes or a list of vectors.
#'
#' @return A dataframe of the unique patch sizes \code{size}, the number of 
#'   patches of equal or larger size than each value in \code{size}, and a 
#'   probability that any given patch is equal or larger size than each value 
#'   in \code{size}. 
#' 
#' @examples
#' 
#' data(B)
#' indicator_cumpsd(B)
#' 
#' 
#' @export
#' 

indicator_cumpsd <- function(x = NULL, patchvec = patchsizes(x) ) {
  check_mat(x) # sanity checks for the passed matrix 

  if ( ! is.null(x) && is.list(x)) { 
    return( lapply(x, indicator_cumpsd) )
  } 
  
  out <- data.frame(size =  unique(patchvec))
  if(length(out$size) > 1) {
    out$n = sapply(seq.int(out$size), function(i) sum(patchvec >= out$size[i]) ) 
    out$p = out$n/length(patchvec)
  } else {
    out$n = 1 
    out$p = 1 
  }
  return(out)

}

