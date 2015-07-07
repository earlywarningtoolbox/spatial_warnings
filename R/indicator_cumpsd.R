#' Get cumulative patch sizes.
#'
#' @param x A vector of patch sizes or a list of vectors.
#'
#' @return A dataframe of the unique patch sizes \code{size}, the number of patches of equal or larger size than each value in \code{size}, and a probability that any given patch is equal or larger size than each value in \code{size}. 
#' @export
#' 

indicator_cumpsd <- function(x = NULL, patchvec = patchsizes(x) ) {
  check_mat(x) # sanity checks for the passed matrix (no error if NULL)
  if ( ! is.null(x) && is.list(x)) { # FALSE for x = NULL
    return( lapply(x, indicator_cumpsd) )
  } else {
    out <- data.frame(size =  unique(patchvec))
      out$n = sapply(1:length(out$size), function(i)  length(which(patchvec >= out$size[i])) ) 
      out$p = out$n/length(patchvec)
      
      return(out)
  }
}

