#' @title Patch Sizes Vector (psv)
#'
#' @param labelled_mat A labelled matrix as returned by `label`, or a list of 
#'                     labelled matrices.
#' @param ... Further arguments passed to `label`
#' 
#' @return A vector or a list of patch sizes vectors. 
#' 
#' @examples 
#' 
#' data(B)
#' psv_output <- psv(label(B)) 
#' 
#' # Observe rank distribution
#' plot(psv_output)
#' 
#' 
#' @export
psv <- function(labelled_mat) { 
  
  # Tabulate returns a vector x where x[i] = number of occurrences of integer i,
  # excluding zeros i.e. for each patch i, x[i] is the size of the patch.
  output <- sort( tabulate(labelled_mat) )
  
  if (length(output) <= 2) { 
    warning('Less than two patches found in the matrix. Are you sure ',
            'you passed a labelled matrix ?')
  }
  
  return(output)
}

#' Get cumulative patch sizes.
#'
#' @param labelled_mat A labelled matrix as returned by `label`
#' 
#' @param `...` further arguments passed to `psv`
#' 
#' @return A dataframe of the unique patch sizes \code{size}, the number of 
#'         patches of equal or larger size than each value in \code{size}, and 
#'         a probability that any given patch is equal or larger size than each 
#'         value in \code{size}. 
#'
#' @examples
#' 
#' data(B)
#' patchdistr <- psd(label(B))
#' 
#' # Plot distribution
#' plot(p ~ size, data=patchdistr, 
#'      log='xy', xlab='Patch size', ylab='N')
#' 
#' @export
psd <- function(labelled_mat) {
  
  patchvec <- psv(labelled_mat)
  
  out   <- data.frame(size = sort(unique(patchvec)))
  out$n <- sapply(seq.int(nrow(out)), function(i) sum(patchvec >= out$size[i]) ) 
  out$p <- out$n / length(patchvec)
  
  return(out)
}
