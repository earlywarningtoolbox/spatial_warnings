#' @title Get patch sizes.
#' 
#' @description Get the distribution of patch sizes
#' 
#' @param x A binary matrix or a list of binary matrices.
#' 
#' @return A vector of patch sizes or a list of vectors if the input was a list
#'   of binary matrices. If input contained no patches (all values to FALSE), 
#'   then NA is returned.
#' 
#' @examples
#' data(forestdat)
#' patchsizes(forestdat[['matrices']][[1]])
#'
#' @export
patchsizes <- function(x, merge = FALSE) { 
  
  if ( is.list(x)) { 
    result <- lapply(x, patchsizes) 
    if (merge) { 
      # This always works even if only one element
      result <- do.call(c, result)
      names(result) <- NULL
    }
    return(result)
  }
  
  if ( ! is.logical(x) ) { 
    stop('Patch-size distributions require a logical matrix',
         '(TRUE/FALSE values): please convert your data first.')
  }
  
  # If there is no patch at all -> return NA
  if ( ! any(x) ) { 
    return( numeric() )
  }
  
  # We use the label function -> it returns patch sizes as attributes
  map <- label(x) 
  
  return(attr(map, "psd"))
}

# Get the higher tail cumulative distribution of something (P(x >= k))
cumpsd <- function(dat) { 
  x <- sort(unique(dat))
  N <- length(dat)
  y <- sapply(x, function(k) { sum(dat >= k) / N })
  return( data.frame(patchsize = x, y = y) )
}

