#' @title Labelling of unique patches and detection of percolation. 
#' 
#' @description Label each patch with a number in a TRUE/FALSE matrix using 
#'   flood fill algorithm
#' 
#' @param mat A binary matrix
#' 
#' @param nbmask a "neighboring mask": a matrix with odd dimensions describing
#'   which neighbors are to be considered as neighbors around a cell 
#'   (see examples).
#' 
#' @param wrap Whether to wrap around lattice boundaries (`TRUE`/`FALSE`), 
#'   effectively using periodic boundaries.
#'
#' @return A matrix containing ID numbers for each connected patch. Default 
#'   parameters assume 4-cell neighborhood and periodic boundaries.
#' 
#' @details The \code{label} function "labels" the patches of a binary (1/0) 
#'   matrix. It returns a matrix of similar height and width, with integer 
#'   values representing the ID of the patch. Empty cells are labeled \code{NA}.
#' 
#' @examples 
#' 
#' data(forestdat)
#' attach(forestdat)
#' 
#' par(mfrow=c(1, 2))
#' image(matrices[[1]])
#' image(label(matrices[[1]]))
#' 
#' # With 8-way neighborhood mask and no wrapping around borders
#' nbmask8 <- matrix(c(1,1,1,
#'                     1,0,1,
#'                     1,1,1), ncol=3)
#' image(label(matrices[[1]], nbmask8, wrap = FALSE))
#' 
#' @export
label <- function(mat, 
                  nbmask = matrix(c(0,1,0,
                                    1,0,1,
                                    0,1,0), ncol=3), # 4way NB 
                  wrap = FALSE) {
  
  # The matrix is full
  if ( all(mat) ) { 
    result <- matrix(1, nrow = nrow(mat), ncol = ncol(mat)) 
    attr(result, "psd") <- prod(dim(mat))
    attr(result, "percolation") <- TRUE
    return(result)
  } 
  
  # The matrix is empty
  if ( !any(mat) ) { 
    result <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat)) 
    attr(result, "psd") <- integer(0)
    attr(result, "percolation") <- FALSE
    return(result)
  }
  
  # Otherwise we scan for patches
  .label(mat, nbmask, wrap)
}

#' @rdname label
#'
#' @description \code{percolation()} detects whether percolation occurs in the
#'   matrix (i.e. a patch has a width or a height equal to the size of the 
#'   matrix)
#' 
#' @export
percolation <- function(mat, nbmask = matrix(c(0,1,0,
                                               1,0,1,
                                               0,1,0), ncol=3)) { 
  # We never wrap for percolation, by definition. 
  patches <- label(mat, nbmask, wrap = FALSE)
  return(attr(patches, "percolation"))
}


#' @title Get patch sizes.
#' 
#' @description Get the distribution of patch sizes
#' 
#' @param mat A logical matrix or a list of these matrices.
#' 
#' @return If mat is a logical matrix, then the function returns a vector of 
#'   patch sizes. If mat is a list of logical matrices, then it returns 
#'   a list of vectors of patch sizes: this list is flattened if merge is TRUE.
#' 
#' @examples
#' data(forestdat)
#' patchsizes(forestdat[['matrices']][[1]])
#'
#' @export
patchsizes <- function(mat, 
                       merge = FALSE,
                       nbmask = matrix(c(0,1,0,
                                         1,0,1,
                                         0,1,0), ncol=3), # 4way NB 
                       wrap = FALSE) { 
  
  if ( is.list(mat)) { 
    result <- lapply(mat, patchsizes) 
    if (merge) { 
      # This always works even if there is only one element
      result <- do.call(c, result)
      names(result) <- NULL
    }
    return(result)
  }
  
  if ( ! is.logical(mat) ) { 
    stop('Patch-size distributions require a logical matrix',
         '(TRUE/FALSE values): please convert your data first.')
  }
  
  # We use the label function -> it returns patch sizes as attributes
  map <- label(mat, nbmask, wrap)
  
  return(attr(map, "psd"))
}

# Get the higher tail cumulative distribution of a psd (P(x >= k))
cumpsd <- function(dat) { 
  x <- sort(unique(dat))
  N <- length(dat)
  y <- sapply(x, function(k) { sum(dat >= k) / N })
  return( data.frame(patchsize = x, y = y) )
}


