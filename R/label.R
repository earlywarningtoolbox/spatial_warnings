#' @title Labelling of unique patches and detection of percolation. 
#' 
#' @description Label each patch with a number in a binary matrix 
#' 
#' @param mat A binary matrix
#' 
#' @param nbmask a "neighboring mask": a matrix with odd dimensions describing
#'   which cells are to be considered as neighbors around a cell 
#'   (see examples).
#' 
#' @param wrap Whether to wrap around lattice boundaries (`TRUE`/`FALSE`), 
#'   effectively using periodic boundaries.
#'
#' @return A matrix containing ID numbers for each connected patch. Default 
#'   parameters assume 4-cell neighborhood and periodic boundaries. The 
#'   distribution of patch sizes is returned as the attribute "psd" and the 
#'   percolation status as "percolation" (whether a TRUE patch has a width 
#'   or height equal to the size of the matrix). 
#' 
#' @details The \code{label} function "labels" the patches of a binary 
#'   (TRUE/FALSE) matrix. It returns a matrix of similar height and width, 
#'   with integer values representing the ID of each unique patch (contiguous
#'    cells). Empty cells are labelled as \code{NA}.
#' 
#' @seealso \code{\link{patchsizes}}, \code{\link{patchdistr_sews}}
#' 
#' @examples 
#' 
#' data(forestgap)
#' 
#' rmat <- matrix(rnorm(100) > .1, ncol = 10)
#' display_matrix(label(rmat))
#' 
#' # With 8-way neighborhood mask and no wrapping around borders
#' nbmask8 <- matrix(c(1,1,1,
#'                     1,0,1,
#'                     1,1,1), ncol=3)
#' display_matrix(label(rmat, nbmask8, wrap = FALSE))
#' 
#' # On real data: 
#' display_matrix(label(forestgap[[5]], nbmask8, wrap = FALSE))
#' 
#' @export
label <- function(mat, 
                  nbmask = matrix(c(0,1,0,
                                    1,0,1,
                                    0,1,0), ncol=3), # 4way NB 
                  wrap = FALSE) {
  
  if ( ! is.logical(mat) ) { 
    stop('Labelling of patches requirese a logical matrix',
         '(TRUE/FALSE values): please convert your data first.')
  }
  
  if ( ! is.matrix(mat) ) { 
    stop('The input object must be a matrix')
  }
  
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
  
  # The matrix is a row/column vector 
  if ( ncol(mat) == 1 || nrow(mat) == 1 ) { 
    vec <- as.vector(mat)
    result <- cumsum( c(vec[1] > 0, diff(vec)) == 1 ) * vec
    result <- ifelse(result > 0, result, NA)
    # If we wrap, then we need to merge the two patches at the end of the vector
    if ( wrap && !is.na(head(result, 1)) && !is.na(tail(result, 1)) ) { 
      result[ result == tail(result, 1) ] <- head(result, 1)
    }
    
    # PSD is the just the number of times each unique values appears in the 
    # result vector. 
    attr(result, "psd") <- tabulate(result)
    # Adjust dimensions
    dim(result) <- dim(mat)
    
    # If there is a patch, then it necessarily has the width or height 
    # of the matrix, so percolation is present. 
    attr(result, "percolation") <- any(mat)
    return(result)
  }
  
  # Otherwise we scan for patches
  label_cpp(mat, nbmask, wrap)
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
#' @description Get the distribution of patch sizes from a logical matrix
#' 
#' @param mat A logical matrix or a list of such matrices.
#' 
#' @param merge Controls whether the obtained patch size distributions are to 
#'   be pooled together if \code{mat} is a list of matrices. 
#' 
#' @param nbmask a square matrix with an odd number of lines and columns that 
#'   describes which neighbors are to be considered around a cell. Default 
#'   is 4-way neighborhood (the neighborhood of a cell comprises the cell 
#'   above, below, on the right and on the left of the target cell). 
#' 
#' @param wrap Whether to wrap around lattice boundaries (`TRUE`/`FALSE`), 
#'   effectively using periodic boundaries.
#' 
#' @return If mat is a logical matrix, then the function returns a vector of 
#'   patch sizes. If mat is a list of logical matrices, then it returns 
#'   a list of vectors of patch sizes: this list is flattened if merge is TRUE.
#' 
#' @seealso \code{\link{label}}
#' 
#' @examples
#' 
#' data(forestgap)
#' patchsizes(forestgap[[5]]) # Use a single matrix
#' 
#' # Compute the average patch size of each matrix
#' list_patches <- patchsizes(forestgap) # get the patch size for each matrix
#' print( sapply(list_patches, mean)) # print the average patch size 
#' 
#' # Example with 8-way neighborhood
#' nbmask8 <- matrix(c(1,1,1,
#'                     1,0,1,
#'                     1,1,1), ncol = 3)
#' patchsizes(forestgap[[5]], nbmask = nbmask8)
#' 
#'
#' @export
patchsizes <- function(mat, 
                       merge = FALSE,
                       nbmask = matrix(c(0,1,0,
                                         1,0,1,
                                         0,1,0), ncol = 3), # 4way neighborhood
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
    stop('Computing patch-size distributions requires a logical matrix',
         '(TRUE/FALSE values): please convert your data first.')
  }
  
  # We use the label function -> it returns patch sizes as attributes
  map <- label(mat, nbmask, wrap)
  
  return(attr(map, "psd"))
}
