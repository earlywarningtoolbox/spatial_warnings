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
                  wrap = TRUE) {
  
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
  
  patches <- label(mat, nbmask, wrap = FALSE)
  
  if ( all(is.na(patches)) ) { 
    return(NA) # logical
  } 
  
  unique_patches <- seq.int(max(patches, na.rm = TRUE))
  
  # We scan all patches and see whether they have height or width equal 
  #   to the size of the matrix
  for ( patch in unique_patches ) { 
    patch_cell_coords <- which(patches == patch, arr.ind = TRUE)
    pos.min <- apply(patch_cell_coords, 2, min)
    pos.max <- apply(patch_cell_coords, 2, max)
    if ( any( (pos.max - pos.min)+1 == dim(mat)) ) { 
      return(TRUE)
    }
  }
  
  # If we have not found any patch of such size, then there is no percolation 
  #   so we return
  return(FALSE)
}

