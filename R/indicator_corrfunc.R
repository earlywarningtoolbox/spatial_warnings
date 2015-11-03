#' @title Spatial Warning Indicators: Correlation Function
#' 
#' @description This function computes the correlation function of a binary 
#'              matrix. 
#'
#' @param mat A binary matrix or a list of binary matrices. 
#' 
#' @return A numeric vector of correlation at different distances (from 0 to 
#' matrix size/2 by increments of 1 unit) or a list of these vectors if input
#' is a list of binary matrices.
#'
#' @details TODO: add details (reference ? what is a correlation function ?)
#'
#' @export
indicator_corrfunc <- function(mat) {
  
  check_mat(mat) # checks if binary and sensible
  
  if (is.list(mat)) { 
    return( lapply(mat, indicator_corrfunc) )
  } 
  
  return( corrfunc(mat) )
  
}

# This function is for internal use only
corrfunc <- function(mat) {
  
  L <- dim(mat)[1] # system size
  c <- as.matrix(expand.grid(seq.int(L), seq.int(L)))
  d <- apply(c, 1, function(X) sqrt((X[1] - L/2)^2 + (X[2] - L/2)^2))
  
  fullmat <- cbind(c,d)
  fullmat <- fullmat[order(fullmat[ ,3]), ]
  dists   <- unique(fullmat[ ,3])

  CoRmatNT <- rep(0, L/2) # What if L is not divisible by 2 ? 
  
  meanf <- mean(mat)
  varf  <- var(as.vector(mat))
  
  for (i in seq(0, L/2) ){
    #indices = randi(L*L,500,1)
    indices <- seq.int(L^2)
    indexi  <- c[, 2]
    indexj  <- c[ ,1]
    
    if (i == 0) {
      submat <- matrix(fullmat[fullmat[ ,3] <= i, ], 
                       nrow = 1, 
                       ncol = length(fullmat[fullmat[ ,3] <= i, ]))
    } else {
      upt    <- which(fullmat[ ,3] <= i)
      dwt    <- which(fullmat[ ,3] >  i-1)
      submat <- fullmat[intersect(dwt, upt), ]
    }
    r <- nrow(submat)
    cordump <- numeric(length = length(indices))
    for (idx in seq.int(length(indexi))) {
      counter <- 0
      caltd   <- mat[indexi[idx],indexj[idx]] - meanf
      for (j in 1:r){
          rowid <- submat[j,1] +indexi[idx] - L/2
          colid <- submat[j,2] +indexj[idx] - L/2
          rowid <- rowid %% L 
          colid <- colid %% L 
        if (rowid == 0) {
          rowid=L
        }
        
        if (colid == 0){
          colid <- L
        }
        counter <- counter+(caltd*(mat[rowid,colid]-meanf))
      }
    cordump[idx] <- (counter/(r))/varf
    }
    CoRmatNT[i+1] <- mean(cordump)
  }
  
  return(CoRmatNT)
}
