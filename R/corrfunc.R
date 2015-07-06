
# Function calculates correlation function for 1 matrix. It is NOT THE MAIN FUNCTION to calculcate correlation function. It is a dependency of Indicator_CorrelationFunction.R
corrfunc <- function(mat) {
  
  L <- dim(mat)[1] # system size
  c <- as.matrix(expand.grid(seq.int(L), seq.int(L)))
  d <- apply(c, 1, function(X) sqrt((X[1] - L/2)^2 + (X[2] - L/2)^2))
  
  fullmat <- cbind(c,d)
  fullmat <- fullmat[order(fullmat[,3]),]
  dists   <- unique(fullmat[,3])


  CoRmatNT <- rep(0, L/2) # What if L is not divisible by 2 ? 
  amat <- mat
  
  meanf <- mean(amat)
  varf  <- var(as.vector(amat))
  
  for (i in seq(0,L/2) ){
    #indices = randi(L*L,500,1)
    indices <- seq.int(L^2)
    indexi  <- c[, 2]
    indexj  <- c[1, ]

    if (i == 0) {
      submat <- matrix(fullmat[fullmat[ ,3] <= i, ], 
                       nrow = 1, 
                       ncol = length(fullmat[fullmat[ ,3] <= i, ]))
    } else {
      upt    <- which(fullmat[,3] <= i)
      dwt    <- which(fullmat[,3] >  i-1)
      submat <- fullmat[intersect(dwt,upt), ]
    }
    r <- nrow(submat)
    cordump <- numeric(length=length(indices))
    for (idx in seq.int(length(indexi))){
      counter <- 0
      caltd   <- amat[indexi[idx],indexj[idx]] - meanf
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
        counter <- counter+(caltd*(amat[rowid,colid]-meanf))
      }
    cordump[idx] <- (counter/(r))/varf
    }
    CoRmatNT[i+1] <- mean(cordump)
  }
  
  return(CoRmatNT)
}