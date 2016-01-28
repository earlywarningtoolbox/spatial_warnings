#####MOran
indicator_moran <- function(input, 
                            subsize,  
                            nreplicates ) {
  
  #check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_moran, 
                   subsize, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the Moran\'s I index requires a square matrix')
    } 
    
    return( compute_indicator_with_null(input,subsize,
                                        nreplicates, raw_moran) ) 
    
    
  }
}

#####power spectrum
powerspectrum <- function(mat) {
  
  # Handles mat if it is a list
  #check_mat(mat)
  if ( is.list(mat) ) { 
    return( lapply(mat, powerspectrum) ) 
  }
  
  nr <- nrow(mat)
  nc <- ncol(mat)
  
  n0x <- floor(nc/2) + 1
  n0y <- floor(nr/2) + 1
  
  # Create distance and angle matrices
  f1 <- t(replicate(nr,seq(1,nc))) - n0x
  f2 <- replicate(nc,seq(1,nr)) - n0y
  DIST <- sqrt(f1^2 + f2^2)
  ANGLE <- atan2(-f2,f1)*180/pi
  
  # Calculate DFT
  mi <- 1
  ma <- min(c(n0x,n0y))
  DISTMASK <- DIST>=mi & DIST <= ma
  
  tmp <- fft(mat)
  class(tmp) <- "matrix"
  tmpshift <- myfftshift(tmp)
  tmpshift[n0x,n0y] <- 0
  aspectr2D <- abs(tmpshift)^2 / (n0x*n0y)^4
  
  sig2 <- sum(aspectr2D[DISTMASK]) #Normalisation
  aspectr2D <- aspectr2D/sig2 #Normalisation
  
  # Now calculate r-spectrum
  STEP <- 1
  ray <- seq(mi,ma,STEP)
  rspectr <- numeric(length(ray))
  for (i in 1:length(ray))
  {
    m <- DIST >= ray[i] - STEP/2 & DIST < ray[i] + STEP/2
    rspectr[i] <- mean(aspectr2D[m])
  }

  out <- data.frame(dist  = ray,  rspec = rspectr);
              
  return(out)
  
}


####SDR
indicator_sdr <- function(input, low_range=low_range, high_range=high_range, 
                          nreplicates = nreplicates) { 
  
  #check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_sdr, low_range, high_range, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      warning('The matrix is not square: indicator_sdr will only use a square ', 
              'subset centered around the middle point.')
    } 
    
    if ( any( max(high_range) > dim(input)/2 ) ) { 
      warning('Your maximum correlation distance is higher than half of the ',
              'matrix size')
    }
    
    
    return( 
      compute_indicatorSDR_with_null(input,  
                                  nreplicates = nreplicates, 
                                  indicator_function = indicator_sdr_core,
                                  low_range=low_range,high_range=high_range)
    )
    
  }
  
}

indicator_sdr_core <- function(mat, low_range=low_range, high_range=high_range) { 
  
  # Compute r-spectrum
  spectrum <- powerspectrum(mat)
  
  # Compute subsets
  low_subset  <- with(spectrum, dist <= max(low_range)  & 
                        dist >= min(low_range))
  high_subset <- with(spectrum, dist <= max(high_range) & 
                        dist >= min(high_range))
  
  # Return ratio of means
  return( with(spectrum, mean(rspec[low_subset]) / mean(rspec[high_subset])) )
}

###Indicator skewnesss
indicator_skewness <- function(input, 
                               subsize     = subsize, 
                               nreplicates = nreplicates) {
  
  #check_mat(input) # checks if binary and sensible
  
  if ( is.list(input) ) {
    # Returns a list of lists
    return( lapply(input, indicator_skewness, 
                   subsize, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the skewness indicator requires a square matrix')
    } 
    
    return( compute_indicator_with_null(input, subsize, 
                                        nreplicates, raw_skewness2) ) 
    
  }
}

raw_skewness <- function(mat) { 
                      if (var(as.vector(mat))==0) {return(0)}

                      else  {return (moments::skewness(as.vector(mat)))} 
                      }


####VARIANCE
indicator_variance <- function(input, 
                               subsize     = subsize, 
                               nreplicates = nreplicates) {
  
  #check_mat(input) # checks if binary and sensible
  
  if (is.list(input)) {
    # Returns a list of lists
    return( lapply(input, indicator_variance, 
                   subsize, nreplicates) )
  } else { 
    
    if (diff(dim(input)) != 0) { 
      stop('Computation of the variance indicator requires a square matrix')
    } 
    

    
    return( compute_indicator_with_null(input, subsize,
                                        nreplicates, raw_variance) ) 
    
  }
}

raw_variance <- function(mat) { var(as.vector(mat)) }
raw_mean<-function(mat){mean(as.vector(mat))}

raw_moran <- function(input)
{
  m <- mean(as.vector(input))
  n <- (nrow(input)-1)
  
  moranI <- 0
  for (i in 2:n){
    for (j in 2:n) {
      moranI <- moranI + (input[i,j]-m)*(input[i,j-1]+input[i,j+1]+input[i-1,j]+input[i+1,j]-4*m)	
    }
  }
  moranI <- moranI/(4*(n-2)*(n-2))
  return(moranI)
}

myfftshift <- function(X)
{
  nr=dim(X)[1]
  nc=dim(X)[2]
  shiftX = X
  if (nr != nc)
    print("Not a square matrix X")
  else
  {
    n=nc
    shift = floor(n/2)
    for (i in 1:n)
      for (j in 1:n)
      {
        a = (i+shift)%%n
        b = (j+shift)%%n
        if (a==0) a = n
        if (b==0) b = n
        shiftX[a,b] = X[i,j]
      }
  }
  return(shiftX)
}

raw_skewness2 <- function(input)
{
  m <- mean(as.vector(input))
  n <- nrow(input)
  
  skew <- 0
  for (i in 1:n){
    for (j in 1:n) {
      skew <- skew + (input[i,j]-m)^3  
    }
  }
  skew <- skew/(n*n)
                
  return(skew)
}