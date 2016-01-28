#' 
#' @title Spatial warning indicators: Power Spectrum
#'
#' @description Function to calculate theta and r spectrum.
#' 
#' @param mat A binary matrix or a list of binary matrices
#' 
#' @return A list of lists(of theta and R spectrum values for distances 1 to 
#'   SystemSize/2 for each matrix in the list) if input is a list (OR) a single 
#'   list (of theta and R spectrum values for distances 1 to SystemSize/2) if 
#'   input is a single matrix
#' 
#' @details writeme
#' 
#' @export
indicator_powerspectrum <- function(mat) {
  
  # Handles mat if it is a list
  check_mat(mat)
  if ( is.list(mat) ) { 
    return( lapply(mat, indicator_powerspectrum) ) 
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
  tmpshift <- .myfftshift_cpp(tmp)
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
  
  # Now calculate theta-spectrum
  DISTMASK <- DIST>=mi & DIST<=ma
  STEP <- 5 #increments of 5 degrees
  anglebin <- seq(STEP,180,STEP)
  tspectr <- numeric(length(anglebin))
  for (i in 1:(length(tspectr)-1))
  {
    m <- which(DISTMASK & ANGLE>=anglebin[i]-STEP & ANGLE<anglebin[i])
    tspectr[i] <- sum(aspectr2D[m])/length(m)
  }
  
  m <- which(DISTMASK & 
             ANGLE >= anglebin[length(anglebin)] - STEP & 
             ANGLE <= anglebin[length(anglebin)])
  tspectr[length(tspectr)] <- sum(aspectr2D[m])/length(m)
  
  out <- list(r_spectrum = data.frame(dist  = ray,      rspec = rspectr),
              t_spectrum = data.frame(angle = anglebin, tspec = tspectr))
  
  return(out)
  
}
