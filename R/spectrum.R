# 
# This file contains functions that compute r/theta spectra
# 


# Compute rspectrum
rspectrum <- function(mat, step) { 
  
  nr <- nrow(mat)
  nc <- ncol(mat)
  
  # Middle point of matrix
  n0x <- floor(nc/2) + 1
  n0y <- floor(nr/2) + 1
  
  # Compute distances to center
  DIST <- get_distances(nr, nc, n0x, n0y)
  
  # Compute DFT
  mi <- 1
  ma <- min(c(n0x, n0y))
  DISTMASK <- DIST >= mi & DIST <= ma
  tmp <- fft(mat)
  tmpshift <- .myfftshift_cpp(tmp)
  tmpshift[n0x, n0y] <- 0
  aspectr2D <- abs(tmpshift)^2 / (n0x*n0y)^4
  aspectr2D <- normalize(aspectr2D, DIST, n0x, n0y)
  
  
  # Now calculate r-spectrum
  STEP <- 1
  ray <- seq(mi, ma, by = STEP)
  rspectr <- get_rspectr(ray, STEP, DIST, aspectr2D)
  
  out <- data.frame(dist  = ray, rspec = rspectr)
  
  return(out)
}