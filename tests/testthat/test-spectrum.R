# 
# 
# Test the computation of spectra (r-spectrum so far)
# 

context("Test the correct computation of r-spectrum")

test_that("rspectrum works as indicator_powerspectrum", { 
  
  data(forestdat)
  testmat <- forestdat[["matrices"]][[1]]
  
  rspec <- indicator_powerspectrum(testmat)[["r_spectrum"]]
  rspec2 <- rspectrum(testmat)
  all.equal(rspec, rspec2)
  expect_equal(rspec, rspec2)
  
})

test_that("the cpp implementation of the spectrum computations is correct", { 
  
  # Redefine the old function
  rspectrum_old <- function(mat, step) { 

  nr <- nrow(mat)
  nc <- ncol(mat)
  
  n0x <- floor(nc/2) + 1
  n0y <- floor(nr/2) + 1
  
  # Create distance and angle matrices
  f1 <- t(replicate(nr,seq(1,nc))) - n0x
  f2 <- replicate(nc,seq(1,nr)) - n0y
  DIST <- sqrt(f1^2 + f2^2)
  
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
  print(sig2)
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

  out <- data.frame(dist  = ray, rspec = rspectr)
  
  return(out)
  }
  
  testmat <- forestdat[["matrices"]][[1]]
  
  expect_equal(rspectrum_old(testmat), rspectrum(testmat))
  
})
