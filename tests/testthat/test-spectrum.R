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