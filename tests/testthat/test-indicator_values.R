# 
# This file tests whether the indicators are producing correct values
# 

data(forestdat)


testmat <- forestdat$matrices[[1]]




# Test variance indicator
test_that("Indicator variance returns correct values", { 
  
  varf <- function(mat) var(as.vector(mat))
  
  expect_equal(varf(testmat), 
               indicator_variance(testmat, subsize = 1, 
                                  nreplicates = 0)[["value"]])
  
  for (subsize in seq.int(10)) { 
    expect_equal(varf(coarse_grain(testmat, subsize = subsize)), 
                 indicator_variance(testmat, subsize = subsize, 
                                    nreplicates = 0)[["value"]])
  }
})




test_that("Indicator skewness returns correct values", { 
  
  skewf <- function(mat) moments::skewness(as.vector(mat))
  
  expect_equal(skewf(testmat), 
               indicator_skewness(testmat, subsize = 1, 
                                  nreplicates = 0, 
                                  absolute = FALSE)[["value"]])
  
  for (subsize in seq.int(10)) { 
    expect_equal(skewf(coarse_grain(testmat, subsize = subsize)), 
                 indicator_skewness(testmat, subsize = subsize, 
                                    nreplicates = 0,
                                    absolute = FALSE)[["value"]]) # adjust for test
  }
})




test_that('Indicator Moran returns a correct value', { 
  
  # Test first the raw function
  
  # Moran's I should be -1 for checkboard pattern
  checkerboard <- matrix(c(0,1), byrow = TRUE, nrow = 1000, ncol = 1001)
  expect_equal(raw_moran(checkerboard), -1, tolerance = 1e-2)
  
  # Moran's I should be zero for random matrix
  random <- matrix(rbinom(1e6, 1, .5), nrow = 1e3, ncol = 1e3)
  expect_equal(raw_moran(random), 0, tolerance = 1e-3)
  
  # Moran's I should be one for perfectly segregated matrix
  split <- cbind(matrix(1, ncol = 500, nrow = 1000), 
                 matrix(0, ncol = 500, nrow = 1000))
  expect_equal(raw_moran(split), 1, tolerance = 1e-2)
  
  
  # Now test the indicator functions
  expect_equal(raw_moran(testmat), 
               indicator_moran(testmat, do_coarse_graining = FALSE, 
                                  nreplicates = 0)[['value']])
  
})

# Test(s) of correlation function 
# 
# - at length zero, correlation should be 1 (except for very small matrices 
#     (like ~ 4x4) (due to approximation of variance).
# - for a matrix with one state only, correlation is zero at all lengths
