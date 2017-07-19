# 
# This file tests whether the indicators are producing correct values
# 

data(forestgap)


testmat <- forestgap[[3]]



context('Test that all indicator functions return correct values')

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
                                  absolute = FALSE, 
                                  nreplicates = 0)[["value"]])
  
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
  expect_equal(raw_moran(random), 0, tolerance = 1e-2)
  
  # Moran's I should be one for perfectly segregated matrix
  split <- cbind(matrix(1, ncol = 500, nrow = 1000), 
                 matrix(0, ncol = 500, nrow = 1000))
  expect_equal(raw_moran(split), 1, tolerance = 1e-2)
  
  
  # Now test the indicator functions
  expect_equal(raw_moran(testmat), 
               indicator_moran(testmat, nreplicates = 0)[['value']])
  
})




test_that('Generic indicator task function returns correct values', { 
  
  # Parameters
  size <- 4
  moran_do_cg <- FALSE
  detrending <- FALSE
  moran_do_cg <- FALSE
  
  genindic_result <- generic_spews(testmat, 
                                   subsize = size, 
                                   moranI_coarse_grain = moran_do_cg)
  
  # Moran
  expect_equal(genindic_result[['results']][['moran']], 
               indicator_moran(testmat, 
                               subsize = 1,
                               nreplicates = 0)[['value']])
  
  # Skewness
  expect_equal(genindic_result[['results']][['skewness']],
               indicator_skewness(testmat, 
                                  subsize = size, 
                                  detrending = detrending,
                                  absolute = FALSE, 
                                  nreplicates = 0)[['value']])
  
  # Variances
  expect_equal(genindic_result[['results']][['variance']],
               indicator_variance(testmat, 
                                  subsize = size, 
                                  detrending = detrending,
                                  nreplicates = 0)[['value']])
  
})
