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

# Test(s) of correlation function 
# 
# - at length zero, correlation should be 1 (except for very small matrices 
#     (like ~ 4x4) (due to approximation of variance).
# - for a matrix with one state only, correlation is zero at all lengths
