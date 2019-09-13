# 
# Test whether coarse-graining works
# 

context('Test the coarse-graining function') 

testmat4x4 <- matrix(c(1, 1, 0, 0, 
                       1, 1, 0, 0,
                       0, 0, 1, 1,
                       0, 0, 1, 1), 
                       byrow = TRUE, ncol = 4)

checkerboard <- matrix(c(0,1), byrow = TRUE, nrow = 1000, ncol = 1001)


test_that("Coarse-graining works", { 
  
  expect_equal(coarse_grain(testmat4x4, 2), 
               matrix(c(1, 0, 
                        0, 1), ncol = 2, byrow = TRUE))
  
  expect_equal(unique(as.vector(coarse_grain(checkerboard, subsize = 2))), 
               0.5)
  
  expect_warning(coarse_grain(checkerboard, subsize = 0))
  
  for (size in c(1, 3, 10, 100, 1000)) { 
    for ( subsize in c(1, 2, 3, 4, 5) ) { 
      testdiag <- diag(size)
      ref <- diag(floor(size/subsize)) / subsize
      expect_equal(coarse_grain(testdiag, subsize), ref)
    }
  }
  
})

test_that("CG handles negative values", { 
  expect_true( all(is.numeric( coarse_grain(matrix(runif(100, -10, -5), 100, 100), 
                                        subsize = 4) )) )
})

