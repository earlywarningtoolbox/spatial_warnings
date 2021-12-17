# 
# 
# 
context("Test that skewness computation is correct") 

test_that("Skewness computation is OK", { 
  if ( requireNamespace("moments", quietly = TRUE) ) { 
    X <- runif(1000) 
    expect_equal(moments::skewness(X), 
                 cpp_skewness(X))
  }
})

