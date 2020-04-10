# 
# This function makes sure parallelism is enabled 
#

context("Test that parallel computing works")

test_that("Parallelism work", { 
  
  if ( requireNamespace("future", quietly = TRUE) && 
       availableCores() > 1 ) { 
    a <- generic_sews(forestgap)
    
    plan(sequential)
    b.1 <- system.time( indictest(a, 49) ) 
    plan(multiprocess)
    b.2 <- system.time( indictest(a, 49) ) 
    
    expect_true( b.1["elapsed"] > b.2["elapsed"] )
    
    if ( .Platform$OS.type == "unix" ) { 
      plan(multicore)
      b.3 <- system.time( indictest(a, 49) ) 
      expect_true( b.1["elapsed"] > b.3["elapsed"] )
    }
    
    plan(sequential) # restore plan to no-parallelism
  }
  
})

