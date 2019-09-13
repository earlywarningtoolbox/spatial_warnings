
context('Test that all single indicator functions perform correctly')

test_that('indicator functions stop if provided garbage data', { 

  indicator_functions <- list(indicator_moran,
                              indicator_skewness,
                              indicator_variance, 
                              # Note: for sdr we explicitely specify the ranges
                              #   as otherwise a warning is produced
                              function(x) indicator_sdr(x, c(0, .2), c(.8, 1))) 
                              #indicator_sdr) # add others here
  
  garbage_badclass  <- logical(10)
  garbage_has_nas   <- { a <- diag(10); a[5] <- NA; a }
  garbage_notbinary <- matrix(sample(c(1,2,3), 100, replace=TRUE), nrow=10)
  garbage_diffsizes <- lapply(5:10, diag)
  
  garbage_list_diff_types <- list(garbage_notbinary,
                                    garbage_notbinary == 3)
  data(forestgap)

  # This reflects checks in check_mat
  for (f in indicator_functions) { 
#     print(f)
    expect_error(f(garbage_badclass))
    expect_error(f(garbage_has_nas))
    
    expect_warning(f(garbage_diffsizes))
    expect_warning(f(garbage_list_diff_types))
  }
  
})
