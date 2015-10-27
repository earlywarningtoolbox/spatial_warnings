
context('Test that all indicator functions perform according to specification')

test_that('indicator functions stop if provided garbage data', { 

  indicator_functions <- list(indicator_moran,
                              indicator_cumpsd,
                              indicator_fitpsd,
                              indicator_fracgeo,
                              indicator_largestpatch,
                              indicator_powerspectrum,
                              indicator_skewness,
  #                             indicator_corrfunc, # Disabled to have the package build
                              indicator_variance) # add others here
  
  garbage_badclass  <- logical(10)
  garbage_has_nas   <- { a <- diag(10); a[5] <- NA; a }
  garbage_notbinary <- matrix(sample(c(1,2,3), 100, replace=TRUE), nrow=10)
  garbage_diffsizes <- lapply(5:10, diag)
  
  data(B)
  data(L)

  # This reflects checks in check_mat
  for (f in indicator_functions) { 
    expect_error(f(garbage_badclass))
    expect_error(f(garbage_has_nas))
    expect_error(f(garbage_notbinary))
    expect_warning(f(garbage_diffsizes))
    # Test if the functions actually work
    f(B)
    f(L)
  }
  
})
