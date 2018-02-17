 
context('Test that patch labelling handles corner cases')


test_that("Patch counting works and handles weird matrices", { 
    
    # Labelling 
    
    # Fail if not a logical matrix
    expect_error(label(diag(10)))
    
    # Return things
    expect_true( all(is.na(label(diag(10) == 2))) )
    expect_true( unique(as.vector(label(diag(10) < 2))) == 1 )
    
    # Patch counting 
    testlist <- list(diag(10) > 0, diag(10) > 0 )
    expect_true( all(patchsizes(testlist, merge = TRUE) == 1) )
    expect_error( patchsizes(diag(10)) )
               
  })
