 
context('Test that patch labelling handles corner cases')


test_that("Patch counting works and handles weird matrices", { 
    
    # Labelling 
    
    # Fail if not a logical matrix
    expect_error(label(diag(10)))
    
    # Fail if not a matrix
    expect_error(label(seq.int(10)))
    
    # Wrapping 
    a <- diag(5) > 0; a[5,1] <- TRUE
    t <- label(a, wrap = FALSE)
    expect_true((t[1,1] != t[5,1]) != t[5,5])
    t <- label(a, wrap = TRUE)
    expect_true((t[1,1] == t[5,1]) == t[5,5])
    
    # Neighboring mask 
    nbm <- label(diag(5) > 0, nbmask = matrix(c(1,1,1,1,0,1,1,1,1), 
                                              ncol = 3, nrow = 3))
    expect_true(unique(na.omit(as.vector(nbm))) == 1)
    
    # Return things
    expect_true( all(is.na(label(diag(10) == 2))) )
    expect_true( unique(as.vector(label(diag(10) < 2))) == 1 )
    
    # Patch counting 
    testlist <- list(diag(10) > 0, diag(10) > 0 )
    expect_true( all(patchsizes(testlist, merge = TRUE) == 1) )
    expect_error( patchsizes(diag(10)) )
    
    # column/row vector patch counting 
    ex <- matrix(seq.int(5) > 2, ncol = 1)
    expect_true(attr(label(ex), "psd") == 3)
    expect_true(attr(label(t(ex)), "psd") == 3)
    expect_true(all(dim(label(ex)) == dim(ex))) # Check that dims are equal
    
    # Non-square matrix counting (width > height)
    ex <- matrix(c(1, 0, 0, 1, 
                   0, 0, 0, 1, 
                   0, 0, 0, 1), byrow = TRUE, ncol = 4) > 0
    test <- label(ex, wrap = TRUE)
    expect_true(test[1,1] == test[2, 4])
    test <- label(ex, wrap = FALSE)
    expect_true(test[1,1] != test[2, 4])
    expect_true(test[1,4] == test[2, 4])
    
    # Non-square matrix counting (height > width)
    ex <- matrix(c(1, 0, 0, 
                   1, 0, 0, 
                   1, 0, 0, 
                   1, 0, 1), byrow = TRUE, ncol = 3) > 0
    test <- label(ex, wrap = TRUE)
    expect_true(test[1,1] == test[4, 3])
    test <- label(ex, wrap = FALSE)
    expect_true(test[1,1] != test[4, 3])
    expect_true(test[4,1] == test[3, 1])
    
    # Non-square matrix counting 
    ex <- matrix(c(1, 1, 1, 1, 
                   0, 0, 0, 1, 
                   0, 0, 0, 1), byrow = TRUE, ncol = 4) > 0
    test <- label(ex)
    expect_true(attr(test, "percolation"))
    
})
