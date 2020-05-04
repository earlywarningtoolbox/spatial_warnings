# 
# This file tests whether the indicators are producing correct values
# 

data(forestgap)


testmat <- forestgap[[3]]
subsizes <- c(2, 4, 7)


context('Test that all indicator functions return correct values')

if ( exists("EXTENDED_TESTS") ) { 
  
  # Test variance indicator
  test_that("Indicator variance returns correct values", { 
    skip_on_cran()

    varf <- function(mat) var(as.vector(mat))
    
    expect_true(abs(varf(testmat) - 
                    raw_cg_variance(testmat, subsize = 1)) < 0.001)
    
    for (subsize in subsizes) { 
      expect_true( abs(varf(coarse_grain(testmat, subsize = subsize)) - 
                       raw_cg_variance(testmat, subsize = subsize)) < 0.001 )
    }
  })




  test_that("Indicator skewness returns correct values", { 
    skip_on_cran()

    skewf <- function(mat) moments::skewness(as.vector(mat))
    
    expect_true(abs(skewf(testmat) - 
                      raw_cg_skewness(testmat, subsize = 1, 
                                      absolute = FALSE)) < 0.001 )
    
    for (subsize in subsizes) { 
      expect_true( abs(skewf(coarse_grain(testmat, subsize = subsize)) - 
                       raw_cg_skewness(testmat, subsize = subsize, 
                                       absolute = FALSE)) < 0.001 ) # adjust for test
    }
  })




  test_that('Indicator Moran returns correct values', { 
    skip_on_cran()

    # Test first the raw function
    
    # Moran's I should be -1 for checkboard pattern
    checkerboard <- matrix(c(0,1), byrow = TRUE, nrow = 1000, ncol = 1001)
    expect_true( abs(raw_cg_moran(checkerboard) - (-1)) < 0.01 )
    
    # Moran's I should be zero for random matrix
    random <- matrix(rbinom(1e6, 1, .5), nrow = 1e3, ncol = 1e3)
    expect_true(abs(raw_cg_moran(random) - 0) < 1e-2)
    
    # Moran's I should be one for perfectly segregated matrix
    split <- cbind(matrix(1, ncol = 500, nrow = 1000), 
                   matrix(0, ncol = 500, nrow = 1000))
    expect_true(abs(raw_cg_moran(split) - 1) < 1e-2)
    
  })
  
  
  
  test_that('Indicator psdtype returns correct values', { 
    skip_on_cran()

    # Test one fit on the whole dataset
    expect_true(with((forestgap, merge = TRUE), 
                     as.character(type[best])) == "tpl") 
    
    # Test individual fits
    if ( exists("EXTENDED_TESTS") ) { 
      a <- sapply((forestgap, best_by = 'BIC'), 
                  with, as.character(type[best]))
      expect_true(all(c(is.na(a[1]), 
                        a[-1] == c("pl", "pl", "pl", "tpl", "tpl", "tpl", 
                                  "tpl", "exp"))))
    }
  })
  
  
  
  test_that('Indicator plrange returns correct values', { 
    skip_on_cran()
    
    # Test that workflow function and individual function return the same thing
    indiv_ic_plrange <- indicator_plrange(forestgap, merge = TRUE)$plrange
    workflow_plrange <- patchdistr_sews(forestgap, merge = TRUE)$plrange$plrange
    
    expect_true( abs(indiv_ic_plrange - 0.8153096) < 0.001 )
    expect_true( abs(workflow_plrange - 0.8153096) < 0.001 )
    expect_true( abs(workflow_plrange - indiv_ic_plrange) < 0.001 )
    
  })




  test_that('Generic indicator task function returns correct values', { 
    skip_on_cran()
    
    # Parameters
    size <- 4
    moran_do_cg <- FALSE
    
    genindic_result <- generic_sews(testmat, 
                                    subsize = size, 
                                    moranI_coarse_grain = moran_do_cg)
    
    # Moran
    expect_true( abs(as.data.frame(genindic_result, wide = TRUE)$moran - 
                     raw_cg_moran(testmat, subsize = 1)) < 0.001 )
    
    # Skewness
    expect_true( abs(as.data.frame(genindic_result, wide = TRUE)$skewness - 
                     raw_cg_skewness(testmat, subsize = size, 
                                     absolute = FALSE)) < 0.001 ) 
    
    # Variances
    expect_true( abs(as.data.frame(genindic_result, wide = TRUE)$variance - 
                     raw_cg_variance(testmat, subsize = size)) < 0.001 )
    
  })
}
