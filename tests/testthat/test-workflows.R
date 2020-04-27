# 
# 
# This file contains a unit tests that make sure the complete workflow is working
# 

context('Test workflows') 

data(forestgap)
data(serengeti)

datasets <- list(forestgap[[3]], 
                 serengeti[5:6])

test_methods <- function(teststring, datalength, obj, .test_df = TRUE) { 
  ok_print <- any(grepl(teststring, capture.output(print(obj))))
  expect_true(ok_print)
  
  ok_summary <- any(grepl(teststring, capture.output(summary(obj))))
  expect_true(ok_summary)
  
  
  if (.test_df) { 
    ok_as_df <- nrow(as.data.frame(obj)) == datalength
  } else { 
    ok_as_df <- is.data.frame(as.data.frame(obj)) 
  }
  expect_true(ok_as_df)
  
  return(TRUE)
}

test_that("The workflow functions work", { 
  skip_on_cran()
  
  for ( dataset in datasets ) { 
    
    # Length of data
    datal <- ifelse(is.matrix(dataset), 1, length(dataset))
    
    # Generic indicators
    indics <- generic_sews(dataset) 
    test_methods("Generic indicators", 
                 datal*4, indics) # l(dataset) * 4 indics
    # test_methods("Generic Spatial Early-Warnings", 4, indics[[1]])
    
    indics.test <- indictest(indics, nulln = 3)
    test_methods("Generic indicators", 
                  datal*4, indics.test)
    
    if ( datal > 1 ) { # multiple values
      suppressWarnings( print( plot(indics.test) ) )
      suppressWarnings( print( plot(indics) ) )
    }
    
    # Warn when matrix is logical but no cg will be performed
    expect_warning({ 
      generic_sews(forestgap, subsize = 1)
    })

    
    
    # Spectral indicators
    indics <- spectral_sews(dataset, 
                            sdr_low_range  = c(0,  0.2), 
                            sdr_high_range = c(.8, 1)) 
    test_methods("Spectrum-based indicators", 
                 length(dataset), indics, .test_df = FALSE)
    expect_warning({ spectral_sews(dataset) }) # give a warning when no args are passed
    
    indics.test <- indictest(indics, nulln = 3)
    test_methods("Spectrum-based indicators", 
                  datal, indics.test, .test_df = FALSE)
    
    if ( datal > 1 ) { # multiple values
      suppressWarnings( print( plot(indics.test) ) )
      suppressWarnings( print( plot(indics) ) )
    }
    suppressWarnings( print( plot_spectrum(indics.test) ) ) 
    suppressWarnings( print( plot_spectrum(indics) ) ) 
    
    
    
    
    # PSD-based indicators. Suppress warnings that can be produced by optim() 
    # here. 
    indics <- suppressWarnings( patchdistr_sews(dataset, fit_lnorm = TRUE) )
    test_methods("Patch-based indicators", 
                 datal*4, indics) # l(dataset) * 4 psd types fitted
    # test_methods("Patch-based Early-Warnings results", 
    #              datal*4, indics[[1]])
    # Here, indictest may produce warnings on the testing datasets, so we 
    # quiet them down
    indics.test <- suppressWarnings( indictest(indics, nulln = 3) )
    test_methods("Patch-based indicators", 
                  datal*4, indics.test, .test_df = FALSE)
    
    # Test prediction of PSDs
    indics.pred <- predict(indics)
    
    if ( ! is.matrix(dataset) ) { 
      suppressWarnings( print( plot(indics) ) )
    }
    suppressWarnings( print( plot_distr(indics) ) )
    suppressWarnings( print( plot_distr(indics.test) ) )
    
    # We print a warning when there is no data to display because all fits 
    # failed. This can happen e.g. when there is a single patch in a matrix
    a <- matrix(FALSE, ncol = 10, nrow = 10) 
    a[1,1] <- TRUE
    a <- list(a, a)
    expect_warning({ 
      plot(patchdistr_sews(a))
    })
    
    
    
    # Variogram-based indicators
    indics <- variogram_sews(dataset)
    test_methods("Spatial Early-Warning: Variogram-based indicators", 
                 # l(dataset) * 4 metrics produced
                 datal*4, indics, .test_df = FALSE) 
    indics.test <- indictest(indics, 3)
    test_methods("Spatial Early-Warning: Variogram-based indicators", 
                 # l(dataset) * 4 metrics produced
                 datal*4, indics.test, .test_df = FALSE) 
    
  }
  
})

