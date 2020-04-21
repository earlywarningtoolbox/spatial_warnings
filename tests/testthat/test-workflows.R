# 
# 
# This file contains a unit tests that make sure the complete workflow is working
# 

context('Test workflows') 

data(forestgap)
data(serengeti)

# We run theses tests without parallelism as it appears to produce spurious
# warnings. See: https://github.com/HenrikBengtsson/future/issues/13
# Note that this is not entirely satisfactory as the above issue claims that 
# this problem has been fixed. 
plan(sequential)

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
    
    
    
    
    # Flowlength indicator
    indics <- flowlength_sews(dataset)
    test_methods("Spatial Early-Warning: Flow length", 
                 length(dataset), indics, .test_df = FALSE)
    if ( ! is.matrix(dataset) ) { 
      suppressWarnings( print( plot(indics) ) )
    }
    
    indics.test <- indictest(indics, nulln = 3)
    test_methods("Spatial Early-Warning: Flow length", 
                  datal, indics.test, .test_df = FALSE)
    
    
    
    # KBDM indicator
    indics <- kbdm_sews(dataset) 
    test_methods("Spatial Early-Warning: Kbdm Complexity", 
                 datal, indics) # l(dataset) * 4 psd types fitted
    
    if ( ! is.matrix(dataset) ) { 
      suppressWarnings( print( plot(indics) ) )
    }
    
    
    # Variogram-based indicators
    indics <- variogram_sews(dataset)
    test_methods("Spatial Early-Warning: Variogram-based indicators", 
                 datal*4, indics, .test_df = FALSE) # l(dataset) * 4 metrics produced
    indics.test <- indictest(indics, 3)
    test_methods("Spatial Early-Warning: Variogram-based indicators", 
                 datal*4, indics.test, .test_df = FALSE) # l(dataset) * 4 metrics produced
    
  }
  
})

