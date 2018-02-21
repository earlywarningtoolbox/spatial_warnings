# 
# 
# This file contains a unit tests that make sure the complete workflow is working
# 

context('Test workflows') 

data(forestgap)
data(serengeti)

datasets <- list(forestgap[[3]], 
                 forestgap[2:4], 
                 serengeti[5:6])

test_that("The workflow functions work", { 
  
  for ( dataset in datasets ) { 
      
    # Generic indicators
    expect_true({
      capture.output({
        
        indics <- generic_spews(dataset) 
        
        print(indics)
        summary(indics)
        as.data.frame(indics) 
        
        indics.test <- indictest(indics, nperm = 29)
        print(indics.test)        
        summary(indics.test)      
        as.data.frame(indics.test) 
        
        if ( ! is.matrix(dataset) ) { # multiple values
          suppressWarnings( print( plot(indics.test) ) )
          suppressWarnings( print( plot(indics) ) )
        }
        
      })
    # Return true 
    TRUE})
    
    
    # Spectral-based indicators
    expect_true({
      capture.output({
        
        # These two comands should produce the same thing
        indics <- spectral_spews(dataset, 
                                 sdr_low_range  = c(0,  0.2), 
                                 sdr_high_range = c(.8, 1)) 
        
        indics <- spectral_spews(dataset, quiet = TRUE) 
        
        print(indics)
        summary(indics)
        as.data.frame(indics) 
        
        indics.test <- indictest(indics, nperm = 29)
        print(indics.test)        
        summary(indics.test)      
        as.data.frame(indics.test) 
        
        if ( ! is.matrix(dataset) ) { 
          suppressWarnings( print( plot(indics.test) ) )
          suppressWarnings( print( plot(indics) ) )
        }
        
        suppressWarnings( plot_spectrum(indics)      )
        suppressWarnings( plot_spectrum(indics.test) )
        
      })
        
    # Return true 
    TRUE})
    
    # PSD-based indicators
    expect_true({
      capture.output({
        
        indics <- patchdistr_spews(dataset) 
        
        print(indics)
        summary(indics)
        as.data.frame(indics) 
        
        if ( ! is.matrix(dataset) ) { 
          suppressWarnings( print( plot(indics) ) )
        }
        
        # This test can sometimes fail (?! something to investigate ?)
        predict(indics)
        
        suppressWarnings( plot_distr(indics) )
        
      })
    # Return true 
    TRUE})
    
  }
  
})

