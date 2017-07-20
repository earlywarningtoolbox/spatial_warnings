# 
# 
# This file contains a unit tests that make sure the complete workflow is working
# 

context('Test workflows') 

data(forestgap)
data(serengeti)

datasets <- list(forestgap[[3]], 
                 forestgap[1:3], serengeti[5:6])

test_that("The Generic-spews workflow works", { 
  
    for ( dataset in datasets ) { 
      
    expect_true({
      capture.output({
        
        gensp <- generic_spews(dataset) 
        
        print(gensp)        
        summary(gensp)      
        as.data.frame(gensp) 
        
        gensp.test <- indictest(gensp, nperm = 29)
        
        print(gensp.test)        
        summary(gensp.test)      
        as.data.frame(gensp.test) 
        
        if ( ! is.matrix(dataset) ) { 
          suppressWarnings( print( plot(gensp.test) ) )
          suppressWarnings( print( plot(gensp) ) )
        }
        
      })
    
    # Return true 
    TRUE})
        
  }
  
})


test_that("The Spectral-spews workflow works", { 
  
    for ( dataset in datasets ) { 
      
    expect_true({
      capture.output({
        
        specsp <- spectral_spews(dataset, quiet = TRUE) 
        
        print(specsp)        
        summary(specsp)
        as.data.frame(specsp)
        
        specsp.test <- indictest(specsp, nperm = 29)
        
        print(specsp.test)
        summary(specsp.test)      
        as.data.frame(specsp.test)
        
        if ( ! is.matrix(dataset) ) { 
          suppressWarnings( print( plot(specsp.test) ) )
          suppressWarnings( print( plot(specsp) ) )
        }
        
      })
    
    # Return true 
    TRUE})
    
  }
  
})


test_that("The PSD-spews workflow works", { 
  
    for ( dataset in datasets ) { 
    
    expect_true({
      
      capture.output({
        
        specsp <- suppressWarnings( patchdistr_spews(dataset) )
        specsp <- suppressWarnings( patchdistr_spews(dataset, fit_lnorm = TRUE) )
        
        print(specsp)        
        summary(specsp)      
        as.data.frame(specsp) 
        
        if ( ! is.matrix(dataset) ) { 
          # This produces warnings because of some NAs
          suppressWarnings( print( plot.patchdistr_spews(specsp) ) )
        }
        
      })
      
    # Return true 
    TRUE})
    
  }
  
})

