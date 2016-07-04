# 
# 
# This file contains a unit tests that make sure the complete workflow is working
# 

context('Test workflows') 


test_that("The Generic-spews workflow works", { 
  
    
    data(forestdat)
    data(arid)
    
    datasets <- list(forestdat[['matrices']], arid, 
                     forestdat[['matrices']][[1]], arid[[1]])
    
    for ( dataset in datasets ) { 
      
    expect_true({
      capture.output({
        
        gensp <- generic_spews(dataset) 
        
        print.generic_spews(gensp)        
        summary.generic_spews(gensp)      
        as.data.frame(gensp) 
        as.data.frame(gensp) 
        
        
        gensp.test <- indictest(gensp)
        
        print.generic_spews_test(gensp.test)        
        summary.generic_spews_test(gensp.test)      
        as.data.frame.generic_spews_test(gensp.test)
        
        if ( ! is.matrix(dataset) ) { 
          
          plot.generic_spews_test(gensp.test)  
          plot.generic_spews(gensp)            
        }
        
      })
    
    # Return true 
    TRUE})
        
  }
  
})


test_that("The Spectral-spews workflow works", { 
  
    
    data(forestdat)
    data(arid)
    
    datasets <- list(forestdat[['matrices']], arid, 
                     forestdat[['matrices']][[1]], arid[[1]])
    
    for ( dataset in datasets ) { 
      
    expect_true({
      capture.output({
        
        specsp <- spectral_spews(dataset) 
        
        print.spectral_spews(specsp)        
        
        if ( ! is.matrix(dataset) ) { 
          summary.spectral_spews_list(specsp)      
          as.data.frame.spectral_spews_list(specsp) 
        } else { 
          summary.spectral_spews_single(specsp)      
          as.data.frame.spectral_spews_single(specsp) 
        }
        
        
        specsp.test <- indictest(specsp)
        
        print.spectral_spews_test(specsp.test)        
        
        # Missing functions !
        # summary.spectral_spews_test(specsp.test)      
        # as.data.frame.spectral_spews_test(specsp.test)
        print('NOTE: 2 methods missing')
        
        if ( ! is.matrix(dataset) ) { 
          plot.spectral_spews_test(specsp.test)  
        }
        
      })
    
    # Return true 
    TRUE})
    
  }
  
})

