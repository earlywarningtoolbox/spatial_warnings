
context('Test the creation of custom indicators')

data(forestgap)
data(serengeti)

datasets <- list(forestgap[3:4], 
                 forestgap[1:3], 
                 serengeti[5:6])

test_that('Custom indicators work', { 
  
  for (dataset in datasets) { 
    # Run a classical workflow and make sure there are no errors
    expect_true({ 
      capture.output({
        
        maxpatchsize <- function(mat) { 
          max(patchsizes(mat > 0))
        }
        
        indicator_mp <- create_indicator(maxpatchsize)
        a <- indicator_mp(dataset)
        
        summary(a)
        summary(a[[1]])
        
        as.data.frame(a) 
        as.data.frame(a[[1]]) 
        
        if (length(dataset) > 1) { 
          plot(a)
        }
        
        indictest(a[[1]], nperm = 9)
        summary(a[[1]])
        as.data.frame(a[[1]])
        print(a[[1]])
        
        options(mc.cores = 2) 
        b <- indictest(a, nperm = 19)

        summary(b)
        print(b)
        
        if (length(dataset) > 1) { 
          plot(b)
        }
      })
      
      TRUE
    })
    
    indicator_mp <- create_indicator(maxpatchsize)
    a <- indicator_mp(dataset)
    
    expect_true(all.equal(a, custom_indicator(dataset, fun = maxpatchsize)))
    
  }
#   library(ggplot2)
#   plot(b) + 
#     scale_y_log10()

})
