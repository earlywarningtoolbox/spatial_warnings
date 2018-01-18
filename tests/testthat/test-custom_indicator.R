
context('Test the creation of custom indicators')

test_that('Custom indicators work', { 
  
  # Run a classical workflow and make sure there are no errors
  
  # Absorb all output in a temporary file
  sink(tempfile())
  
  maxpatchsize <- function(mat) { 
    max(patchsizes(mat > 0))
  }

  indicator_mp <- create_indicator(maxpatchsize)
  a <- indicator_mp(forestgap)
  
  summary(a)
  summary(a[[1]])

  as.data.frame(a) 
  as.data.frame(a[[1]]) 

  indictest(a[[1]], nperm = 9)

  options(mc.cores = 2)
  b <- indictest(a, nperm = 19)
  
  summary(b)
  print(b)
  
  sink()
  
#   library(ggplot2)
#   plot(b) + 
#     scale_y_log10()

})
