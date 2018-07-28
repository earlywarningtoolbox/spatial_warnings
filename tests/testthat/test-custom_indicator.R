
context('Test the creation of custom indicators')

data(forestgap)
data(serengeti)

datasets <- list(forestgap[3:4], 
                 forestgap[1:2])

test_methods <- function(teststring, datalength, obj) { 
  
  ok_print <- any(grepl(teststring, capture.output(print(obj))))
  expect_true(ok_print)
  
  ok_summary <- any(grepl(teststring, capture.output(summary(obj))))
  expect_true(ok_summary)
  
  ok_as_df <- nrow(as.data.frame(obj)) == datalength
  expect_true(ok_as_df)
  
  return(TRUE)
}

test_that('Custom indicators work', { 
  skip_on_cran()
  
  for (dataset in datasets) { 
    # Run a classical workflow and make sure there are no errors
    maxpatchsize <- function(mat) { 
      max(patchsizes(mat > 0))
    }
    
    indicator_mp <- create_indicator(maxpatchsize)
    a <- indicator_mp(dataset)
    
    # Test methods for custom indics
    # Several matrices
    test_methods("Custom Spatial Early-Warnings", length(dataset), a)
    test_methods("Custom Spatial Early-Warnings", 1, a[[1]])
    
    if (length(dataset) > 1) { 
      # Suppress the warnings related to missing values in geom_path
      suppressWarnings( plot(a) )
    }
    
    indictest(a[[1]], nperm = 9)
    
    options(mc.cores = 2) 
    b <- indictest(a, nperm = 9)
    test_methods("Custom Spatial Early-Warnings", length(dataset), b)
    test_methods("Custom Spatial Early-Warnings", 1, b[[1]])
    
    if (length(dataset) > 1) { 
      # Suppress the warnings related to missing values in geom_path
      suppressWarnings( plot(b) )
    }
    
  }
  
  # Test that create_indicator and custom_indicator work the same
  indicator_mp <- create_indicator(maxpatchsize)
  a <- indicator_mp(dataset)
  expect_true({ 
    all.equal(a, custom_indicator(dataset, fun = maxpatchsize))
  })
  
})


test_that('Custom indicators handles anon functions correctly', { 
  # Test create_indicator with anonymous function 
  expect_warning(
    anon_fun_indic <- create_indicator(function(mat) mean(mat))
  )
  expect_true({
    anon_fun_indic <- create_indicator(function(mat) mean(mat), 
                                       fun.name = "testfun")
    TRUE
  })
})


