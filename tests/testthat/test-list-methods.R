# 


# 
context('Test that methods are properly listed') 

test_that("Methods are OK", { 
  
  
  # Test variogram sews
  a <- variogram_sews(serengeti[1:2])
  
  # For a list
  mets <- list_methods(class(a))
  expect_true({ 
    all(c('plot_variogram', 'plot', 'predict', 'extract_variogram',
          'indictest') %in% mets)
  })
  
  # For a single element
  mets <- list_methods(class(a[[1]]))
  expect_true({ 
    all(c('plot_variogram', 'predict', 'extract_variogram') %in% mets)
  })
  expect_true({ 
    ! c('plot') %in% mets
  })
  
  
  
  # Test spectral sews
  a <- spectral_sews(serengeti[1:2])
  mets <- list_methods(class(a))
  expect_true({ 
    all(c('display_matrix', 'plot', 'indictest', 'plot_spectrum') %in% mets)
  })
  mets <- list_methods(class(a[[1]]))
  expect_true({ 
    ! 'plot' %in% mets
  })
  
  
  
  # Test simple sews
  a <- compute_indicator(serengeti[1:2], 
                         function(mat) mean(mat), 
                         taskname = "Test indic")
  mets <- list_methods(class(a))
  expect_true({ 
    all(c('display_matrix', 'plot', 'indictest', 'as.data.frame') %in% mets)
  })
  mets <- list_methods(class(a[[1]]))
  expect_true({ 
    ! 'plot' %in% mets
  })
  
  
  
  # Test patchdistr sews
  a <- patchdistr_sews(serengeti[1:2])
  mets <- list_methods(class(a))
  expect_true({ 
    all(c('display_matrix', 'plot', 'indictest', 'as.data.frame') %in% mets)
  })
  mets <- list_methods(class(a[[1]]))
  expect_true({ 
    ! 'plot' %in% mets
  })
  
})
