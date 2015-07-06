
# We define the broad context of the tests in this file
context('Testing indicator_example')

# We build some necesary data
example_mat <- diag(10) # an example matrix
example_list_mat <- list(diag(10), diag(5)) # an example list of matrices
example_garbage <- list(NA, diag(10))

test_that('indicator_example produces proper output', { 
  
  # The result applied on a single matrix
  expect_is(indicator_example(example_mat),    c('numeric','integer'))
  expect_is(indicator_example(example_list_mat), 'list')  
  
})


test_that('indicator_example handles bad arguments correctly', { 

  # Proper handling of garbage arguments
  expect_error(indicator_example(example_garbage))
  
})
