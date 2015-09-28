
context('as.binary_matrix.* functions work as expected')

# Explicitely load B
data(B)

test_that('conversion from matrix works', { 
  
  # Matrix object
  mat <- B # data object
  mat_NA <- mat 
  mat_NA[10] <- NA 
  
  expect_is(as.binary_matrix(mat), 'binary_matrix')
  expect_error(as.binary_matrix(mat_NA))
  
})


test_that('conversion from data.frame works', { 
  # df object
  df_logical    <- as.data.frame(B)
  df_logical_NA <- as.data.frame(B)
  df_logical_NA[3, 4] <- NA
  
  expect_is(as.binary_matrix(df_logical), 'binary_matrix')
  expect_error(as.binary_matrix(df_logical_NA))
  
})

# Lists of stuff
# list_mat <- L
# list_mat_NA <- c(L, mat_NA)

# Bad class object
bad_class <- lm(a ~ b, data = data.frame(a = rnorm(10), b = rnorm(10)))

test_that('bad class object produces an error', { 
  expect_error(as.binary_matrix(bad_class))
})