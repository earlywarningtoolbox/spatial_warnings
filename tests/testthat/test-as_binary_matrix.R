
context('as.binary_matrix.* functions work as expected')

# Explicitely load forestdat
data(forestdat)

test_that('conversion from matrix works', { 
  
  # Matrix object
  mat <- forestdat[['matrices']][[1]] # data object
  mat_NA <- mat 
  mat_NA[10] <- NA 
  
  expect_is(as.binary_matrix(mat), 'binary_matrix')
  expect_error(as.binary_matrix(mat_NA))
  
})


test_that('conversion from data.frame works', { 
  # df object
  
  # Logical case
  df_logical    <- as.data.frame(forestdat[['matrices']][[1]])
  df_logical_NA <- as.data.frame(forestdat[['matrices']][[1]])
  df_logical_NA[3, 4] <- NA
  expect_is(as.binary_matrix(df_logical), 'binary_matrix')
  expect_error(as.binary_matrix(df_logical_NA))
  
  # Character case
  df_char <- data.frame(a = letters, b = rev(letters))
  expect_error(as.binary_matrix(df_char)) # no ref state specified
  expect_is(as.binary_matrix(df_char, state = 'r'), 'binary_matrix')
  
  # Numeric case
  df_num <- data.frame(a = rnorm(100), b = rnorm(100))
  expect_error(as.binary_matrix(df_num)) # no ref state specified
  expect_is(as.binary_matrix(df_num, state = 20), 'binary_matrix')
  
  # Integer class
  df_int <- data.frame(a = seq.int(100), b = seq.int(100)) # integer
  expect_error(as.binary_matrix(df_int)) # no ref state specified
  expect_is(as.binary_matrix(df_int, state = 20), 'binary_matrix')
  
})

# Bad class object
bad_class <- lm(a ~ b, data = data.frame(a = rnorm(10), b = rnorm(10)))

test_that('bad class object produces an error', { 
  expect_error(as.binary_matrix(bad_class))
})
