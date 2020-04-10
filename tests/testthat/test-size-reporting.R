# 
# 
# 

test_that("Correct sizes are reported", { 
  
  a <- generic_sews(serengeti)
  expect_true( any(grepl("size: 250x250", capture.output(print(a)))) )
  b <- indictest(a, 3)
  expect_true( any(grepl("size: 250x250", capture.output(print(b)))) )
  
  # List of matrices with different sizes
  expect_warning(generic_sews(arizona))
  a <- suppressWarnings({ generic_sews(arizona) })
  expect_true( any(grepl("(variable sizes)", capture.output(print(a)))) )
  b <- indictest(a, 3)
  expect_true( any(grepl("(variable sizes)", capture.output(print(b)))) )
  
})
