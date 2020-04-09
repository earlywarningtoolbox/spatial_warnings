

context('Test the deprecation of functions')


test_that("Deprecation works and produces warnings", { 
  
  expect_error(generic_spews(forestgap[[3]]))
  expect_error(spectral_spews(forestgap[[3]]))
  expect_error(patchdistr_spews(forestgap[[3]]))
  
  expect_warning(indicator_sdr(forestgap[[2]]), nulln = 10)
  expect_warning(indicator_variance(forestgap[[2]]), nulln = 10)
  expect_warning(indicator_skewness(forestgap[[2]]), nulln = 10)
  expect_warning(indicator_moran(forestgap[[2]]), nulln = 10)
  
})

