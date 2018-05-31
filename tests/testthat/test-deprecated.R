

context('Test the deprecation of functions')


test_that("Deprecation works and produces warnings", { 
  
  expect_warning(generic_spews(forestgap[[3]]), "deprecated")
  expect_warning(spectral_spews(forestgap[[3]]), "deprecated")
  expect_warning(patchdistr_spews(forestgap[[3]]), "deprecated")
  
})

