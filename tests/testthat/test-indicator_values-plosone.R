# 
# 
# This file contains code that tests the generic indicator result against 
#   the published results in PLOS One (Kéfi et al., etc)
# 

context("Test that results matches those in PLOS One")

test_that('results matches those in PLOS One', { 
  
  stopifnot(require(reshape2))
  
  datdir <- './plosone/' # mind the trailing /
  source(paste0(datdir,'early_warning_generic_R_code.R'), chdir = TRUE)
  
  # This creates a whole bunch of variables, among which : 
  # mean_reduced, var_reduced, cvar_reduced, skew_reduced that contains 
  #   indicator values after a 10x10 coarse-graining
  # 
  # We redo the same our way and compare the results
  
  cgsize <- 10
  
  # Read the data first
  fulldat <- read.table(paste0(datdir, 'CA_all.txt'))
  data <- data.matrix(fulldat)
  nreplicates <- nrow(data) / ncol(data)
  
  # Extract the matrices from the binary data and conver it
  startends <- data.frame(start = (seq.int(nreplicates)-1)*ncol(data)+1,
                          end   = seq.int(nreplicates)*ncol(data))
  matrices <- plyr::dlply(startends, ~ start + end, 
                          function(df) { 
                            data[seq.int(df[['start']], df[['end']]), ]
                          })
  matrices <- lapply(matrices, function(x) x == 1) # veg is 1
  
  # Now compute indicators
  test_results  <- generic_spews(matrices, subsize = 10, 
                                 moranI_coarse_grain = TRUE)
  test_reshaped <- acast(as.data.frame(test_results), 
                         replicate ~ indicator)
  
  test_reshaped
  
  # Now test for concordance
  ref_results <- cbind(mean = mean_reduced,  # ! order matters !
                       corr = corr_reduced, 
                       skew = skew_reduced, 
                       var  = var_reduced)
  
  expect_true(all(abs(ref_results - test_reshaped) < 1e-10))
})
