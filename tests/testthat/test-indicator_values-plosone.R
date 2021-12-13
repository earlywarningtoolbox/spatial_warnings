# 
# 
# This file contains code that tests the generic indicator result against 
#   the published results in PLOS One (Kéfi et al. 2014)
# 

context("Test that results match those in PLOS One")

test_that('results matches those in PLOS One', { 
  
  if ( requireNamespace("moments", quietly = TRUE) ) { 
    
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
    dataplos <- data.matrix(fulldat)
    nreplicates <- nrow(dataplos) / ncol(dataplos)
    
    # Extract the matrices from the binary data and conver it
    startends <- data.frame(start = (seq.int(nreplicates)-1)*ncol(dataplos)+1,
                            end   = seq.int(nreplicates)*ncol(dataplos))
    
    matrices <- plyr::dlply(startends, ~ start + end, 
                            function(df) { 
                              dataplos[seq.int(df[['start']], df[['end']]), ]
                            })
    matrices <- lapply(matrices, function(x) x == 1) # veg is 1
    
    # Now compute indicators
    test_results  <- generic_sews(matrices, subsize = 10, 
                                  moranI_coarse_grain = TRUE)
    test_reshaped <- ddply(as.data.frame(test_results), ~ matrixn, 
                           function(df) { 
      a <- as.list(df[ ,"value"])
      names(a) <- df[ ,"indic"]
      a <- a[c("mean", "moran", "skewness", "variance")]
      as.data.frame(a)
    })
    test_reshaped <- as.matrix(test_reshaped[ ,-1])
    
    # Now test for concordance
    ref_results <- cbind(mean = mean_reduced,  # ! order matters !
                         corr = corr_reduced, 
                         skew = skew_reduced, 
                         var  = var_reduced)
    
    # Mean skew var are expected to map 1:1
    expect_true(all(abs(ref_results[ ,"mean"] - test_reshaped[ ,"mean"]) < 1e-10))
    expect_true(all(abs(ref_results[ ,"skew"] - test_reshaped[ ,"skew"]) < 1e-10))
    expect_true(all(abs(ref_results[ ,"var"] - test_reshaped[ ,"var"]) < 1e-10))
    
    # Moran correlation is computed differently in spw, so resuults do not have to 
    # match exactly 
    expect_true(cor(ref_results[ ,"corr"], test_reshaped[ ,"moran"]) > 0.8)
    
  }
  
})
