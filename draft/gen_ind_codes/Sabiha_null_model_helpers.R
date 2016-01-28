# 
# These functions factorize some code between numerical indicators. Most 
# notably it handles creating the null model testing, etc.
# 
# 
compute_indicator_with_null <- function(input, 
                                        subsize, 
                                        nreplicates, 
                                        indicator_function) { 
  

 # coarse_grain the input and calculte the indicator
  value <- indicator_function(coarse_grain(input,subsize))
  result <- list(value = value)
  
  if (nreplicates > 2) { 
    # Compute the index on a randomized matrix
    

    nulldistr <- replicate(nreplicates, 
                           indicator_function(coarse_grain((matrix(sample(input), nrow = nrow(input))),subsize)) )
    
    #if (mean(nulldistr)==0 && sd(nulldistr)==0) {
    #    MYz_score=0; MYnull_95 = 0; MYnull_05=0;
    #}
    #else {
    #MYz_score= (value - mean(nulldistr)) / sd(nulldistr);
    #      MYnull_95 = quantile(nulldistr, .95);
    #      MYnull_05 = quantile(nulldistr, 0.05)
    #}
    result <- c(result,
                list(null_mean = mean(nulldistr), 
                     null_sd   = sd(nulldistr)))
                                      
  }
  
  return(result)
}
  

# This creates an alternate version of the indicator_function above that 
# does coarse-graining before computing its value.
make_indic_f_with_cg <- function(indicf, subsize) { 
  function(matinput) { 
    indicf( coarse_grain(matinput, subsize) )
  }
}


compute_indicatorSDR_with_null <- function(input, 
                                        subsize, 
                                        nreplicates, 
                                        indicator_function,
                                        low_range,high_range) { 
  
    
  # Check whether we need to coarse-grain before computing the indicator or not


    
  # Compute the indicator_function
  # Note: subsize is always passed so the indicator_function function should 
  # accept extra arguments.
  value <- indicator_function(input,low_range,high_range)
  result <- list(value = value)
  
  if (nreplicates > 2) { 
    # Compute the index on a randomized matrix
    nulldistr <- replicate(nreplicates, 
                           indicator_function(matrix(sample(input), nrow = nrow(input)),low_range,high_range) )
    
    #if (mean(nulldistr)==0 && sd(nulldistr)==0) {
    #    MYz_score=0; MYnull_95 = 0; MYnull_05=0;
    #}
    #else {
    #MYz_score= (value - mean(nulldistr)) / sd(nulldistr);
    #      MYnull_95 = quantile(nulldistr, .95);
    #      MYnull_05 = quantile(nulldistr, 0.05)
    #}
    result <- c(result,
                list(null_mean = mean(nulldistr), 
                     null_sd   = sd(nulldistr)))
    
  }
  
  return(result)
}
#unused results
#null_95   = MYnull_95,
#null_05   = MYnull_05,
#zscore    = MYz_score,
## Should the p-value be one-sided/two-sided ? 
#pval      = 1 - rank(c(value, nulldistr))[1] / (nreplicates+1))) 