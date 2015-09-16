# 
# These functions factorize some code between numerical indicators. Most 
# notably it handles creating the null model testing, etc.
# 
# 
compute_indicator_with_null <- function(input, 
                                        subsize, 
                                        detrending, 
                                        discrete,
                                        nreplicates, 
                                        indicator_function) { 
  
  # Check options and apply modifications --------------------
  if (detrending) { 
    input <- input - mean(input)
  }
  
  # Check whether we need to coarse-grain before computing the indicator or not
  if (discrete) { 
    indicf <- make_indic_f_with_cg(indicator_function, subsize)
  } else { 
    indicf <- indicator_function
  }
  
  # Compute the indicator_function
  # Note: subsize is always passed so the indicator_function function should 
  # accept extra arguments.
  value <- indicf(input)
  result <- list(mean = mean(input), 
                 value = value)
  
  if (nreplicates > 2) { 
    # Compute the index on a randomized matrix
    nulldistr <- replicate(nreplicates, 
                           indicf(matrix(sample(input), nrow = nrow(input))) )
    result <- c(result,
                list(null_mean = mean(nulldistr), 
                     null_sd   = sd(nulldistr),
                     z_score   = (value - mean(nulldistr)) / sd(nulldistr),
                     # Should the p-value be one-sided/two-sided ? 
                     pval      = 1 - rank(c(value, nulldistr))[1] / (nreplicates+1)))
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
