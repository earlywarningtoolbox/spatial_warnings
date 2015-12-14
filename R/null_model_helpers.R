# 
# These functions factorize some code between numerical indicators. Most 
# notably it handles creating the null model testing, etc.
# 
# WARNING: IT IS UP TO THE INDICF FUNCTION TO DECIDE WHETHER TO COARSE 
#   GRAIN OR NOT
# 
compute_indicator_with_null <- function(input, 
                                        detrending, 
                                        nreplicates, 
                                        indicf) { 
  
  # Check options and apply modifications --------------------
  if (detrending) { 
    input <- input - mean(input)
  }
  
  # Compute the indicator_function
  value  <- indicf(input)
  result <- list(value = value)
  
  if (nreplicates > 2) { 
    # Compute the index on a randomized matrix
    nulldistr <- replicate(nreplicates, 
                           indicf(matrix(sample(input), nrow = nrow(input))) )
    
    # If indicf returns only one value, then we need to set the dimensions of 
    #   nulldistr explicitely so that apply functions work on the only line.
    if ( is.null(dim(nulldistr)) ) { 
      dim(nulldistr) <- c(1, nreplicates)
    }
    
    nullstats <- list(null_mean = apply(nulldistr, 1, mean), 
                      null_sd   = apply(nulldistr, 1, sd), 
                      null_95   = apply(nulldistr, 1, safe_quantile, .95), 
                      null_05   = apply(nulldistr, 1, safe_quantile, .05), 
                      z_score   = apply(cbind(value, nulldistr), 1, 
                                        function(X) (X[1] - mean(X[-1]) / sd(X[-1]))),
                      pval      = apply(cbind(value, nulldistr), 1, 
                                        function(X) 1 - rank(X)[1] / length(X)))
    result <- append(result, nullstats)
    
  }
  
  return(result)
}

# We use a safe version of quantile that reports the appearance of warnings in 
# the null distribution.
safe_quantile <- function(nulldistr, p) { 
  if ( any( is.na(nulldistr) ) ) { 
    warning('Computation of null values produced NAs')
  }
  quantile(nulldistr, p, na.rm = TRUE)
}