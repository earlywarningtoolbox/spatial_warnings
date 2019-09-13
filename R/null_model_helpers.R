# 
# These functions factorize some code between numerical indicators. Most 
# notably it handles creating the null model testing, etc.
# 
compute_indicator_with_null <- function(input, 
                                        nreplicates, 
                                        indicf, 
                                        null_method, 
                                        covariate_layers) { 
  
  # Compute the observed value
  value  <- indicf(input)
  result <- list(value = value)
  
  if (nreplicates > 2) { 
    
    if ( length(unique(input)) == 1 ) { 
      # Compute the index on the same matrix as randomization will do nothing
      nulldistr <- matrix(rep(indicf(input), nreplicates), 
                          nrow = 1, ncol = nreplicates)
      
    # We use permutation to produce null models
    } else if ( null_method == "perm" ) { 
      # Compute the index on a randomized matrix
      # shuffle_and_compute will convert all matrices to numeric matrices 
      # internally. We need to explicitely convert back to logical after 
      # shuffling before computing the indicator 
      if ( is.logical(input) ) { 
        nulldistr <- shuffle_and_compute(input, function(x) indicf(x>0), 
                                         nreplicates)
      } else { 
        nulldistr <- shuffle_and_compute(input, indicf, nreplicates)
      }
      
    } else if ( null_method == "glm" ) { 
      
      # We build a model that takes covariates into account
      N <- nrow(input) * ncol(input)
      glm.family <- if ( is.logical(input) ) binomial() else gaussian()
      glm.df <- data.frame(y = as.vector(input))
      form_string <- "y ~ 1"
      if ( !is.null(covariate_layers)) { 
       glm.df <- data.frame(glm.df, 
                            do.call(cbind, lapply(covariate_layers, as.vector)))
       form_string <- paste(form_string, "+", paste(names(covariate_layers), 
                                                    collapse = " + "))
      } 
      
      mod <- glm(as.formula(form_string), 
                 family = glm.family, 
                 data = glm.df)
      
      nulldistr <- generate_glm_nulldistr(mod, 
                                          is_binomial = is.logical(input), 
                                          indicf = indicf, 
                                          nr = nrow(input), 
                                          nc = ncol(input), 
                                          nreplicates = nreplicates)
      
    } else { 
      stop(paste0("Unknown null-model method: ", null_method))
    }
    
    # nulldistr is a list so far => combine it to a matrix, with each null 
    # replicate as a row
    nulldistr <- t( do.call(rbind, nulldistr) )
    
    # Note that here nulldistr always has one or more rows and nreplicates 
    #   columns -> it is always a matrix
    nullstats <- list(null_mean = apply(nulldistr, 1, mean), 
                      null_sd   = apply(nulldistr, 1, sd), 
                      null_95   = apply(nulldistr, 1, safe_quantile, .95), 
                      null_05   = apply(nulldistr, 1, safe_quantile, .05), 
                      z_score   = apply(cbind(value, nulldistr), 1, 
                                        function(X) (X[1] - mean(X[-1])) / sd(X[-1])),
                      pval      = apply(cbind(value, nulldistr), 1, 
                                        function(X) 1 - rank(X)[1] / length(X)))
    result <- append(result, nullstats)
  }
  
  return(result)
}

# 
generate_glm_nulldistr <- function(mod, is_binomial, indicf, 
                                   nr, nc, 
                                   nreplicates) { 
  
  lapply(seq.int(nreplicates), function(n) { 
    if ( is_binomial ) { 
      newmat <- matrix(rbinom(nr*nc, 1, predict(mod, type = "response")), 
                       nrow = nr, ncol = nc)
    } else { 
      # Gaussian model
      newmat <- matrix(rnorm(nr*nc, 
                             mean = predict(mod, type = "response"), 
                             sd = sigma(mod)), 
                      nrow = nr, ncol = nc)
    }
    
    indicf(newmat)
  })
}

# We use a safe version of quantile that reports as warnings 
# the appearance of NAs in the null distribution.
safe_quantile <- function(nulldistr, p) { 
  if ( any( is.na(nulldistr) ) ) { 
    warning(paste0('Computation of null values produced NAs (', 
                   sum(is.na(nulldistr)), " out of ", length(nulldistr), "). "))
  }
  quantile(nulldistr, p, na.rm = TRUE)
}
