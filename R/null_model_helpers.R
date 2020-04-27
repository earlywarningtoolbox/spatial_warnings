# 
# These functions factorize some code between numerical indicators. Most 
# notably it handles creating the null model testing, etc.
# 
# This function will compute a null distribution of indicator values on input, 
# by reshuffling it nulln times. 
# 
compute_indicator_with_null <- function(input, 
                                        nulln, 
                                        indicf, 
                                        null_method, 
                                        null_control) { 
  
  # Create null_control list with default arguments 
  null_control <- null_control_set_args(input, null_control)
  
  # Compute the observed value
  value  <- indicf(input)
  result <- list(value = value)
  
  if ( nulln > 2 ) { 
    
    null_results <- generate_nulls(input, indicf, nulln, 
                                   null_method, null_control)
    
    # nulldistr is a list so far => combine it to a matrix, with each null 
    # replicate as a row
    null_results[["nulldistr"]] <- do.call(rbind, null_results[["nulldistr"]]) 
    
    # Compute summary stats
    summ_stats <- 
      list(null_mean = apply(null_results[["nulldistr"]], 2, mean), 
           null_sd   = apply(null_results[["nulldistr"]], 2, sd), 
           null_qsup   = apply(null_results[["nulldistr"]], 2, safe_quantile,
                               null_control[["qsup"]]), 
           null_qinf   = apply(null_results[["nulldistr"]], 2, safe_quantile,
                               null_control[["qinf"]]), 
           z_score   = apply(rbind(value, null_results[["nulldistr"]]), 2, 
                             function(X) (X[1] - mean(X[-1])) / sd(X[-1])),
           pval      = apply(rbind(value, null_results[["nulldistr"]]), 2, 
                             function(X) 1 - rank(X)[1] / length(X)))
    
    # Add to that list what is returned from null + some stats
    result <- c(result, null_results, summ_stats, nulln = nulln, 
                null_method = null_method)
  }
  
  return(result)
}

# Returns a list with the values of indicf obtained from random matrices + 
# a function used to generate the nulls
generate_nulls <- function(input, indicf, nulln, null_method, 
                           null_control) {
                             
  if ( ( !is.function(null_method) ) && 
       ( ! null_method %in% c("perm", "intercept", "smooth") ) ) { 
    stop(paste0("Unknown null model method: ", null_method))
  }
  
  # Nulldistr is null as long as we've not computed it yet
  has_computed_nulldistr <- FALSE
  
  
  
  
  # If the user provided a function to generate null matrices
  if ( is.function(null_method) ) { 
    
    # We warn if the function does not return logical values 
    if ( is.logical(input) && ! is.logical(null_method(input)) ) { 
      warning("The original data is logical (TRUE/FALSE), but the null ", 
              "method does not return logical values")
    }
    
    null_mod <- NULL # No model involved when the function is provided
    nullf <- null_method
  }
  
  
  
  
  # If the null method is set to permutation
  if ( is.character(null_method) && null_method == "perm" ) { 
    
    # Compute the index on a randomized matrix. Here we use a dedidcated 
    # function that can do the shuffling in-place in C++, instead of relying 
    # on R which may copy matrices in memory many times. 
    # 
    # shuffle_and_compute will convert all matrices to numeric matrices 
    # internally. We need to explicitely convert back to logical after 
    # shuffling before computing the indicator. 
    if ( is.logical(input) ) { 
      nulldistr <- shuffle_and_compute(input, function(x) indicf(x>0), 
                                       nulln)
    } else { 
      nulldistr <- shuffle_and_compute(input, indicf, nulln)
    }
    has_computed_nulldistr <- TRUE
    null_mod <- NULL # No model involved when we are permuting
    nullf <- function() { 
      if ( is.logical(input) ) { 
        shuffle_matrix(input) > 0.5 
      } else { 
        shuffle_matrix(input) 
      }
    }
  }
  
  
  
  
  # If the null method is to fit an intercept only 
  if ( is.character(null_method) && null_method == "intercept" ) { 
    null_mod <- glm(y ~ 1, data = data.frame(y = as.vector(input)), 
                    family = null_control[["family"]])
    nullf <- create_nullmat_generator(null_mod, null_control[["family"]], 
                                      input)
  }
  
  
  
  # If the null method is to fit a smoother to the matrix
  if ( is.character(null_method) && null_method == "smooth" ) { 
    
    if ( ! requireNamespace("mgcv", quietly = TRUE) ) { 
      stop("The 'gam' method requires mgcv. Please install it beforehand.")
    }
    
    # We fit a smoother over the matrix, with automatic parameter selection 
    # for the smoother. 
    mat_tab <- data.frame(expand.grid(row = seq.int(nrow(input)), 
                                      col = seq.int(ncol(input))), 
                          value = as.vector(input))
    null_mod <- mgcv::gam(value ~ s(row, col, bs = "tp"), data = mat_tab, 
                          family = null_control[["family"]])
    nullf <- create_nullmat_generator(null_mod, null_control[["family"]], 
                                      input)
  }
  
  
  
  # Compute the actual indicator values in the null distribution (if that's 
  # not already done)
  if ( ! has_computed_nulldistr ) { 
    nulldistr <- lapply(seq.int(nulln), function(n) { 
      indicf(nullf())
    })
  }

  # We did not generate a null distribution
  list(nulldistr = nulldistr, 
       nullf     = nullf, 
       null_mod  = null_mod)
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

# Returns TRUE if fam is a binomial() object
is.binomial <- function(fam) { 
  ( !is.list(fam) && fam == "binomial") || 
    ( inherits(fam, "family") && fam[["family"]] == "binomial" )
}

# Set default arguments for null methods 
null_control_set_args <- function(mat, arglist) { 
  
  # Choose a sensible default if family is unset
  family <- NULL
  if ( ! "family" %in% names(arglist) ) { 
    if ( is.logical(mat) ) { 
      family <- binomial()
    } else { 
      family <- gaussian()
    }
  } 
  
  args <- list(family = family, 
               qinf = .05, 
               qsup = .95) # Add other arguments here
  
  for ( i in seq_along(arglist) ) { 
    if ( names(arglist)[i] %in% names(args) ) { 
      args[[ names(arglist)[i] ]] <- arglist[[i]]
    } else { 
      warning("Unknown null model control argument: ", names(arglist)[i])
    }
  }
  
  return(args)
}

# Returns a function that generates matrices, given a null model and the 
# original matrix
create_nullmat_generator <- function(null_mod, family, input) { 
  function() { 
    # When the family is binomial, simulate often returns 1/0 instead of 
    # TRUE/FALSE values so we need to convert it back here. 
    sim <- matrix(stats::simulate(null_mod)[ ,1], 
                  nrow = nrow(input), ncol = ncol(input)) 
    if ( is.binomial(family) ) { 
      sim <- sim > .5
    }
    return(sim)
  }
}

