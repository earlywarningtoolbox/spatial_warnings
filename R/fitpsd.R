# 
# 
# This file contains code related to patch size distribution fitting. These 
#   functions can fit Power-law (pl), Truncated Power-law (tpl), Lognormal 
#   (lnorm) and Exponential (exp) distributions using maximum likelihood, as 
#   per Clauset et al. 's recommendations. 
# 


# Notes on psd fitting
# 
# 
# We want to fit four types of distributions using MLE: PL/TPL/LNORM/EXP
#   with an xmin of 1 (no xmin basically), except for PL for which xmin can vary





# Riemann zeta function with xmin taken into account :
# sum(1/k^-expo for i=xmin to i = inf
# This is vectorized over xmins so that we do sum things several times. 
zeta_w_xmin <- function(expo, xmins) { 
  perm <- order(xmins)
  xmins <- xmins[order(xmins)]
  
  # Compute zeta value
  zetaval <- VGAM::zeta(expo)
  
  # Initialize
  output <- rep(NA, length(xmins))
  current_k <- xmins[1]
  output[perm[1]] <- zetaval - sum_all_one_over_k(from = 1, to = xmins[1], expo)
  
  # If there is only one value, we bail now
  if ( length(xmins) <= 1) { 
    return(output)
  }
  
  for ( i in 2:length(xmins)) { 
    next_k <- xmins[i]
    if (next_k > current_k) { 
      output[perm[i]] <- output[perm[i-1]] - 
                           sum_all_one_over_k(from = current_k, 
                                              to = next_k, expo)
      current_k <- next_k
    } else { 
      output[perm[i]] <- output[perm[i-1]]
    }
  }
  
  return(output)
}

# PL fitting 
# ---------------------------------------


# PL: P(x=k)
dpl <- function(x, expo, xmin = 1, log = FALSE) { 
  const <- VGAM::zeta(expo)
  
  # Adjust constant for threshold (note that this has no effect if xmin == 1, 
  #   as expected)
  const <- const - sum_all_one_over_k(from = 1, to = xmin, expo)
  
  # Compute values
  if (!log) { 
    ans <- (1/const) * x^(-expo)
    ans[x < xmin] <- NA
  } else { 
    if ( const < 0 ) { 
      # Const can be negative as nlm finds its way: the check makes sure 
      # no warning is produced by the log. 
      ans <- NaN
    } else { 
      ans <- -expo * log(x) - log(const)
    }
  }
  
  return(ans)
}

# PL: P(x>=k) 
ppl <- function(x, expo, xmin = 1) { 
  const <- VGAM::zeta(expo)
  
  is_below_xmin <- x < xmin
  
  # Adjust constant for threshold (note that this has no effect if xmin == 1, 
  #   as expected)
  const <- const - sum_all_one_over_k(from = 1, to = xmin, expo)
  
#   ans <- sapply(x, function(x) zeta_w_xmin(expo, x)/const)
  ps <- zeta_w_xmin(expo, x[!is_below_xmin]) / const 
  
  # Values below threshold are NA'ed
  ans <- NA*x;
  ans[!is_below_xmin] <- ps
  
  return(ans)
}

# PL: Log likelihood
pl_ll <- function(dat, expo, xmin) { 
#   a <- sum( dpl(dat, expo, xmin, log = TRUE) )  
#   cat(length(dat), " data points (", xmin, "/", expo, ") (xmin/expo)", 
#       " -> ", a, "\n", sep = "")
  sum( dpl(dat, expo, xmin, log = TRUE) )  
}

# PL: Fit by MLE
# Method is approximate if xmin > 10 (correponding to an error of 0.1% if 
#   expo = 2.0), but that can be overridden by parameter "auto"
pl_fit <- function(dat, xmin = 1, method = "auto") { 
  
  # Check and decide on the right method to apply
  if ( ! method %in% c("auto", "approx", "direct") ) { 
    stop("Unknown method for power-law fitting")
  }
  
  if ( method == "auto" && xmin >= 10) { 
    method <- "approx"
  } else { 
    method <- "direct"
  }
  
  # Cut data to specified range
  dat <- dat[dat >= xmin] 
  
  # Start with the approximation given in Clauset's 
  npts <- length(dat)
  expo_estim <- 1 + npts / ( sum(log(dat)) - npts * log(xmin-.5) )
  
  # If we want no approximation, then find the best fit
  if ( method == "direct") { 
    negll <- function(expo) { - pl_ll(dat, expo, xmin) }
    
    if ( OPTIMWARNINGS ) { 
      est <- optim(expo_estim, negll, method = 'L-BFGS-B', 
                   lower = 1 + .Machine$double.eps) # expo is always > 1
    } else { 
      est <- suppressWarnings( optim(expo_estim, negll, method = 'L-BFGS-B', 
                                     lower = 1 + .Machine$double.eps) )
    }
    
    expo_estim <- est[["par"]]
  }
  
  result <- list(type = 'pl',
                 method = 'll', 
                 expo = expo_estim,
                 ll = pl_ll(dat, expo_estim, xmin),
                 xmin = xmin,
                 npars = 1)
  return(result)
}

xmin_estim <- function(dat, bounds = range(dat)) { 
  
  # Create a vector of possible values for xmin
  xmins <- sort(unique(dat))
  
  # We need at least 3 values for a pl fit, so the last value of xmin 
  # needs to have three points after it
  if ( length(xmins) <= 6 ) { 
    if ( OPTIMWARNINGS ) { 
      warning('Not enough data points to estimate xmin, returning NA')
    }
    return(NA_integer_)
  }
  
  # We build a vector of possible xmins. The last three values are stripped 
  #   away as they won't allow enough data for a fit
  xmins <- head(xmins, length(xmins)-3)
  xmins <- xmins[xmins >= min(bounds) & xmins <= max(bounds)]
  
  # Compute all kss
  kss <- adply(xmins, 1, get_ks_dist, dat = dat)[ ,2]
#   plot(kss)
  
  # Note that sometimes the fit fails, especially when xmin is around the 
  #   distribution tail -> we need to remove some NAs here
  xmin <- xmins[!is.na(kss) & kss == min(kss, na.rm = TRUE)]
  
  return(xmin)
}

get_ks_dist <- function(xmin, dat) { 
  # Crop dat to values above xmin and compute cdf
  dat <- dat[dat >= xmin]
  cdf_empirical <- sapply(dat, function(x) mean(dat >= x) )
  
  # Fit and retrieve cdf
  fit <- pl_fit(dat, xmin = xmin)
  cdf_fitted <- ppl(dat, fit[["expo"]], fit[["xmin"]])
  
#   # debug
#   plot(data.frame(dat, rbinom(length(dat), 1, .5)), type = 'n')
#   plot(log10(data.frame(dat, cdf_empirical))) 
#   points(log10(data.frame(dat, cdf_fitted)), col = 'red')
#   browser()
#   zeta.fit(dat, xmin)$exponent
#   fit$expo
  
  # We return the ks distance
  maxks <- max(abs(cdf_empirical - cdf_fitted))
#   cat(xmin, ",", max(dat), "->", maxks,  "\n" )
  return( maxks )
}

# EXP fitting 
# ---------------------------------------
# pexp/dexp is already implemented in R

# EXP: P(x = k)
ddisexp <- function(dat, rate) { 
  # sum(P = k) for k = 1 to inf
  const <- (1 - exp(-rate)) * exp(rate)
  return( const * exp(-rate * dat) )
}

# EXP: P(x>=k)
# Imported and cleaned up from powerRlaw (def_disexp.R)
pdisexp <- function(x, rate) {
  # p >= k
  p <- pexp(x + .5, rate, lower.tail = FALSE) 
  # p >= 1 
  const <-  1 - pexp(1 + .5, rate) 
  return( p/const)
}

exp_ll <- function(dat, rate) { 
  sum( log(ddisexp(dat, rate)) )
}

exp_fit <- function(dat) { 
  
  rate0 <- 1 / mean(dat)
  
  negll <- function(rate) - exp_ll(dat, rate) 
  
  if ( OPTIMWARNINGS ) { 
    est <- nlm(negll, rate0)
  } else { 
    est <- suppressWarnings( nlm(negll, rate0) )
  }
  
  result <- list(type = 'exp',
                 method = 'll', 
                 rate = est[['estimate']], 
                 ll = - est[["minimum"]],
                 npars = 1)
  return(result)
}




# LNORM fitting 
# ---------------------------------------

# LNORM: P(X=k)
ddislnorm <- function(x, meanlog, sdlog, log = FALSE) { 
  
  p_over_one <- plnorm(.5, meanlog, sdlog, lower.tail = FALSE)
   
  p_equals_k <- plnorm(x-.5, meanlog, sdlog, lower.tail = FALSE) - 
                  plnorm(x+.5, meanlog, sdlog, lower.tail = FALSE)
                  
#   cat('x =>', x, ", meanlog=", meanlog, "sdlog=", sdlog, "\n")                
#   cat('p_equals_k =>', log(p_equals_k), "\n")
#   cat('p_over_one =>', log(p_over_one), "\n")
#   cat('===>', log(p_equals_k) - log(p_over_one), "\n")
#   cat("\n")
  
  if ( !log ) { 
    return( p_equals_k / p_over_one )
  } else { 
    return( log(p_equals_k) - log(p_over_one) )
  }
}

# LNORM: P(X>=k)
pdislnorm <- function(x, meanlog, sdlog) { 
  px_supto_k <- plnorm(x - .5, meanlog, sdlog, lower.tail = FALSE)
  px_supto_one <- plnorm(.5, meanlog, sdlog, lower.tail = FALSE)
  return( px_supto_k / px_supto_one )
}

# LNORM: LL
lnorm_ll <- function(x, meanlog, sdlog) { 

#   cat('\n', 'LL:\n')
#   cat("meanlog=", meanlog, "/sdlog=", sdlog, " -> ", ddislnorm(x, meanlog, sdlog, log = TRUE), "\n\n")
  
  sum( ddislnorm(x, meanlog, sdlog, log = TRUE) ) 
}

# LNORM: fit
lnorm_fit <- function(dat) { 
  
  # Pars[1] holds mean of log-transformed data
  # Pars[2] holds sd 
  pars0 <- c( mean(log(dat)), sd(log(dat)) )
  
  negll <- function(pars) { 
    ll <- - lnorm_ll(dat, pars[1], pars[2]) 
    if ( is.finite(ll) ) ll else 1e10
  }
  if ( OPTIMWARNINGS ) { 
    est <- optim(pars0, negll, method = 'L-BFGS-B', 
                lower = c(-Inf, .Machine$double.eps)) # sd is always > 0
  } else { 
    est <- suppressWarnings( optim(pars0, negll, method = 'L-BFGS-B', 
                lower = c(-Inf, .Machine$double.eps)) ) # sd is always > 0 
  }
  
  result <- list(type = 'lnorm',
                 method = 'll', 
                 meanlog = est[['par']][1], 
                 sdlog = est[['par']][2], 
                 ll = - est[["value"]], 
                 npars = 2)
  return(result)
}




# TPL fitting 
# ---------------------------------------

tplnorm <- function(expo, rate) { 
  # Inspired from python package: normalization constant for 
  # a discrete tpl. 
  # -> https://github.com/jeffalstott/powerlaw/blob/master/powerlaw.py
  VGAM::lerch(exp(-rate), expo, 1) * exp(-rate)
}

# P(x=k)
dtpl <- function(x, expo, rate) { 
  const <- tplnorm(expo, rate)
  
  p_equals_x <- x^(-expo) * exp(- x *rate)
  
  return( p_equals_x / const )
}


# P(x>=k)
ptpl <- function(x, expo, rate) { 
  const <- tplnorm(expo, rate)
  
  # tplsum is vectorized over x
  p_inf_to_k <- tplsum(expo, rate, x) / const
  
  return( 1 - p_inf_to_k ) 
}

tpl_ll <- function(x, expo, rate) { 
  ll <- sum( log(dtpl(x, expo, rate)) )
  if ( !is.finite(ll) ) { 
    ll <- sign(ll) * .Machine$double.xmax
  }
  ll
} 

tpl_fit <- function(dat, 
                    expo0 = pl_fit(dat, method = "approx")[['expo']], 
                    rate0 = exp_fit(dat)[['rate']]) { 
  
  negll <- function(pars) { 
#     print(paste0(round(pars[1], 10), ", ", 
#                  round(pars[2], 10), " -> ", - tpl_ll(dat, pars[1], pars[2]))) 
    - tpl_ll(dat, pars[1], pars[2])
  }
  
  pars0 <- c(expo0, rate0) # rate0)
  
  lower_bounds  <- c(expo = 1, rate = 0 + .Machine$double.eps)
  upper_bounds  <- c(expo = Inf, rate = rate0)
  
  if ( OPTIMWARNINGS ) { 
    est <- optim(pars0, negll, method = 'L-BFGS-B', 
                 lower = lower_bounds, upper = upper_bounds)
  } else { 
    est <- suppressWarnings( optim(pars0, negll, method = 'L-BFGS-B', 
                                   lower = lower_bounds, upper = upper_bounds) ) # rate is always >0  )
  }
  
  result <- list(type = 'tpl',
                 method = 'll', 
                 expo = est[['par']][1], 
                 rate = est[['par']][2], 
                 ll = - est[["value"]],
                 npars = 2)
  return(result)
}


