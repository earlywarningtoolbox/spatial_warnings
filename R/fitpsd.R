# 
# 
# This file contains code that fits psds
# 


# Notes on psd fitting
# 
# 
# We want to fit four types of distributions using MLE: PL/TPL/LNORM/EXP
#   with an xmin of 1 (no xmin basically), except for PL for which xmin can vary





zeta_w_xmin2 <- function(expo, xmin = 1) { 
  VGAM::zeta(expo) - sum_all_one_over_k(from = 1, to = xmin, expo)
}

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
    ans <- -expo * log(x) - log(const)
  }
  
  return(ans)
}

# PL: P(x>=k) 
ppl <- function(x, expo, xmin = 1) { 
  const <- VGAM::zeta(expo)
  
  # Adjust constant for threshold (note that this has no effect if xmin == 1, 
  #   as expected)
  const <- const - sum_all_one_over_k(from = 1, to = xmin, expo)
  
#   ans <- sapply(x, function(x) zeta_w_xmin(expo, x)/const)
  ans <- zeta_w_xmin(expo, x) / const 
  
  # Values below threshold are NA'ed
  ans[x < xmin] <- NA
  
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
    est <- nlm(negll, expo_estim)
    expo_estim <- est[["estimate"]]
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
  
  # We build a vector of possible xmins. The last three values are stripped 
  #   away as they won't allow enough data for a fit
  xmins <- sort(unique(dat))
  xmins <- head(xmins, length(xmins)-3)
  xmins <- xmins[xmins >= min(bounds) & xmins <= max(bounds)]
  
  if ( length(xmins) <= 2 ) { 
    warning('Not enough data points to estimate xmin, returnin NA')
    return(NA_integer_)
  }
  
  # Compute empirical cdf of data
  cdf <- sapply(dat, function(x) mean(dat >= x) )
  
  kss <- adply(xmins, 1, get_ks_dist, 
               dat = dat, cdf = cdf, .progress = 'time')[ ,2]
  
  # Note that sometimes the fit fails, especially when xmin is around the 
  #   distribution tail -> we need to remove some NAs here
  xmin <- xmins[!is.na(kss) & kss == min(kss, na.rm = TRUE)]
  
  return(xmin)
}

get_ks_dist <- function(xmin, dat, cdf) { 
  
  # Crop cdf and dat to values above xmin
  cdf_empirical <- cdf[dat >= xmin]
  dat <- dat[dat >= xmin]
  # Fit and retrieve cdf
  fit <- pl_fit(dat, xmin = xmin)
  cdf_fitted <- ppl(dat, fit[["expo"]], fit[["xmin"]])
  
  # We return the ks distance
  return( max(abs(cdf_empirical - cdf_fitted)) )
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
  est <- nlm(negll, rate0)
  
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
  
  est <- optim(pars0, negll, method = 'L-BFGS-B', 
               lower = c(-Inf, .Machine$double.eps)) # sd is always > 0
  
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

# P(x=k)
dtpl <- function(x, expo, rate) { 
  const <- tplsum(expo, rate, from = 1, to = 1e6)
#   C = ( float(exp(self.xmin * self.Lambda) /
#             lerchphi(exp(-self.Lambda), self.alpha, self.xmin)) )
  
  p_equals_x <- x^(-expo) * exp(- x *rate)
  return( p_equals_x / const )
}


# P(x>=k)
ptpl <- function(x, expo, rate) { 
  
  const <- tplsum(expo, rate, from = 1, to = 1e6)
  
  p_inf_to_k <- sapply(x, function(k) { 
    tplsum(expo, rate, from = 1, to = ceiling(k-1)) / const 
    })
  
#   p_sup_to_1 <- tplsum(expo, rate, from = 1, to = 1)
  
  return( 1 - p_inf_to_k ) 
  
}

tpl_ll <- function(x, expo, rate) { 
  sum( log(dtpl(x, expo, rate)) )
} 

tpl_fit <- function(dat) { 
  
  pars0 <- c( pl_fit(dat)[['expo']], exp_fit(dat)[['rate']] )
  
  negll <- function(pars) - tpl_ll(dat, pars[1], pars[2])
  rss   <- function(pars) sum( (dat - ptpl(dat, pars[1], pars[2]))^2 )
  
  est <- constrOptim(pars0, negll, 
                     grad = NULL,
                     ui = matrix(c(1, 0, 0, 1), ncol = 2), 
                     ci = c(-1, 0))
  
  result <- list(type = 'tpl',
                 method = 'll', 
                 expo = est[['par']][1], 
                 rate = est[['par']][2], 
                 ll = - est[["value"]],
                 npars = 2)
  return(result)
}


