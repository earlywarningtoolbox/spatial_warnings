# 
# This file contains code related to patch size distribution fitting. These 
#   functions can fit Power-law (pl), Truncated Power-law (tpl), Lognormal 
#   (lnorm) and Exponential (exp) distributions using maximum likelihood, as 
#   per Clauset et al. 's recommendation. 
# 
# In addition, it provides the estimation of xmin using the ks-distance for 
# power-laws

# Optimisation global options
ITERLIM <- 10000
GRADTOL <- 1e-10 

# This is a safe version of nlm that returns a sensible result (NA) when 
# the algorithm fails to converge. This can happen quite often when looking 
# for pathological cases (e.g. fitting distribution from few points in the 
# tails, etc.). 
# This 
optim_safe <- function(f, pars0) { 
  
  # Wrap a neg ll objective function so that it does not return NAs
  safe <- function(f) { 
    function(pars) { 
      ans <- f(pars) 
      if ( is.na(ans) ) { 
        return(1e15) 
      } else { 
        return(ans)
      }
    }
  }
  
  # Try to do first a quick SANN optimisation to find a good first 
  # approximation and get out of a possible initial local minimum (happens 
  # with lnorm fitting). 
  # 
  # Note that in some pathological cases the fit fails (not enough points, etc.)
  # This happens a lot when finding xmin as we end up fitting on very few 
  # points in the tail of the distribution. Here, we report the fit failed but 
  # do not stop execution. In most (all?) of those cases it has no 
  # consequences on the results. 
  result <- try( { 
    sann_approx <- optim(pars0, safe(f), 
                         method = "SANN", 
                         control = list(maxit = 100)) 
    
    # Now do a Newton method to find the (hopefully global) minimum. 
    result <- nlm(safe(f), sann_approx[['par']], 
                  iterlim = ITERLIM, gradtol = GRADTOL)
  
  }, silent = TRUE)
  
  if ( class(result) == "try-error" ) { 
    warning('Optimization failed to converge, returning NA. Error is:\n', 
            result)
    return( list(minimum = NA_real_, 
                 estimate = NA_real_) )
  } else { 
    # Code results above 3 highlight a true problem, below 3 the solution 
    # is either exact or approximate. 
    if ( result[["code"]] > 3 ) { 
      warning(paste0('nlm returned ', result[["code"]]))
    }
    return(result)
  }
}

# Bounds on parameters, these should be large and no observed distribution should 
# have values beyond them
# Power-laws lambdas
PLMIN <- 1 
PLMAX <- 20
# Exponential rates
EXPMIN <- .Machine$double.eps # A very close value to, but not, zero
EXPMAX <- 20
# Bounds for truncated power-laws
TPL_EXPOMIN <- -1 # Taken from Clauset's code
TPL_EXPOMAX <- 20
TPL_RATEMIN <- 0 # Taken from Clauset's code
TPL_RATEMAX <- 20

# These functions are useful when doing the fit to rescale the values to 
# a bounded range. This allows using unbounded optimization methods (sann, nlm, 
# etc.) but still have bounded parameters. 
to_rescaled   <- function(x, min, max) VGAM::extlogit(x, min, max, inverse = TRUE)
from_rescaled <- function(x, min, max) VGAM::extlogit(x, min, max)

# Riemann zeta function with xmin taken into account :
# sum( 1/k^-expo ) for i=xmin to i = inf
# This is vectorized over xmins so that we do not sum things several times. 
zeta_w_xmin <- function(expo, xmins) { 
  perm <- order(xmins)
  xmins <- xmins[order(xmins)]
  
  # Compute zeta value
  zetaval <- VGAM::zeta(expo)
  
  # Initialize
  output <- rep(NA_real_, length(xmins))
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
    ans[x < xmin] <- NA_real_
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
  
  ps <- zeta_w_xmin(expo, x[!is_below_xmin]) / const 
  
  # Values below threshold are NA'ed
  ans <- NA_real_*x
  ans[!is_below_xmin] <- ps
  
  return(ans)
}

# PL: Log likelihood
pl_ll <- function(dat, expo, xmin) { 
  sum( dpl(dat, expo, xmin, log = TRUE) )  
}

# PL: Fit by MLE
pl_fit <- function(dat, xmin = 1) { 
  
  # Cut data to specified range
  dat <- dat[dat >= xmin] 
  
  # Start with the approximation given in Clauset's 
  npts <- length(dat)
  expo_estim <- 1 + npts / (sum(log(dat)) - npts*log(xmin-.5))
  
  negll <- function(expo) {
    result <- - pl_ll(dat, to_rescaled(expo, PLMIN, PLMAX), xmin) 
    if ( is.infinite(result) ) { 
      return(NA_real_)
    } else { 
      return(result)
    }
  }
  
  est <- optim_safe(negll, from_rescaled(expo_estim, PLMIN, PLMAX))
  
  expo_estim <- to_rescaled(est[["estimate"]], PLMIN, PLMAX)
  
  result <- list(type = 'pl',
                 method = 'll', 
                 expo = expo_estim,
                 ll = - est[['minimum']],
                 xmin = xmin,
                 npars = 1)
  return(result)
}

#' @title Estimate the minimum patch size of a power-law distribution 
#' 
#' @description When fitting a power-law to a discrete distribution, it might 
#'   be worth discarding points below a certain threshold (xmin) to improve 
#'   the fit. This function estimates the optimal xmin based on the 
#'   Kolmogorov-Smirnoff distance between the fit and the empirical 
#'   distribution, as suggested by Clauset et al. (2009). 
#' 
#' @param dat A vector of integer values
#' 
#' @param bounds A bounds 
#' 
#' @return The estimated xmin as an integer value 
#' 
#' @details The function returns NA if \code{dat} has only three unique values 
#'   or if the power-law fit failed. 
#' 
#' @seealso \code{\link{patchdistr_spews}}, \code{\link{patchsizes}}, 
#'   \code{\link{indicator_psdtype}}
#' 
#' @references 
#' 
#' Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). 
#'   Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
#' 
#' @examples 
#' 
#' \dontrun{ 
#' psd <- patchsizes(forestgap[[5]])
#' xmin_estim(psd)
#' }
#'@export
xmin_estim <- function(dat, bounds = range(dat)) { 
  
  # Create a vector of possible values for xmin
  xmins <- sort(unique(dat))
  
  # We need at least 3 values for a pl fit, so the last value of xmin 
  # needs to have three points after it
  if ( length(xmins) <= 3 ) { 
    warning('Not enough data points to estimate xmin, returning NA')
    return(NA_integer_)
  }
  
  # We build a vector of possible xmins. The last three values are stripped 
  #   away as they won't allow enough data for a fit
  xmins <- head(xmins, length(xmins)-3)
  xmins <- xmins[xmins >= min(bounds) & xmins <= max(bounds)]
  
  # Compute all ks-distances
  kss <- adply(xmins, 1, get_ks_dist, dat = dat)[ ,2]
  
  if ( all(is.nan(kss)) ) { 
    return(NA_integer_)
  }
  
  # Note that sometimes the fit fails, especially when xmin is around the 
  #   distribution tail -> we need to remove some NAs here
  xmin <- xmins[!is.na(kss) & kss == min(kss, na.rm = TRUE)]
  
  # Note that xmin can be NaN
  return(xmin)
}

get_ks_dist <- function(xmin, dat) { 
  # Crop dat to values above xmin and compute cdf
  dat <- dat[dat >= xmin]
  cdf_empirical <- sapply(dat, function(x) mean(dat >= x) )
  
  # Fit and retrieve cdf
  fit <- pl_fit(dat, xmin = xmin)
  
  if ( is.na(fit[['expo']]) ) { 
    # Note: a warning was already produced in this case as it means that the 
    # fit failed to converge: we do not produce one here again. 
    return(NA_real_)
  }
  
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
ddisexp <- function(dat, rate, xmin = 1, log = FALSE) { 
  # sum(P = k) for k = 1 to inf
  if ( log ) { 
    
    const <- log(1 - exp(-rate)) + rate * xmin
    return( ifelse(dat < xmin, NA_real_, const - rate * dat) )
  
  } else { 
    
    const <- (1 - exp(-rate)) * exp(rate*xmin) 
    return( ifelse(dat < xmin, NA_real_, const * exp(-rate * dat)) )
    
  }
} 

# EXP: P(x>=k)
# Imported and cleaned up from powerRlaw (def_disexp.R)
pdisexp <- function(x, rate, xmin) {
  # p >= k
  p <- pexp(x + .5, rate, lower.tail = FALSE) 
  # p >= 1 
  const <-  1 - pexp(xmin + .5, rate) 
  return(p/const)
}

exp_ll <- function(dat, rate, xmin) { 
  sum( ddisexp(dat, rate, xmin, log = TRUE)) 
}

exp_fit <- function(dat, xmin = 1) { 
  
  dat <- dat[dat>=xmin]
  
  rate0 <- 1 / mean(dat)
  
  negll <- function(rate) {
    - exp_ll(dat, to_rescaled(rate, EXPMIN, EXPMAX), xmin)
  }
  
  est <- optim_safe(negll, from_rescaled(rate0, EXPMIN, EXPMAX))
  
  result <- list(type = 'exp',
                 method = 'll', 
                 rate = to_rescaled(est[['estimate']], EXPMIN, EXPMAX), 
                 ll = - est[["minimum"]],
                 npars = 1)
  return(result)
}





# LNORM fitting 
# ---------------------------------------

# LNORM: P(X=k)
ddislnorm <- function(x, meanlog, sdlog, xmin, log = FALSE) { 
  
  p_over_thresh <- plnorm(xmin - .5, meanlog, sdlog, lower.tail = FALSE)
   
  p_equals_k <- plnorm(x-.5, meanlog, sdlog, lower.tail = FALSE) - 
                  plnorm(x+.5, meanlog, sdlog, lower.tail = FALSE)
  
  if ( !log ) { 
    return( ifelse(x<xmin, NA_real_, p_equals_k / p_over_thresh) )
  } else { 
    return( ifelse(x<xmin, NA_real_, log(p_equals_k) - log(p_over_thresh)) )
  }
}

# LNORM: P(X>=k)
pdislnorm <- function(x, meanlog, sdlog, xmin) { 
  px_supto_k <- plnorm(x - .5, meanlog, sdlog, lower.tail = FALSE)
  px_supto_xmin <- plnorm(xmin - .5, meanlog, sdlog, lower.tail = FALSE)
  ifelse(x<xmin, NA_real_, px_supto_k / px_supto_xmin)
}

# LNORM: LL
lnorm_ll <- function(x, meanlog, sdlog, xmin) { 
  x <- x[x>=xmin]
  sum( ddislnorm(x, meanlog, sdlog, xmin, log = TRUE) ) 
}

# LNORM: fit
lnorm_fit <- function(dat, xmin = 1) { 
  
  # Pars[1] holds mean of log-transformed data
  # Pars[2] holds sd 
  pars0 <- c( mean(log(dat)), sd(log(dat)) )
  
  negll <- function(pars) { 
    ll <- - lnorm_ll(dat, pars[1], pars[2], xmin) 
    if ( is.finite(ll) ) ll else 1e10
  }
  
  est <- optim_safe(negll, pars0)
  
  result <- list(type = 'lnorm',
                 method = 'll', 
                 meanlog = est[['estimate']][1], 
                 sdlog = est[['estimate']][2], 
                 ll = - est[["minimum"]], 
                 npars = 2)
  return(result)
}




# TPL fitting 
# ---------------------------------------

tplnorm <- function(expo, rate, xmin) { 
  # Inspired from python package: normalization constant for 
  # a discrete tpl. 
  # -> https://github.com/jeffalstott/powerlaw/blob/master/powerlaw.py
  if ( is.infinite( exp(-rate) ) ) { 
    return(NA_real_)
  } else { 
    VGAM::lerch(exp(-rate), expo, xmin) * exp(-xmin * rate)
  }
}

# P(x=k)
dtpl <- function(x, expo, rate, xmin) { 
  const <- tplnorm(expo, rate, xmin)
  p_equals_x <- x^(-expo) * exp(- x *rate)
  
  return( ifelse(x < xmin, NA_real_, p_equals_x / const) )
}


# P(x>=k)
ptpl <- function(x, expo, rate, xmin) { 
  const <- tplnorm(expo, rate, xmin)
  
  # tplsum is vectorized over x
  p_inf_to_k <- tplsum(expo, rate, x, xmin) / const
  
  return( 1 - p_inf_to_k ) 
}

tpl_ll <- function(x, expo, rate, xmin) { 
  x <- x[x>=xmin]
  ll <- sum( log(dtpl(x, expo, rate, xmin)) )
  if ( !is.finite(ll) ) { 
    ll <- sign(ll) * .Machine$double.xmax
  }
  return( ll )
} 

tpl_fit <- function(dat, xmin = 1) { 
  
  negll <- function(pars) { 
    - tpl_ll(dat, 
             to_rescaled(pars[1], TPL_EXPOMIN, TPL_EXPOMAX), 
             to_rescaled(pars[2], TPL_RATEMIN, TPL_RATEMAX), xmin)
  }
  
  # Initialize and find minimum
  expo0 <- pl_fit(dat, xmin)[['expo']] 
  rate0 <- exp_fit(dat, xmin)[['rate']]
  pars0 <- c(from_rescaled(expo0, TPL_EXPOMIN, TPL_EXPOMAX), 
             from_rescaled(rate0, TPL_RATEMIN, TPL_RATEMAX))
  
  est <- optim_safe(negll, pars0)
  
  result <- list(type = 'tpl',
                 method = 'll', 
                 expo = to_rescaled(est[['estimate']][1], TPL_EXPOMIN, TPL_EXPOMAX), 
                 rate = to_rescaled(est[['estimate']][2], TPL_RATEMIN, TPL_RATEMAX), 
                 ll = - est[["minimum"]],
                 npars = 2)
  return(result)
}


