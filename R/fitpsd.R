# 
# 
# This file contains code that fits psds
# 


# Notes on psd fitting
# 
# 
# We want to fit four types of distributions using MLE: PL/TPL/LNORM/EXP
#   with an xmin of 1 (no xmin basically). 





# PL fitting 
# ---------------------------------------


# Riemann zeta function with xmin taken into account :
# sum(1/k^-expo for i=xmin to i = inf
zeta_w_xmin <- function(expo, xmin = 0) { 
  VGAM::zeta(expo) - sum_all_one_over_k_before(xmin, expo)
}

# PL: P(x=k)
dpl <- function(x, expo) { 
  const <- VGAM::zeta(expo)
  return( (1/const) * x^(-expo) )
}

# PL: P(x>=k) with threshold = 1
ppl <- function(x, expo) { 
  const <- VGAM::zeta(expo)
  sapply(x, function(x) zeta_w_xmin(expo, x)/const)
}

# PL: Log likelihood
pl_ll <- function(dat, expo) { 
  sum( log(dpl(dat, expo)) ) 
}

# PL: Fit by MLE
pl_fit <- function(dat) { 
  
  # Start with the approximation given in Clauset's paper
  expo0 <- 1+ length(dat) * (1 / sum(log(dat/(1-.5))))
  
  negll <- function(expo) { - pl_ll(dat, expo) }
  est <- nlm(negll, expo0)
  
  result <- list(type = 'pl',
                 method = 'll', 
                 expo = est[['estimate']], 
                 ll = - est[['minimum']],
                 npars = 1)
  return(result)
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

# Memoized version of tplsum
mtplsum <- memoise::memoize(tplsum)

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


