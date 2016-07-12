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

# PL: P(x>=k)
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
                 ll = dpl_ll(dat, est[['estimate']]))
  return(result)
}





# EXP fitting 
# ---------------------------------------

# pexp/dexp is already implemented in R
exp_ll <- function(dat, expo) { 
  sum(dexp(dat, expo, log = TRUE))
}

# EXP: P(x>=k)
# Imported and cleaned up from powerRlaw (def_disexp.R)
pdisexp <- function(x, expo) {
  p <- pexp(x + .5, expo, lower.tail = FALSE) 
  const <-  1 - pexp(1 + .5, expo) 
  p <- p / const
  return(p)
}

pexp_xmin <- function(x, expo, shift, ...) { 
  pexp(x-shift, expo, ...)
}

exp_fit <- function(dat) { 
  
  expo0 <- 1 / mean(dat)
  
  negll <- function(expo) - exp_ll(dat, expo)
  est <- nlm(negll, expo0)
  
  result <- list(type = 'exp',
                 method = 'll', 
                 expo = est[['estimate']], 
                 ll = exp_ll(dat, est[['estimate']]))
  return(result)
}




