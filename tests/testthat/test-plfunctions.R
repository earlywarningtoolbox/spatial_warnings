# 
# 
# This file contains tests to check the computation of pl-related things with 
#   xmin != 1
# 

test_that("PL computations work with xmins", { 
  
  # Setup pli from Clauzet et al's
  try(setwd('./tests'))
  for ( s in dir('./testthat/pli-R-v0.0.3-2007-07-25', 
                full.names = TRUE, pattern = '*.R') ) { 
    source(s)
  }
  
  xmax <- 1000
  expos <- c(1.1, 1.5, 2)
  for (expo in expos) { 
    for (xmin in c(1, 4, 10, 100, 500)) {
      x <- seq.int(xmax)
      
      pldat <- poweRlaw::rpldis(xmax, xmin, expo)
      pldat <- pldat[pldat < 1e5] # squeeze tail for speed
      
      # Test dpl with xmin != 1
      expect_equal(dzeta(x, xmin, expo), 
                   dpl(x, expo, xmin))
      
      # Test ppl 
      expect_equal(pzeta(x, xmin, expo, lower.tail = FALSE),
                   ppl(x, expo, xmin))
      
      # Test likelihood func
      expect_equal(zeta.loglike(pldat, xmin, expo),
                   pl_ll(pldat, expo, xmin))
      
      # Test equality of fits
      expect_equal(pl_fit(pldat, xmin = xmin)[["expo"]], 
                   zeta.fit(pldat, xmin)[["exponent"]], 
                   tol = 1e-3)
      
      # Test the estimation of xmin
      expect_is(xmin_estim(pldat), "numeric")
    }
  }
  
})

test_that("xmins estimation is coherent", { 
  
  parms <- expand.grid(expo      = seq(1, 2, by = .1), 
                       true_xmin = 1* 10^seq(0, 7, by = .5))
  xmax <- 1e3
  
  estim_xmin <- function(df) { 
    pldat <- poweRlaw::rpldis(xmax, df[ ,'true_xmin'], df[, 'expo'])
    pldat <- pldat[pldat<xmax]
    est_xmin <- xmin_estim(pldat)
    cat(df[ ,'true_xmin'], ' -> ', est_xmin, "\n", sep = "")
    data.frame(df, est_xmin = est_xmin)
  }
  
  if ( require(plyr) ) { 
    xmin_ests <- ddply(parms, ~ expo + true_xmin, estim_xmin, .progress = 'time')
  }
  
})
  
