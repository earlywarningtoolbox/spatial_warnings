# 
# 
# This file contains tests to check the computation of pl-related things with 
#   xmin != 1
# 

test_that("PL estimations work with xmins", { 
  
  # Setup pli from Clauzet et al's
  try(setwd('./tests/testthat'), silent = TRUE)
          
  for ( s in dir('./pli-R-v0.0.3-2007-07-25', 
                full.names = TRUE, pattern = '*.R') ) { 
    source(s)
  }

  # Compile auxiliary binaries
  system("cd ./pli-R-v0.0.3-2007-07-25/zeta-function/ && make")
  system("cd ./pli-R-v0.0.3-2007-07-25/exponential-integral/ && make")
  system("cd ./pli-R-v0.0.3-2007-07-25/ && \
            gcc -lm discpowerexp.c -o discpowerexp && \
            chmod +x discpowerexp")
  
  expos <- c(1.5, 2)
  for (expo in expos) { 
    for (xmin in c(1, 10, 40)) {
      x <- seq.int(1000)
      
      pldat <- poweRlaw::rpldis(1000, xmin, expo)
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
