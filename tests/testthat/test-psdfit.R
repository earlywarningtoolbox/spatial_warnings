# 
# 
# This code tests the psd-fitting functions
# 

library(poweRlaw)
library(plyr)
library(ggplot2)

context('Test the fitting of distributions')

# Setup pli from Clauzet et al's
#   try(setwd('./tests/testthat'), 
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

visual <- FALSE

test_that('PL fitting works', { 
  expos <- c(1.1, 1.5, 2)
  xmins <- c(2, 1,  10, 100)
  for ( expo in expos ) { 
    for ( xmin in xmins ) { 
      stop()
      dat <- seq.int(1000)
      pldat <- poweRlaw::rpldis(1000, xmin = xmin, alpha = expo)
      # Squeeze tail a bit for speed
      pldat <- pldat[pldat < 1e6]
      
      # left: spw <-> right: pli
      # dpl <-> dzeta
      expect_equal(dzeta(dat,, exponent = expo, threshold = xmin), 
                   dpl(dat, expo, xmin = xmin))
      
      # ppl <-> pzeta with higher tail
        expect_equal(pzeta(dat, exponent = expo, threshold = xmin, lower.tail = FALSE), 
                     ppl(dat, expo, xmin = xmin))
      
      # ppl_ll <-> zeta.loglike
      expect_equal(zeta.loglike(dat, exponent = expo, threshold = xmin), 
                   pl_ll(dat, expo, xmin = xmin))
      
      # pl_fit <-> zeta.fit 
      pl_expo <- pl_fit(pldat)[['expo']]
      expect_equal(zeta.fit(pldat)[['exponent']], pl_expo, tol = 1e-3)
      
      # Redo fit and look at it
      pl_expo <- pl_fit(pldat)[['expo']]
      plot(log10(cumpsd(pldat[pldat>=xmin])))
      xs <- c(min(pldat[pldat>=xmin]), max(pldat[pldat>=xmin]))
      lines(log10(xs), log10(ppl(xs, pl_expo, xmin = xmin)), col = 'red')
      title('PLFIT')
    }
  }
})

test_that('EXP fitting works', { 
  rates <- c(1.1, 1.5, 2)
  xmins <- c(1, 2, 4)
  for (xmin in xmins) { 
    for ( rate in rates ) { 
        dat <- seq.int(1000)
        expdat <- ceiling(rexp(1000, rate = rate))
        
        expect_equal(ddiscexp(dat, rate, threshold = xmin),
                     ddisexp(dat, rate, xmin = xmin))
        
        pdisexp(dat, rate, xmin = xmin)
        
        fit <- exp_fit(expdat, xmin = xmin)
        expect_equal(fit[['rate']],
                     discexp.fit(expdat, threshold = xmin)[["lambda"]], tol = 1e-3)
        
#         # Look at fit
#         plot(log10(cumpsd(expdat)))
#         xs <- seq(min(expdat), max(expdat), length.out = 100)
#         lines(log10(xs), log10(pdisexp(xs, fit[["rate"]], xmin = xmin)), col = 'red')
#         title('EXPFIT')
    }
  }
  
})

test_that('LNORM fitting works', { 
  
  meanlogs <- c(1, 3, 10)
  sdlogs <- c(1, 2, 3)
  xmins <- c(1, 10, 30)
  for (xmin in xmins) { 
    for ( meanlog in meanlogs ) { 
      for ( sdlog in sdlogs ) { 
#       stop()
        xmax <- 1000
        dat <- seq.int(xmax)
        lnormdat <- ceiling(rlnorm(xmax, meanlog, sdlog))
        
        # Test distr functions
        expect_equal(ddislnorm(dat, meanlog, sdlog, xmin),
                     dlnorm.tail.disc(dat, meanlog, sdlog, threshold = xmin))
        
#         plnorm.tail.disc(dat, meanlog, sdlog, threshold = xmin)
#         pdislnorm(dat, meanlog, sdlog, xmin)
        
        # Test ll function
        expect_equal(lnorm.tail.disc.loglike(lnormdat, meanlog, sdlog, xmin),
                     lnorm_ll(lnormdat, meanlog, sdlog, xmin))
        
        lnorm_fit(lnormdat, xmin = xmin)
        fit.lnorm.disc(lnormdat, threshold = xmin)
        
        # Look at fit
  #         plot(log10(cumpsd(lnormdat)))
  #         xs <- seq(min(lnormdat), max(lnormdat), length.out = 100)
  #         lines(log10(xs), 
  #               log10(pdislnorm(xs, fit[['meanlog']], fit[['sdlog']])), col = 'red')
  #         lines(log10(xs), 
  #               log10(pdislnorm(xs, fit.lnorm.disc(lnormdat, threshold = 1)[["meanlog"]], 
  #                               fit.lnorm.disc(lnormdat, threshold = 1)[["sdlog"]])), 
  #                               col = "blue")
  #         title('LNORMFIT')
      }
    }
  }
  
})

# For TPL xmax must not be too low (>3?)
test_that('TPL fitting works', { 
  
  rates <- c(1.1, 1.3)
  expos <- c(1.1, 1.3)
  xmins <- c(1, 2, 4) 
  for (xmin in xmins) { 
    for ( rate in rates ) { 
      for ( expo in expos ) { 
        
        tpldat <- round(rpowerexp(10000, 1, expo, rate))
        dat <- seq.int(max(tpldat))
        
        # Normalizing coeff
        expect_equal(discpowerexp.norm(xmin, expo, rate), 
                     tplnorm(expo, rate, xmin))
        
        # P(X=x)
        # Note: we explicitely convert output from pli to numeric as if 
        # the returned values are all NAs then R thinks it's logical 
        expect_equal(dtpl(dat, expo, rate, xmin),
                     as.numeric( ddiscpowerexp(dat, expo, rate, threshold = xmin) ))
        
        expect_equal(tpl_ll(tpldat, expo, rate, xmin),
                     discpowerexp.loglike(tpldat, expo, rate, threshold = xmin))
        
        fit <- tpl_fit(tpldat, xmin)
        # We skip the other codebase test as they produce errors all the time
        #  (gsl underflow)
        
  #       fit2 <- discpowerexp.fit(tpldat, threshold = 1)
        
  #       expect_equal(fit$expo, fit2$exponent, tol = 1e3)
  #       expect_equal(fit$ll, fit2$loglike, tol = 1e3)
        
  #         # Look at fit
  #         plot(log10(cumpsd(tpldat)))
  #         xs <- seq(min(tpldat), max(tpldat), length.out = 100)
  #         points(log10(xs), 
  #               log10(ptpl(xs, fit[["expo"]], fit[["rate"]])), col = 'red')
  #         title('TPLFIT')
      }
    }
  }
  
})

