# 
# 
# This code tests the psd-fitting functions
# 

library(poweRlaw)
library(plyr)
library(ggplot2)

context('Test the fitting of distributions')

# Change dir if running tests manually
if ( file.exists('./tests/testthat') ) { 
  library(testthat)
  setwd('./tests/testthat') 
}

# Setup pli from Clauzet et al's
for ( s in dir('./pli-R-v0.0.3-2007-07-25', 
              full.names = TRUE, pattern = '*.R') ) { 
  source(s)
}

# Compile auxiliary binaries
system("cd ./pli-R-v0.0.3-2007-07-25/zeta-function/ && make")
system("cd ./pli-R-v0.0.3-2007-07-25/exponential-integral/ && make")
system("cd ./pli-R-v0.0.3-2007-07-25/ && \
          gcc discpowerexp.c -lm -o discpowerexp && \
          chmod +x discpowerexp")

visual <- FALSE

test_that('PL fitting works', { 
  
  expos <- c(1.2, 1.5, 2.5)
  xmins <- c(1,  10, 100)
  for ( expo in expos ) { 
    for ( xmin in xmins ) { 
      
      dat <- unique(round(seq.int(1, 1000, length.out = 100)))
      pldat <- poweRlaw::rpldis(1000, xmin = xmin, alpha = expo)
      # Squeeze tail a bit for speed
      pldat <- pldat[pldat < 1e5]
      
      # left: spw <-> right: pli
      # dpl <-> dzeta
      expect_equal(dzeta(dat,, exponent = expo, threshold = xmin), 
                   dpl(dat, expo, xmin = xmin))
      
      # ppl <-> pzeta with higher tail
      expect_equal(pzeta(dat, exponent = expo, threshold = xmin, lower.tail = FALSE), 
                   ppl(dat, expo, xmin = xmin))
      
      # ppl_ll <-> zeta.loglike
      expect_equal(zeta.loglike(pldat, exponent = expo, threshold = xmin), 
                   pl_ll(pldat, expo, xmin = xmin))
      
      # pl_fit <-> zeta.fit 
      our_expo <- pl_fit(pldat, xmin = xmin)[['expo']]
      clauset_expo <- zeta.fit(pldat, threshold = xmin)[['exponent']]
      powerlaw_expo <- estimate_pars( poweRlaw::displ$new(pldat) )[["pars"]]
      expect_equal(clauset_expo, our_expo, tol = 1e-3)
      
      
      # Look at fit
      plot(log10(cumpsd(pldat[pldat>=xmin])))
      xs <- c(min(pldat[pldat>=xmin]), max(pldat[pldat>=xmin]))
      lines(log10(xs), log10(ppl(xs, clauset_expo, xmin = xmin)), 
            lwd = 2, col = 'blue')
      lines(log10(xs), log10(ppl(xs, powerlaw_expo, xmin = xmin)), 
            lwd = 2, col = 'green')
      lines(log10(xs), log10(ppl(xs, our_expo, xmin = xmin)), col = 'red')
      title('PLFIT')
    }
  }
  
})

test_that('EXP fitting works', { 
  
  rates <- c(1.1, 1.5)  
  xmins <- c(1, 2, 3)
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
        
        # Look at fit
        plot(log10(cumpsd(expdat[expdat >= xmin])))
        xs <- seq(min(expdat), max(expdat), length.out = 100)
        lines(log10(xs), 
              log10(pdisexp(xs, fit[["rate"]], xmin = xmin)), 
              col = 'red')
        title('EXPFIT')
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
        
        xmax <- 1000
        dat <- seq.int(xmax)
        lnormdat <- ceiling(rlnorm(xmax, meanlog, sdlog))
#         cat('meanlog: ', meanlog, ', sdlog: ', sdlog, ', xmin: ', xmin, "\n")
        
        # Test distr functions
        expect_equal(ddislnorm(dat, meanlog, sdlog, xmin),
                     dlnorm.tail.disc(dat, meanlog, sdlog, threshold = xmin))
        
        # plnorm.tail.disc returns negative values (?!)
#         plnorm.tail.disc(dat, meanlog, sdlog, threshold = xmin)
#         pdislnorm(dat, meanlog, sdlog, xmin)
        
        # Note that dlnorm.disc assumes xmin = 0, so we cannot test it 
        #   with variable xmin. 
        our_dlnorm <- ddislnorm(dat, meanlog, sdlog, xmin = 0)
        clauset_dlnorm <- dlnorm.disc(dat, meanlog, sdlog)
        expect_equal(our_dlnorm, 
                     clauset_dlnorm)
        
        # Test obtained fits
        our_fit <- suppressWarnings( lnorm_fit(lnormdat, xmin = xmin) )
        clauset_fit <- suppressWarnings( fit.lnorm.disc(lnormdat, threshold = xmin) )
#         expect_equal(our_fit[['meanlog']], clauset_fit[['meanlog']], tol = 1e-2)
#         expect_equal(our_fit[['sdlog']], clauset_fit[['sdlog']], tol = 1e-2)
        
        # Look at fit
        plot(log10(cumpsd(lnormdat[lnormdat >= xmin])))
        xs <- seq(min(lnormdat), max(lnormdat), length.out = 10000)
        suppressWarnings( 
          lines(log10(xs), 
                log10(pdislnorm(xs, 
                                our_fit[['meanlog']], 
                                our_fit[['sdlog']], 
                                xmin = xmin)), col = 'red')
        )
        suppressWarnings(
          lines(log10(xs), 
                log10(pdislnorm(xs, 
                                fit.lnorm.disc(lnormdat, threshold = xmin)[["meanlog"]], 
                                fit.lnorm.disc(lnormdat, threshold = xmin)[["sdlog"]],
                                xmin = xmin)), 
                                col = "blue")
        )
        title('LNORMFIT')
        
      }
    }
  }
  
})

# For TPL xmax must not be too low (>3?)
test_that('TPL fitting works', { 
  
  rates <- c(0.1, .5, 1.1, 1.3)
  expos <- c(1.1, 1.3, 1.5)
  xmins <- c(1, 2, 4) 
  for (xmin in xmins) { 
    for ( rate in rates ) { 
      for ( expo in expos ) { 
        
        tpldat <- round(rpowerexp(1000, 1, expo, rate))
        tpldat <- tpldat[tpldat < 1e5]
        dat <- seq(0, 1000)
        
        # Normalizing coeff
        # Here, we check that the binary called by the Clauset code actually 
        # returns something. 
        clauset_result <- suppressWarnings( discpowerexp.norm(xmin, expo, rate) )
#         if (length(clauset_result) == 0) stop()
        if (length(clauset_result) > 0) { 
          expect_equal(clauset_result, 
                       tplnorm(expo, rate, xmin))
        }
        
        # P(X=x)
        # Note: we explicitely convert output from pli to numeric as if 
        # the returned values are all NAs then R thinks it's logical 
        dtpl_result <- dtpl(dat, expo, rate, xmin)
        ddpxp_result <- suppressWarnings ( 
            as.numeric( ddiscpowerexp(dat, expo, rate, threshold = xmin) )
          )
        if ( ! all( is.na(dtpl_result) ) ) { 
          expect_equal(dtpl_result, ddpxp_result)
        }
        
        # Here, we check that the binary called by the Clauset code actually 
        # returns something before performing the test. 
        clauset_result <- suppressWarnings( 
            discpowerexp.loglike(tpldat, expo, rate, threshold = xmin)
          )
        if (length(clauset_result) > 0) { 
          expect_equal(tpl_ll(tpldat, expo, rate, xmin),
                       clauset_result)
        }
        
        our_fit <- tpl_fit(tpldat, xmin = xmin)
        clauset_fit <- discpowerexp.fit(tpldat, threshold = xmin) 
        
        if ( length(clauset_fit) > 0) { 
          if ( ! (our_fit$ll < clauset_fit$loglike) ) { 
            expect_equal(our_fit$expo, clauset_fit$exponent, tol = 5e-3)
            expect_equal(our_fit$rate, clauset_fit$rate, tol = 5e-3)
          }
        }
        
        # LL profile
#         if ( abs(our_fit$expo - clauset_fit$exponent) > 5e-2 ) { 
#           
#           llexpos  <- seq(min(our_fit$expo, clauset_fit$exponent) - .1, 
#                           max(our_fit$expo, clauset_fit$exponent) + .1, 
#                           length.out = 100)
#           llrates  <- seq(min(our_fit$rate, clauset_fit$rate) - .1, 
#                           max(our_fit$rate, clauset_fit$rate) + .1, 
#                           length.out = 100)
#           
#           llgrid <- ddply(expand.grid(llexpo = llexpos, 
#                             llrate = llrates), ~ llexpo + llrate, 
#                           with, data.frame(negll = - tpl_ll(tpldat, llexpo, 
#                                                             llrate, xmin)), 
#                           , .progress = "none")
#           
#           ggplot(llgrid, aes(x = llexpo, y = llrate)) + 
#             geom_raster(aes(fill = negll))  + 
# #             geom_tile(dat = subset(llgrid, negll < quantile(negll, .05)), 
# #                         alpha = .2, color = "white")  + 
#             geom_contour(aes(z = negll, color = negll), binwidth = 1) + 
#             annotate(geom = "point", x = our_fit$expo, y = our_fit$rate, color = "red") + 
#             annotate(geom = "point", 
#                      x = clauset_fit$expo, 
#                      y = clauset_fit$rate, color = "blue") 
#             
#         }
        
        # Look at fit
        plot(log10(cumpsd(tpldat[tpldat >= xmin])))
        xs <- unique( round( seq(min(tpldat), max(tpldat), length.out = 100) )) 
        lines(log10(xs), 
                log10(ptpl(xs, 
                          clauset_fit[["exponent"]], 
                          clauset_fit[["rate"]], xmin)), col = 'blue', lwd = 2)
        lines(log10(xs), 
                log10(ptpl(xs, our_fit[["expo"]], our_fit[["rate"]], xmin)), col = 'red')
        title('TPLFIT')
        
      }
    }
  }
  
})


test_that("PL estimations work with xmins", { 
  
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

# Remove auxiliary binaries now that tests are done
system("cd ./pli-R-v0.0.3-2007-07-25/zeta-function/ && rm zeta_func zeta_func.o")
system("cd ./pli-R-v0.0.3-2007-07-25/exponential-integral/ && rm exp_int exp_int.o")
system("cd ./pli-R-v0.0.3-2007-07-25/ && rm discpowerexp")

