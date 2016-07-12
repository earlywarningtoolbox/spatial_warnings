# 
# 
# This code tests the psd-fitting functions
# 

library(poweRlaw)
library(plyr)
library(ggplot2)


# 
# # # Test correct computation of zeta function
# zeta.loc <- "./tests/testthat/zetafun/zetafun"
# system( paste0("gcc -o zetafun ", 
#                paste0(zeta.loc, ".c"), 
#                " $(gsl-config --cflags --libs)"))
# 
# s <- 2.4
# q <- 1
# system(paste(zeta.loc, as.character(s), as.character(q)))
# 
# result <- apply(seq(1.1, 30, length.out = 100), 1, 
#       function(x) { 
#         q <- sample.int(100, 1)
#         s <- runif(1, 1.01, 10)
#         pli_value <- system(paste(zeta.loc, as.character(s), as.character(q)), intern = TRUE)
#         pli_value <- as.numeric(pli_value)
#         data.frame(vgam = VGAM::zeta(s) - ifelse(q==1, 0, sum(seq.int(q-1)^-s)),
#                    pli = pli_value, 
#                    q = q, s = s)
#         })
# # 
# ggplot(result) + geom_point(aes(x = vgam, y = pli))
# # VGAM::zeta gives pli_value + 1 (it assumes q = 0)

# r_value <- zetafun(q, s)

N <- 10000



# PL -> bof
pl_psd <- rpldis(N, 1, 2)
a <- fitpsd(pl_psd)
plot_distr(a)
indictest(a)
predict(a)

# Okay, wtf is going on here: do a proper test using base pli functions
tplfit  <- discpowerexp.fit(pl_psd, threshold = 1)
plfit <- zeta.fit(pl_psd, threshold = 1) 
power.powerexp.lrt(plfit, tplfit) # 0 if tpl better

# LN -> no problem
ln_psd <- round(rlnorm(N, 2, 1))
ln_psd <- ln_psd[ln_psd>0]
a <- fitpsd(ln_psd)
plot_distr(a)
indictest(a)

# exp -> bof
exp_psd <- round(rexp(N, 1.2))
exp_psd <- exp_psd[exp_psd > 0]
a <- fitpsd(exp_psd)
plot_distr(a)
indictest(a)

# tpl
tpl_psd <- round(rpowerexp(N, threshold = 1, exponent = 1.3, rate = 1.1))
tplfit <- discpowerexp.fit(tpl_psd, threshold = 1)

x0 <- seq(min(tpl_psd), max(tpl_psd), by = .1)
cumpsd <- function(x) sapply(x0, function(a) sum(x > a))
plot(log10(x0), log10(cumpsd(tpl_psd)))
lines(log10(x0), ddiscpowerexp(x0, threshold = 1, exponent = 1.3, 
                               rate = 1.1, log = TRUE))

a <- fitpsd(tpl_psd)
plot_distr(a)
indictest(a)      abline(b = pl_fit(pldat)[['expo']], a = 0, col = 'red')





context('Test the fitting of distributions')

# Setup pli from Clauzet et al's
try(setwd('./tests'))
sapply(dir('./testthat/pli-R-v0.0.3-2007-07-25', full.names = TRUE, 
           pattern = '*.R'), source)

visual <- FALSE

test_that('PL fitting works', { 
  expos <- c(1.001, 1.1, 1.5, 2, 10, 50)
  xmaxs <- c(1, 10, 100, 1000)
  for ( expo in expos ) { 
    for ( xmax in xmaxs ) { 
      
      print(paste(expo, "/", xmax))
      dat <- seq.int(xmax)
      pldat <- poweRlaw::rpldis(1000, xmin = 1, alpha = 1.3)
      # Squeeze tail a bit
      pldat <- pldat[pldat < 1e9]
      
      # left: spw <-> right: pli
      # ppl <-> dzeta
      expect_equal(dzeta(dat,, exponent = expo), 
                   ppl(dat, expo))
      
      # ppl <-> pzeta with higher tail
      expect_equal(pzeta(dat, exponent = expo, lower.tail = FALSE), 
                   ppl(dat, expo))
      
      # ppl_ll <-> zeta.loglike
      expect_equal(zeta.loglike(dat, exponent = expo), 
                   ppl_ll(dat, expo))
      
      # pl_fit <-> zeta.fit 
      pl_expo <- pl_fit(pldat)[['expo']]
      expect_equal(zeta.fit(pldat)[['exponent']], pl_expo, tol = 1e-3)
      
      # Look at fit
      plot(log10(cumpsd(pldat)))
      xs <- seq(min(pldat), max(pldat), length.out = 10)
      lines(log10(xs), log10(ppl(xs, pl_expo)), col = 'red')
      title('PLFIT')
    }
  }
  
})


test_that('EXP fitting works', { 
  expos <- c(1.001, 1.01, 1.1, 1.5, 2)
  xmaxs <- c(1, 10, 100, 1000)
  for ( expo in expos ) { 
    for ( xmax in xmaxs ) { 
      
      print(paste(expo, "/", xmax))
      dat <- seq.int(xmax)
      expdat <- round(rexp(1000, rate = expo))
      
      expect_equal(exp_fit(expdat)[["expo"]],
                   exp.fit(expdat)[["rate"]])
      
      # Look at fit
      plot(log10(cumpsd(expdat)))
      xs <- seq(min(expdat), max(expdat), length.out = 100)
      lines(log10(xs), 
            log10(pdisexp(xs, pl_expo)), 
            col = 'red')
      title('EXPFIT')
    }
  }
  
})


