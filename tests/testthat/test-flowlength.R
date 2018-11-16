# 
# This tests the code for the flowlength
# 
# 

context('Test the computation of flow length')

# Test raw values. rho here represents cover. We test that what is computed 
# from the code for a random matrix matches the theoretical expectations. See 
# Rodriguez et al. 2017 (Eco. Ind.)
library(plyr)
all_fls <- adply(seq(0, 1, length.out = 200), 1, function(rho) { 
  
  # Set other parameters randomly
  L <- sample.int(100, 1)
  cell_size <- 0.5
  slope <- runif(1, 0, 80) * (180/pi)
  
  # Compute flow lengths
  ds <- cell_size / cos(slope * pi/180)
  fls <- replicate(100, { 
    dat <- matrix(runif(L), ncol = 1, nrow = L) < rho
    raw_flowlength_planar(dat, cell_size = cell_size, slope = slope)
  })
  
  mean_fl <- mean(fls)
  var_fl <- var(fls)
  # Theoretical values
  E_fl <- ds * (1 - rho)*(rho * L - (1 - rho)*(1 - (1 - rho)^L)) / (rho^2*L)
  E_fl2 <- ds^2*( (1 - rho)*(rho^2*(1 - rho)*(L+1)^2 + rho^2*L - 6 * (1 - rho) + 
                      (1 - rho)^(L+1)*(rho^2*(2*L^2 - 1) + 6 * rho * L + 6)) ) / 
                      (rho^4 * L^2)
  V_fl <- E_fl2 - E_fl^2
  
  data.frame(rho = rho, 
             obsmean = mean_fl, 
             obsvar  = var_fl, 
             themean = E_fl, 
             thevar  = V_fl, 
             themax  = ds*(L+1)/2)
}, .id = NULL)

# Obs. mean and Theoretical mean should be equal 
expect_true( with(all_fls, t.test(x = obsmean, y = themean)$p.value) > 0.5 )
expect_true( with(all_fls, t.test(x = obsvar, y = thevar)$p.value) > 0.5 )


# library(ggplot2)
# library(tidyr)
# 
# ggplot( gather(subset(all_fls), var, value, obsmean, themean) ) + 
#   geom_line(aes(x = rho, y = value, color = var))
#   
# ggplot( gather(subset(all_fls), var, value, obsvar, thevar) ) + 
#   geom_line(aes(x = rho, y = value, color = var))
# 
# ggplot( subset(all_fls) ) + 
#   geom_point(aes(x = obsmean, y = themean)) + 
#   geom_abline(slope = 1, intercept = 0)
# 
# ggplot( subset(all_fls)) + 
#   geom_point(aes(x = obsvar, y = thevar)) + 
#   geom_abline(slope = 1, intercept = 0)

# Test that full/empty columns have correct values
zero_column <- matrix(0, ncol = 1, nrow = 10) > 0
ds <- 1 / cos(20*pi/180)
expect <- ds * (10+1)/2
expect_true({
    abs(expect - raw_flowlength_planar(zero_column, slope = 20, cell_size = 1)) < 1e-10
  })
expect_true({ 
  abs(0 - raw_flowlength_planar(!zero_column, slope = 20, cell_size = 1)) < 1e-10
})


# Test that we reproduce published results (Rodriguez et al. 2017)
datadir <- './tests/testthat/rodriguez2018/rawdata/'

rodriguez <- lapply(dir(datadir, full.names = TRUE), function(n) { 
  as.matrix(read.table(n)) > 0
})


compute_deviation <- function(mat, slope, cell_size) { 
  fl <- raw_flowlength_planar(mat, slope, cell_size)
  
  cover <- mean(mat)
  n <- nrow(mat)
  p_slope <- cell_size / cos(slope * pi/180)
  
  E_FL_r <- (1-cover)*(cover*n-(1-cover)*(1-(1-cover)^n))*p_slope / (cover*cover*n) 
  E_fl <- ds * (1 - rho)*(rho * L - (1 - rho)*(1 - (1 - rho)^L)) / (rho^2*L)

  return( (fl - E_FL_r) / E_FL_r )
  
}

raw <- lapply(rodriguez, raw_flowlength_planar, slope = .6, cell_size = 0.223)
values <- lapply(rodriguez, compute_deviation, slope = .6, cell_size = 0.223)
covers <- lapply(rodriguez, mean)
data.frame(values = unlist(values), covers = unlist(covers))

# 
# b	cover	diffFLrdm_	diffFLagg
# 0,55	0,2	0,651	0,175
# 0,57	0,3	0,384	0,05
