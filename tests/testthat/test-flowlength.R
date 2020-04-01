# 
# This tests the code for the flowlength
# 
# 

context('Test the computation of flow length')

if ( exists('EXTENDED_TESTS') && EXTENDED_TESTS ) { 
  
  # Test raw values. rho here represents cover. We test that what is computed 
  # from the code for a random matrix matches the theoretical expectations. See 
  # Rodriguez et al. 2017 (Eco. Ind.)
  library(plyr)

  all_fls <- adply(seq(0.01, 1, length.out = 100), 1, function(rho) { 
    
    # Set other parameters randomly
    L <- 100
    cols <- 1 # Relationships hold for a column vector only (especially variance)
    cell_size <- 0.5
    slope <- runif(1, 10, 20) 
    
    # Compute flow lengths
    ds <- cell_size / cos(slope * pi/180)
    fls <- matrixn(19999, { 
      dat <- matrix(runif(L*cols), ncol = cols, nrow = L) < rho
      raw_flowlength_uniform(dat, cell_size = cell_size, slope = slope)
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
  }, .progress = "none", .id = NULL)

  # Obs. mean and Theoretical mean should be equal 
  expect_true( with(all_fls, t.test(x = obsmean, y = themean)$p.value) > 0.5 )
  expect_true( with(all_fls, t.test(x = obsvar, y = thevar)$p.value) > 0.5 )

  # Obs mean and theoretical mean should be very close
  with( subset(all_fls, themean > 0 & thevar > 0), { 
    plot(themean, abs(obsmean - themean) / themean )
    expect_true( all( abs(obsmean - themean) / themean < 0.02) )
    expect_true( all( abs(obsvar - thevar) / thevar < 0.1) )
  })

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
      abs(expect - raw_flowlength_uniform(zero_column, slope = 20, cell_size = 1)) < 1e-10
    })
  expect_true({ 
    abs(0 - raw_flowlength_uniform(!zero_column, slope = 20, cell_size = 1)) < 1e-10
  })

    
  # Test that we reproduce results from images. Note that we base these 
  # tests on images that have been already binarized in matlab/octave
  # 
  imagedir <- './rodriguez2018/images/'
  all_images <- lapply(dir(imagedir, full.names = TRUE, pattern = "*.csv"), 
                      function(i) { 
                        a <- as.matrix(read.csv(i, header = FALSE)) > 0 
                        dimnames(a) <- NULL
                        a
                        })
  fls <- flowlength_sews(all_images, slope = 0.6, cell_size = 0.223)
  ref <- data.frame(covers = c(0.42, 0.37, 0.17, 0.12, 0.19, 0.13, 
                              0.48, 0.38, 0.34, 0.218, 0.114, 0.08), 
                    fl = c(1.1231, 1.2624, 5.1928, 9.0719, 4.6266, 8.6047, 
                          1.5144, 2.5934, 3.3909, 4.945, 11.631, 15.6279))

  fl.df <- as.data.frame(fls)

  compare <- data.frame(fl.df, ref)
  expect_true({ 
      with(compare, all( abs(value - fl) < 0.01))
    })

}

