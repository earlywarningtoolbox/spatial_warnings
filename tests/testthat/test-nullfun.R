# 
# 
# 

context("Test that all null model methods work") 

# Here we just run the code to check that it works
test_that("All null model methods work", { 
  
  
  # Check that all methods run 
  all_methods <- c("perm", "intercept", "smooth") 
  a <- generic_sews(serengeti[2:3])
  b <- patchdistr_sews(serengeti[2:3])
  c <- suppressWarnings( spectral_sews(serengeti[2:3]) )
  d <- variogram_sews(serengeti[2:3])
  null_control <- list(family = binomial())
  for ( m in all_methods ) { 
    indictest(a, nulln = 3, null_method = m, 
              null_control = null_control) 
    indictest(b, nulln = 3, null_method = m, 
              null_control = null_control) 
    indictest(c, nulln = 3, null_method = m, 
              null_control = null_control) 
    indictest(d, nulln = 3, null_method = m, 
              null_control = null_control) 
    expect_true(TRUE)
  }
  
  # Check the values returned by the null model 
  ictest <- indictest(a[[2]], 3, null_method = "intercept", 
                      null_control = null_control)
  nullmean <- mean(replicate(99, mean( ictest$get_nullmat() )))
  expect_equal(mean(ictest[["orig_data"]]), nullmean, 
               tol = 0.01)
  
  # Check the values returned by the null model 
  ictest <- indictest(a[[2]], 3, null_method = "smooth", 
                      null_control = null_control)
  nullmean <- mean(replicate(99, mean( ictest$get_nullmat() )))
  expect_equal(mean(ictest[["orig_data"]]), nullmean, 
               tol = 0.01)
  
  # Check that smoothed null model is closer to reality than the intercept model
  ictest <- indictest(a[[2]], 3, null_method = "intercept", 
                      null_control = null_control)
  error_intercept <- replicate(49, { 
    mean( abs(ictest$get_nullmat() - a[[2]][["orig_data"]]) )
  })
  ictest <- indictest(a[[2]], 3, null_method = "smooth", 
                      null_control = null_control)
                      
  error_smooth <- replicate(49, { 
    mean( abs(ictest$get_nullmat() - a[[2]][["orig_data"]]) )
  })
  expect_true({ 
    mean(error_intercept) > mean(error_smooth)
  })
  
  # Check that we warn when the null method is a function and does not return 
  # logical values when the input matrix is logical 
  nullfun <- function(mat) { mat * rnorm(prod(dim(mat))) } 
  expect_warning({ 
    indictest(compute_indicator(serengeti[[1]], raw_cg_moran), 3, 
              null_method = nullfun)
  })
  
  # Check that arguments are properly set 
  expect_warning({ 
    null_control_set_args(serengeti[[1]], list(a = "opl"), "perm")
  })
  expect_true({ 
    a <- null_control_set_args(serengeti[[1]], 
                               list(family = "binomial"), 
                               "intercept")[["family"]]
    is.binomial(a)
  }) 
  expect_true({ 
    a <- null_control_set_args(serengeti[[1]], 
                               list(family = binomial()), 
                               "intercept")[["family"]]
    is.binomial(a)
  }) 
  
  # Model option are honored
  expect_warning({
      null_control_set_args(serengeti[[1]], list(), "intercept")
  }, regexp = "using a binomial")
  expect_warning({
    null_control_set_args(coarse_grain(serengeti[[1]], 5), 
                          list(), "intercept")
  }, regexp = "using a gaussian")
  
  
  
  
})

# 
# 
# img <- serengeti[[length(serengeti)]]
# img_coarse <- coarse_grain(img, 1)
# img_tab <- data.frame(expand.grid(row = seq.int(nrow(img_coarse)), 
#                                   col = seq.int(ncol(img_coarse))), 
#                       value = as.vector(img_coarse))
# 
# 
# # Test if trend 
# mod <- mgcv::gam(value ~ s(row, col, bs = "tp"), data = img_tab, 
#                  family = binomial())
# img_tab[ ,"pred"] <- predict(mod, type = "response")
# img_tab[ ,"sim"]  <- simulate(mod, type = "response") > .5
# 
# 
# 
# plot1 <- ggplot(img_tab) + 
#   geom_raster(aes(x = col, y = row, fill = value)) + 
#   scale_fill_brewer(palette = "Spectral") + 
#   coord_fixed() + 
#   theme_minimal() + 
#   labs(x = "x", y = "y", title = "Observed matrix")
# 
# plot2 <- ggplot(img_tab) + 
#   geom_raster(aes(x = col, y = row, fill = 1 - pred)) + 
#   scale_fill_distiller(palette = "Spectral", 
#                        name = "Cover", 
#                        direction = -1) + 
#   coord_fixed() + 
#   theme_minimal() + 
#   labs(x = "x", y = "y", title = "Fitted model")
# 
# plot3 <- ggplot(img_tab) + 
#   geom_raster(aes(x = col, y = row, fill = sim)) + 
#   scale_fill_brewer(palette = "Spectral") + 
#   coord_fixed() + 
#   theme_minimal() + 
#   labs(x = "x", y = "y", title = "One null matrix")
# 
# 
# 
# library(patchwork)
# plot1 + plot2 + plot3 + 
#   plot_layout(nrow = 1)
# 
# 
# # Create a null matrix 
# library(mgcv)
# library(memoise)
# fit_model <- memoise(function(data) { 
#   mgcv::gam(value ~ s(row, col, bs = "tp"), data = data, 
#             family = binomial())
# })
# fnull <- function(mat) { 
#   mat_tab <- data.frame(expand.grid(row = seq.int(nrow(mat)), 
#                                     col = seq.int(ncol(mat))), 
#                         value = as.vector(mat))
#   mod <- fit_model(data = mat_tab) 
#   mat[ , ] <- simulate(mod) > .5 
#   return(mat)
# }
# 
# indic <- compute_indicator(serengeti, raw_moran)
# test  <- indictest(indic, null_method = fnull, nulln = 99)
# plot(test, along = serengeti.rain)
