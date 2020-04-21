# 
# 
# 

context("Test that all null model method work") 

# Here we just run the code to check that it works
test_that("All null model methods work", { 
  all_methods <- c("perm", "binom", "smooth")
  a <- generic_sews(serengeti[2:3])
  for ( m in all_methods ) { 
    b <- indictest(a, nulln = 3, null_method = m)
    expect_true(TRUE)
  }
  
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
