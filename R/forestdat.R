#' A list of binary matrices and their associated parameters 
#'
#' @format A list of two components, matrices and parameters
#' 
#'   \enumerate{ 
#'     \item The `matrices` component contains ten binary matrices which are the 
#'       matrix states at the end of the simulations
#'     \item The `parameters` component contains a data frame with the value of 
#'       the parameters used to produce the simulations. 
#'   }
#' 
#' @source Generated using the implementation of Kubo's model in caspr
#' 
#' @references
#' 
#' Kubo, T., Iwasa, Y., & Furumoto, N. (1996). Forest spatial dynamics with gap
#'   expansion: Total gap area and gap size distribution. Journal of Theoretical
#'   Biology, 180(3), 229–246. http://doi.org/10.1006/jtbi.1996.0099
"forestdat"

# 
# The data was generated using the following snippet
# 

# 
# # Generate some data from a forestgap model
# library(plyr)
# 
# # Install caspr
# library(devtools)
# # install_github('fdschneider/caspr')
# library(caspr)
# # install_github('fdschneider/spatial_warnings')
# library(spatialwarnings)
# 
# forestdat <- carray(forestgap, 
#                     init = c(.9,0), 
#                     width = 100, 
#                     parms = list(alpha = .2, delta = seq(0, .2, length.out = 10), d = 0.01))
# 
# forestdat <- llply(as.list(seq(0, .2, length.out = 10)), function(delta) { 
#                      ca(init_landscape(c('+','0'), c(1, 0), width = 100), 
#                         model  = forestgap,
#                         parms = list(alpha = .2, 
#                                      delta = delta, 
#                                      d = 0.01))
#                    }, .progress = 'time')
# 
# tmp <- lapply(forestdat, function(x) x[['landscapes']][[200]])
# tmp <- lapply(tmp, as.matrix)
# tmp <- lapply(tmp, spatialwarnings:::as.binary_matrix, state = '+')
# matrices <- tmp
# 
# # Build data.frame
# parameters <- data.frame(replicate = seq.int(10),
#                          alpha = rep(.2, 10),
#                          delta = seq(0, .2, length.out = 10),
#                          d = rep(0.01, 10))
# 
# 
# # Save data
# forestdat <- list(matrices = matrices, parameters = parameters)
# save(forestdat, file = '/home/alex/forestdat.rda')
# 
