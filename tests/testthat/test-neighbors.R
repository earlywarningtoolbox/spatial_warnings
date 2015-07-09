
context('Test the neighbor-finding functions') 


test_that('neighbors are fetched correctly', { 
  
  
  size <- 50 # hardcoded in tests ! 
  exmat <- matrix(seq.int(size^2), nrow=size)
  X <- c(3,6)
  
  
  nbmask4 <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
  nbmask8 <- matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)
  
  
  expect_equal(get_nb_coords(exmat, c(3,6), nbmask4, wrap = FALSE),
               structure(c(2, 3, 3, 4, 
                           6, 5, 7, 6), 
                         .Dim = c(4L, 2L), 
                         .Dimnames = list(NULL,c('x','y'))))
  
  expect_equal(get_nb_coords(exmat, c(3,6), nbmask8, wrap = FALSE),
               structure(c(2, 2, 2, 3, 3, 4, 4, 4, 
                           5, 6, 7, 5, 7, 5, 6, 7), 
                         .Dim = c(8L, 2L), 
                         .Dimnames = list(NULL, c("x", "y"))))

  expect_equal(get_nb_coords(exmat, c(1,6), nbmask4, wrap = FALSE),
               structure(c(1, 1, 2, 
                           5, 7, 6), 
                         .Dim = c(3L, 2L), 
                         .Dimnames = list(NULL, c("x", "y"))))
  
  expect_equal(get_nb_coords(exmat, c(1,6), nbmask4, wrap = TRUE),
               structure(c(50, 1, 1, 2, 
                           6,  5, 7, 6), 
                         .Dim = c(4L, 2L), 
                         .Dimnames = list(NULL, c("x", "y"))))
  
  expect_equal(get_nb_coords(exmat, c(1,6), nbmask8, wrap = TRUE),
               structure(c(50, 50, 50, 1, 1, 2, 2, 2, 
                           5,   6,  7, 5, 7, 5, 6, 7), 
                         .Dim = c(8L, 2L), 
                         .Dimnames = list(NULL, c("x", "y"))))
  
  # Bad neighbors mask
  expect_error(get_nb_coords(exmat, c(1,6), 
                             cbind(nbmask4,nbmask8), wrap = TRUE))
  
#   # Visualise (deactivated)
#   if (FALSE) { 
#     as_df <- function(mat) {
#       data.frame(expand.grid(x = seq.int(nrow(mat)), 
#                              y = seq.int(ncol(mat))), 
#                              state = as.vector(mat))
#     }
#     
#     exmat_df <- as_df(exmat)
#     library(ggplot2)
#     result <- get_nb_coords(exmat, c(3,6), nbmask4, wrap = FALSE)
# 
#     ggplot(exmat_df) + 
#       geom_raster(aes(x=x,y=y,fill=state)) + 
#       geom_point(aes(x=x,y=y),pch='+',size=5, data=as.data.frame(result))
#     

})