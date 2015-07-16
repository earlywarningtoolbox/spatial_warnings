# 
# 
# Tests for patch-labelling functions.
# 

context('Patch-labelling functions')

test_that('Labelling works', { 
  
  exmat <- matrix(c(0,0,1,0,0,0,1,0,
                    0,1,0,0,1,0,0,0,
                    0,0,1,0,1,0,0,0,
                    0,0,0,0,0,0,0,0,
                    1,0,0,1,1,1,0,0,
                    0,0,0,0,0,0,0,1,
                    0,0,0,0,0,0,0,0,
                    0,0,1,0,0,0,0,0), 
                   nrow=8,byrow=TRUE)
  
  nbmask4 <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
  nbmask8 <- matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)

#   as_df <- function(mat) {
#     data.frame(expand.grid(x = seq.int(nrow(mat)), 
#                            y = seq.int(ncol(mat))),
#                            state = as.vector(mat))
#   }
#   
#   qplot(x, y, fill = state, geom = 'raster', 
#         data = as_df(exmat), main = 'Original matrix')
#   
#   lblmat <- label(exmat, nbmask8, wrap=FALSE)
#   
#   dev.new()
#   qplot(x,y, fill = as.factor(state), geom = 'raster', 
#         data = as_df(lblmat), main = 'Labelled matrix')
  
  expect_equal(label(exmat, nbmask4, wrap=TRUE),
               structure(c(NA, NA, NA, NA, 6L, NA, NA, NA, 
                           NA, 3L, NA, NA, NA, NA, NA, NA, 
                           1L, NA, 5L, NA, NA, NA, NA, 1L, 
                           NA, NA, NA, NA, 7L, NA, NA, NA, 
                           NA, 4L, 4L, NA, 7L, NA, NA, NA, 
                           NA, NA, NA, NA, 7L, NA, NA, NA, 
                           2L, NA, NA, NA, NA, NA, NA, NA, 
                           NA, NA, NA, NA, NA, 8L, NA, NA), 
               .Dim = c(8L, 8L)))
               
  expect_equal(label(exmat, nbmask8, wrap=TRUE),  
               structure(c(NA, NA, NA, NA, 4L, NA, NA, NA, 
                           NA, 1L, NA, NA, NA, NA, NA, NA, 
                           1L, NA, 1L, NA, NA, NA, NA, 1L, 
                           NA, NA, NA, NA, 5L, NA, NA, NA, 
                           NA, 3L, 3L, NA, 5L, NA, NA, NA, 
                           NA, NA, NA, NA, 5L, NA, NA, NA, 
                           2L, NA, NA, NA, NA, NA, NA, NA, 
                           NA, NA, NA, NA, NA, 4L, NA, NA), 
               .Dim = c(8L, 8L)))
  
  expect_equal(label(exmat, nbmask8, wrap=FALSE),  
               structure(c(NA, NA, NA, NA, 4L, NA, NA, NA, 
                           NA, 1L, NA, NA, NA, NA, NA, NA, 
                           1L, NA, 1L, NA, NA, NA, NA, 7L, 
                           NA, NA, NA, NA, 5L, NA, NA, NA, 
                           NA, 3L, 3L, NA, 5L, NA, NA, NA, 
                           NA, NA, NA, NA, 5L, NA, NA, NA, 
                           2L, NA, NA, NA, NA, NA, NA, NA, 
                           NA, NA, NA, NA, NA, 6L, NA, NA), 
                         .Dim = c(8L, 8L)))
  
})