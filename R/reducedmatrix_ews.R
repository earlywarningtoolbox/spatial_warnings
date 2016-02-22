# Function to reduce original matrix to submatrix by averaging
# This function is used bt the generic indicators variance, skewness and moran 
# correlation. It divides the square matrix into submatrices of mean cover.
# 
# Written by Vishwesha Guttal, 7th Nov 2013.
# Modified by Sabiha Majumder
# Converted to c++ by Alex
# 
#'@export
reducedmatrix_ews <- function(mat, subsize) { 
  warning('reducedmatrix_ews will be deleted in a future version of ',
          'spatialwarnings and should not be used ', 
          'please use coarse_grain() instead')
  coarse_grain(mat, subsize)
}

# coarse_grain <- reducedmatrix_ews <- function(mat, subsize) {
#   
#   N <- dim(mat)[1]
#   n <- floor(N/subsize)
#   
#   submatrix <- matrix(nrow=subsize, ncol=subsize);
#   reddata <- matrix(nrow=n, ncol=n);
#   
#   for (i in 1:n) {
#     for (j in 1:n) {
#       submatrix = mat[((i-1)*subsize+1):(i*subsize),
#                       ((j-1)*subsize+1):(j*subsize)];
#       reddata[i,j] = mean(as.vector(submatrix));
#     }
#   }
#   
#   return(reddata)
# }
