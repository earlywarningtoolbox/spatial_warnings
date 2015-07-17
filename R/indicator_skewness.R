#This function checks if the input is a matrix or a list of matrices and 
# then calls the main function to calculate the indicators


indicator_skewness <- function(mat, 
                               subsize = 2, 
                               detrending = FALSE, 
                               discrete = TRUE) {
  check_mat(mat)
  
  if (is.list(mat)) {
    return(lapply(mat, 
                  indicator_skewness_main, 
                  subsize, 
                  detrending, 
                  discrete))
  
  } else {
    return( indicator_skewness_main(mat, subsize, detrending, discrete) )
  }
}
  
