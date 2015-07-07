#This function checks if the input is a matrix or a list of matrices and then calls the main function to calculate the indicators


indicator_skewness<-function(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE){
  
  if(is.list(data)==TRUE){
    return(lapply(rawmatrix,function(x){indicator_skewness_main(x,subsize=2, detrending = FALSE, discrete=TRUE)}))
  
  } else {
    return(indicator_skewness_main(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE))
  }
}
  
