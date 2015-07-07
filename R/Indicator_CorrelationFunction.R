#' @title Spatial Warning Indicators: Correlation Function
#' @description MAIN function to calculate correlation function
#' @details dependencies include corrfunc.R (included in the repository)
 
#' @param data is a list of binary matrices of length >=1 (OR) a single binary matrix
#' @return A list of lists (of correlation at different distances, from 0 to SystemSize/2 with increments of 1 unit) for every matrix of input list (OR) a single vector (of correlation at different distances from 0 to SystemSize/2 with increments of 1 unit) if input is a single matrix

CorrF<-function(data){
  if (sum(is.list(data))==0){
    return(corrfunc(data))
  } 
  else{return(lapply(data,function(x){corrfunc(x)}))
  }
}
