#' @title Spatial warning indicators: Power Spectrum
#' @description MAIN Function to calculate theta and r spectrum.
#' @details Dependencies include myshiftfft_ews.R , rpec_ews.R (both included in the repository)

#' @param data A list of binary matrices of length >=1 (OR) a single binary matrix
#' @return a list of lists(of theta and R spectrum values for distances 1 to SystemSize/2 for each matrix in the list) if input is a list (OR) a single list (of theta and R spectrum values for distances 1 to SystemSize/2) if input is a single matrix
SpecMain<-function(data){
  if (sum(is.list(data))==0){
    return(rspec_ews(data))
  } 
  else{return(lapply(data,function(x){rspec_ews(x)}))
    }
}
