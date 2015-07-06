#data input is a list of matrices of length >=1
CorrF<-function(data){
  #source("~/Desktop/Spatial_warnings/rspec_ews.R")
  if (sum(is.list(data))==0){
    return(corrfunc(data))
  } 
  else{return(lapply(data,function(x){corrfunc(x)}))
  }
}
