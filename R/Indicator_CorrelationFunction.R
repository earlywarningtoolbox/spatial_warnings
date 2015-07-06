#MAIN function to calculate correlation function
## dependencies include corrfunc.R (included in the repository)
#data input is a list of matrices of length >=1 or a single matrix
CorrF<-function(data){
  if (sum(is.list(data))==0){
    return(corrfunc(data))
  } 
  else{return(lapply(data,function(x){corrfunc(x)}))
  }
}
