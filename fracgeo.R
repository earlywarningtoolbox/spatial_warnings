#data input is a list of matrices of length >=1
fracgeo<-function(data){
  if (sum(is.list(data))==0){
    return(FGcore(data))
  } 
  else{return(lapply(data,function(x){FGcore(x)}))
    }
}
