#data input is a list of matrices of length >=1
SpecMain<-function(data){

  if (sum(is.list(data))==0){
    return(rspec_ews(data))
  } 
  else{return(lapply(data,function(x){rspec_ews(x)}))
    }
}
