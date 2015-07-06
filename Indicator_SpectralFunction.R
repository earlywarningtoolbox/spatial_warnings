#MAIN Function to calculate theta and r spectrum.
#Dependencies include myshiftfft_ews.R , rpec_ews.R (both included in the repository)

#data input is a list of matrices of length >=1 or a single matrix
SpecMain<-function(data){
  if (sum(is.list(data))==0){
    return(rspec_ews(data))
  } 
  else{return(lapply(data,function(x){rspec_ews(x)}))
    }
}
