#MAIN function to calculate fractal geometries. Also calculates patch sizes (same as patch area).
#Dependencies include FGcore.R , lbl.R (both included in the repository)

#data input is a list of matrices of length >=1 or a single matrix
fracgeo<-function(data){
  if (sum(is.list(data))==0){
    return(FGcore(data))
  } 
  else{return(lapply(data,function(x){FGcore(x)}))
    }
}
