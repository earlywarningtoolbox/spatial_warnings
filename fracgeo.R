#data input is a list of matrices of length >=1
fracgeo<-function(data){
source("~/Desktop/Spatial_warnings/FGcore.R")
  return(lapply(data,function(x){FGcore(x)}))
}
