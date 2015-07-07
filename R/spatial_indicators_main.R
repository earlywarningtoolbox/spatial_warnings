#data input is a list of matrices


spatial_indicators <- function(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE){
  
  if(is.list(data)==TRUE){
		return(lapply(rawmatrix,function(x){
			      spatial_ews_main(x,subsize=2, detrending = FALSE, discrete=TRUE)
}))
	}else{
		return(spatial_ews_main(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE))
	}
}

