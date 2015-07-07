#data input is a list of matrices


spatial_indicators <- function(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE){

  source("~/Caspr_try/generic_spatial_indicators.R")
  source("~/Caspr_try/morancorrelation_ews.R")
  source("~/Caspr_try/reducedmatrix_ews.R")
  
  if (is.list(data)==TRUE){
    return(lapply(rawmatrix,function(x){
		  spatial_ews_main(x,subsize=2, detrending = FALSE, discrete=TRUE)
}
  )
  )
  }else{
    return(spatial_ews_main(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE))
  }
  
}
