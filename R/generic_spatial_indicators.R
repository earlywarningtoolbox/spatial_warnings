##  Generic spatial early warning signals 
## Code originally written by V. Guttal, V. Dakos and S. Kefi and modified by S. Majumder


spatial_ews = function(rawmatrix, subsize=2, detrending = FALSE)
{
  require("moments")
  require("plotrix")
  require("fields")
  
  source("~/Caspr/morancorrelation_ews.R")
  source("~/Caspr/reducedmatrix_ews.R")
  
  rawmatrix=as.matrix(rawmatrix)
  
  allmatrices = rawmatrix
  nrows = dim(allmatrices)[1]
  ncols = dim(allmatrices)[2]
  
  N=dim(allmatrices)[2] 
  numsnaps=1
  
  if ( nrows != ncols) 
  {
    N = ncols
    numsnaps = nrows/ncols
    if (nrows %% ncols != 0)
    {
      stop("If the matrix has only one spatial snapshot, it should be a square matrix. \n
           If the matrix has more than one spatial snapshot, then numrows must be equal to num of snapshots x numcols.\n
           Please check for consistency.")
    }
    }
  
  mean_data = numeric(numsnaps)  
  var_data = numeric(numsnaps)
  skew_data = numeric(numsnaps)
  corr_data = numeric(numsnaps)
  var_red_data = numeric(numsnaps)
  skew_red_data = numeric(numsnaps)
  corr_red_data = numeric(numsnaps)
  
  var_null = numeric(numsnaps) 
  sk_null = numeric(numsnaps) 
  corr_null =numeric(numsnaps) 
  var_null_red = numeric(numsnaps) 
  sk_null_red = numeric(numsnaps) 
  corr_null_red = numeric(numsnaps) 
  
  
  for (snaps in 1:numsnaps)
  {
    rowstart <- (snaps-1)*N+1
    rowend <- snaps*ncols
    fullmatrix <- allmatrices[rowstart:rowend, 1:ncols]     
    
    
    if (detrending==TRUE){
      #Remove the spatial mean.  
      detrenddata = fullmatrix - mean(as.vector(fullmatrix)) 
    }
    else 
      detrenddata = fullmatrix 
    
    #Compute indicators.
    #Indicators for non-reduced data.
    mean_data[snaps] =  mean(as.vector(detrenddata)) 
    var_data[snaps] = sd(as.vector(detrenddata))^2 
    skew_data[snaps] = skewness(as.vector(detrenddata)) 
    corr_data[snaps] = morancorrelation_ews(detrenddata) 
    
    #Obtain Reduced Data.
    red_detrend_data = reducedmatrix_ews(detrenddata,subsize) 
    
    #Calculate indicators for the reduced data.
    var_red_data[snaps] = sd(as.vector(red_detrend_data))^2 
    skew_red_data[snaps] = skewness(as.vector(red_detrend_data)) 
    corr_red_data[snaps] = morancorrelation_ews(red_detrend_data) 
    
    
    # NULL MODELS

      #Create the random matrix.

      randomdata = matrix(sample(detrenddata),N,N) 
      
      red_randdata = reducedmatrix_ews(randomdata,subsize) 
      
      # Estimate SD SK CORR TSPECT RSPECT of NON reduced data
      var_null[snaps]=sd(as.vector(randomdata))^2 
      sk_null[snaps]=skewness(as.vector(randomdata)) 
      corr_null[snaps]=morancorrelation_ews(randomdata) 	
      
      
      # Estimate SD SK CORR of reduced data
      var_null_red[snaps]=sd(as.vector(red_randdata))^2 
      sk_null_red[snaps]=skewness(as.vector(red_randdata)) 
      corr_null_red[snaps]=morancorrelation_ews(red_randdata) 
  }   
    # Output
    out1<-data.frame(mean_data, var_data, skew_data, corr_data, var_red_data, skew_red_data, corr_red_data)
    colnames(out1)<-c("mean","var_full","sk_full","corr_full","var_red","sk_red","corr_red")
    out2=data.frame(var_null)
    out3=data.frame(sk_null)
    out4=data.frame(corr_null)
    out21=data.frame(var_null_red)
    out31=data.frame(sk_null_red)
    out41=data.frame(corr_null_red)
   
  
  out=list("stats" = out1, "var_null" = out2, "SK_null" = out3, "CORR_null" = out4, "SD_null_red" = out21, "SK_null_red" = out31, "CORR_null_red" = out41)
  
  return(out)  
}	




