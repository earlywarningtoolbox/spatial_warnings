##  Generic spatial early warning signals : moran correlation
## Code originally written by V. Guttal and modified by S. Majumder


#' Description: Spatial Early Warning Signal : Moran Correlation 
#'
#' \code{indicator_moranCor_main} wrapper function used to estimate statistical properties from spatial data and compare results to null models
#'
# Arguments:
#'    @param rawmatrix is the input matrix.It should be a square matrix and contain either one or multiple snapshots of spatial data. If the input is a list of matrices, 
#'    the function should be called as lapply. Otherwise if the input is one matrix with one snapshot after the other in the next row, the function can be called in a regular way.  
#'    @param discrete logical. If TRUE the data represent discrete variables (like presence/absense), otherwise continuous data (like biomass density). Default is FALSE.
#'    @param subsize is the dimension of the submatrix used to subsample the rawmatrix either to reduce the size of the original matrix
#'    @param detrending logical. If TRUE data are detrended by removing the spatial mean. (Default is FALSE)

# Returns:
#'   @return \code{spatial_ews} returns a matrix that contains:
#'   @return \item{stats}{the mean, moran correlation of both the original and the reduced data.}
#'   @return \item{cor_null}{the Moran correlation at lag 1 estimated on the null model based on the original data.}
#'   @return \item{corr_null_red}{the autocorrelation at lag 1 estimated on the reduced null model based on the subsampled data.}
  





indicator_moranCor_main = function(rawmatrix, subsize=2, detrending = FALSE, discrete=TRUE)
{
  require("moments")
  require("plotrix")
  require("fields")
  
  source("~/Caspr_try/morancorrelation_ews.R")
  source("~/Caspr_try/reducedmatrix_ews.R")
  
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
  corr_data = numeric(numsnaps)
  corr_red_data = numeric(numsnaps)
  
  corr_null =numeric(numsnaps) 
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
    #Indicator for non-reduced data.
    mean_data[snaps] =  mean(as.vector(detrenddata)) 
    corr_data[snaps] = morancorrelation_ews(detrenddata) 
    
    if (discrete==TRUE){
      #Obtain Reduced Data.
      red_detrend_data = reducedmatrix_ews(detrenddata,subsize) 
    }
    
    #Calculate indicators for the reduced data.
    corr_red_data[snaps] = morancorrelation_ews(red_detrend_data) 
    
    
    # NULL MODELS
    
    #Create the random matrix.
    
    randomdata = matrix(sample(detrenddata),N,N) 
    
    red_randdata = reducedmatrix_ews(randomdata,subsize) 
    
    # Estimate correlation of NON reduced data
    corr_null[snaps]=morancorrelation_ews(randomdata)   
    
    
    # Estimate correlation of reduced data
    corr_null_red[snaps]=morancorrelation_ews(red_randdata) 
    
  }   
  # Output
  out1<-data.frame(mean_data, corr_data, corr_red_data)
  colnames(out1)<-c("mean","corr_full","corr_red")
  out2=data.frame(corr_null)
  out21=data.frame(corr_null_red)
  
  
  out=list("stats" = out1, "CORR_null" = out2,  "CORR_null_red" = out21)
  
  return(out) 
  
}	



