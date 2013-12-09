#' Description: Spatial Early Warning Signals
#'
#' \code{spatial_ews} wrapper function used to estimate statistical properties from spatial data and compare results to null models
#'
# Details:
#' see ref below
#'
# Arguments:
#'    @param rawmatrix is the input matrix.It should be a square matrix and contain either one or multiple snapshots of spatial data.
#'    @param numsnaps is the number of snapshots in the rawmatrix (DEFAULT = numrow/numcols)
#'    @param discrete logical. If TRUE the data represent discrete variables (like presence/absense), otherwise continuous data (like biomass density). Default is FALSE.
#'    @param subsize is the dimension of the submatrix used to subsample the rawmatrix either to reduce the size of the original matrix to  (DEFAULT value = 5)
#'    @param nullmodel generates a null distribution of spatial statistics. Value 0 means the null model is a reshuffled matrix of original data. Value of 1 means a random matrix with Gaussian distribution of same mean and variance as original data. (DEFAULT is 0)
#'    @param iter is the number of iterations that the null model runs. (DEFAULT is 50)
#'    @param detrending logical. If TRUE data are detrended by removing the spatial mean. (Default is FALSE)
#'    @param logtransform logical. If TRUE data are logtransformed prior to analysis as log(X+1). (Default is FALSE)
#' 
#' 
# Returns:
#'   @return \code{spatial_ews} returns a matrix that contains:
#'   @return \item{stats}{the mean, std deviation, skewness and Moran correlation at lag 1 of both the original and the subsampled data.}
#'   @return \item{thetaspectr}{the theta spectrum of the original data.}
#'   @return \item{rhospectr}{the rho spectrum of the original data.}
#'   @return \item{SD_null}{the std deviatins estimated on the null model based on the original data.}
#'   @return \item{SK_null}{the skewness estimated on the null model based on the original data.}
#'   @return \item{CORR_null}{the Moran correlation at lag 1 estimated on the null model based on the original data.}
#'   @return \item{SD_null_red}{the std deviations estimated on the null model based on the subsampled data.}
#'   @return \item{SK_null_red}{the skewness estimated on the null model based on the subsampled data.}
#'   @return \item{CORR_null_red}{the Moran correlation at lag 1 estimated on the null model based on the subsampled data.}
#'   
#' In addition, \code{spatial_ews} returns for each snapshot a plot with the original data, the 2dim DFT on the original data, the theta and rho spectra, the patch size distribution (if the data are discrete), and a table of the spatial statistics compared to the 5 and 95 percentiles estimated from the chosen null model. 
#' If the input rawmatrix is of multiple snapshots, two extra plots are returned for visualising the trends of the spatial indicators for the original and reduced data.
#'  
#' @export
#' 
#' @author Vishewesha Guttal, Sonia Kefi, Vasilis Dakos \email{vasilis.dakos@@gmail.com}
#' @references Kefi S. et al (2013). Early warning signals of ecological transitions: Methods for spatial patterns. PLoS One (in review)
#' @seealso \code{\link{generic_ews}}  \code{\link{ddjnonparam_ews}}  \code{\link{bdstest_ews}}  \code{\link{sensitivity_ews}}  \code{\link{surrogates_ews}}  \code{\link{ch_ews}}  \code{\link{movpotential_ews}}  \code{\link{livpotential_ews}}  
#'
#' @importFrom moments skewness
#'
#' @examples 
#' data(desertification_BW)
#' out=spatial_ews(desertification_BW, numsnaps=1, discrete=FALSE, subsize=5, nullmodel=0, iter = 50, detrending = FALSE, logtransform = FALSE)
#' @keywords early-warning

spatial_ews = function(rawmatrix, numsnaps=1, discrete=FALSE, subsize=5, nullmodel=0, iter = 50, detrending = FALSE, logtransform = FALSE)
{
  require("moments")
  require("plotrix")
  require("fields")
  
  source("./morancorrelation_ews.R")
  source("./reducedmatrix_ews.R")
  source("./rspec_ews.R")
  source("./myfftshift_ews.R")
  source("./patchsizedistr_ews.R")
  
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
  sd_data = numeric(numsnaps)
  skew_data = numeric(numsnaps)
  corr_data = numeric(numsnaps)
  sd_red_data = numeric(numsnaps)
  skew_red_data = numeric(numsnaps)
  corr_red_data = numeric(numsnaps)
  SD_null = matrix(0,numsnaps,3)
  SK_null = matrix(0,numsnaps,3)
  CORR_null = matrix(0,numsnaps,3)
  SD_null_red = matrix(0,numsnaps,3)
  SK_null_red = matrix(0,numsnaps,3)
  CORR_null_red = matrix(0,numsnaps,3)
  tspectr = matrix(0,,numsnaps)
  rspectr = matrix(0,,numsnaps)
  
  for (snaps in 1:numsnaps)
  {
    rowstart <- (snaps-1)*N+1
    rowend <- snaps*ncols
    fullmatrix <- allmatrices[rowstart:rowend, 1:ncols]     
    
    numiter = iter 
    
    if (discrete==TRUE){
      # Patch size distribution 
      patchD = patchsizedistr_ews(fullmatrix)
      
      #Transform binary matrix (presence absence) to continuous matrix
      fullmatrix = reducedmatrix_ews(fullmatrix,subsize) 
    }
    
    if (logtransform==TRUE){
      #Logtransform data  
      fullmatrix = log(fullmatrix+1) 
    }
    
    if (detrending==TRUE){
      #Remove the spatial mean.	
      detrenddata = fullmatrix - mean(as.vector(fullmatrix)) 
    }
    else 
      detrenddata = fullmatrix 
    
    #Compute indicators.
    #Indicators for non-reduced data.
    mean_data[snaps] =  mean(as.vector(detrenddata)) 
    sd_data[snaps] = sd(as.vector(detrenddata)) 
    skew_data[snaps] = skewness(as.vector(detrenddata)) 
    corr_data[snaps] = morancorrelation_ews(detrenddata) 
    
    #Obtain Reduced Data.
    red_detrend_data = reducedmatrix_ews(detrenddata,subsize) 
    
    #Calculate indicators for the reduced data.
    sd_red_data[snaps] = sd(as.vector(red_detrend_data)) 
    skew_red_data[snaps] = skewness(as.vector(red_detrend_data)) 
    corr_red_data[snaps] = morancorrelation_ews(red_detrend_data) 
    
    #DFT of non-reduced data
    Xax <- (1:N)
    Yax <- (1:N)
    half <- 1+(N/2)
    axhalf <- (2:half)
    dft <- fft(detrenddata)
    lmdft <- log(Mod(dft[axhalf,axhalf]))
    lmdft_spatialvar <- mean(lmdft)
    
    #Rspectrum and Thetaspectrum
    spect_detr = rspec_ews(detrenddata) 
    tspectr1 = spect_detr[[1]]
    rspectr1 = spect_detr[[2]]
    if (snaps == 1){
      tspectr = tspectr1
      rspectr = rspectr1
    }
    else
    {
      tspectr = cbind(tspectr,tspectr1,deparse.level=0)
      rspectr = cbind(rspectr,rspectr1,deparse.level=0)
    }			
    
    #Mean and Standard deviation of the non-reduced pattern
    emptyspace = 1 - mean(as.vector(detrenddata)) # to find aver cover of discrete matrix
    sigma = sd(as.vector(detrenddata))
    
    #For null model iterations. This needs to be done only the first time.  
    if (snaps == 1) {
      sdtemp=numeric(numiter) 
      skewtemp=numeric(numiter) 
      corrtemp=numeric(numiter) 
      
      sdtemp_red=numeric(numiter) 
      skewtemp_red=numeric(numiter) 
      corrtemp_red=numeric(numiter) 
      
      res_ind = matrix(0,3,4)
      rownames(res_ind) =  c("Std deviation","Skewness","Correlation")
      colnames(res_ind) =  c("original","reduced","5tile","95tile")
    }
    
    # NULL MODELS
    for (iter in 1:numiter)
    {
      #Create the random matrix.
      if (discrete==TRUE)
      { binaryrandomdata = matrix (data = Heaviside(runif(N*N),emptyspace), nrow=N, ncol=N) 
        randomdata = reducedmatrix_ews(binaryrandomdata,subsize)
      }
      else if (nullmodel==1)
      {
        randomdata = matrix(data = rnorm(N*N, mean=mean_data[snaps], sd=sigma), nrow=N, ncol=N) 		
      } 
      else if (nullmodel==0)
      {
        randomdata = matrix(sample(detrenddata),N,N) 
      }
      
      red_randdata = reducedmatrix_ews(randomdata,subsize) 
      
      # Estimate SD SK CORR TSPECT RSPECT of NON reduced data
      sdtemp[iter]=sd(as.vector(randomdata)) 
      skewtemp[iter]=skewness(as.vector(randomdata)) 
      corrtemp[iter]=morancorrelation_ews(randomdata) 	
      specttemp = rspec_ews(randomdata) 
      tspectrtemp1 = specttemp[[1]]
      rspectrtemp1 = specttemp[[2]]
      
      # Estimate SD SK CORR of reduced data
      sdtemp_red[iter]=sd(as.vector(red_randdata)) 
      skewtemp_red[iter]=skewness(as.vector(red_randdata)) 
      corrtemp_red[iter]=morancorrelation_ews(red_randdata) 
      
      if (iter == 1){
        tspectrtemp = tspectrtemp1
        rspectrtemp = rspectrtemp1
      }
      else
      {
        tspectrtemp = rbind(tspectrtemp,tspectrtemp1,deparse.level=0)
        rspectrtemp = rbind(rspectrtemp,rspectrtemp1,deparse.level=0)
      }				
    }
    
    #Estimate 5 and 95 percentiles
    SD_null[snaps,] = quantile(sdtemp, probs = c(.05, .5, .95))
    SK_null[snaps,] = quantile(skewtemp, probs = c(.05, .5, .95))
    CORR_null[snaps,] = quantile(corrtemp,probs = c(.05, .5, .95))
    
    SD_null_red[snaps,] = quantile(sdtemp_red, probs = c(.05, .5, .95))
    SK_null_red[snaps,] = quantile(skewtemp_red, probs = c(.05, .5, .95))
    CORR_null_red[snaps,] = quantile(corrtemp_red, probs = c(.05, .5, .95))
    
    tspectr_null = apply(tspectrtemp,2, quantile, probs = c(.05, .5, .95))
    rspectr_null = apply(rspectrtemp,2, quantile, probs = c(.05, .5, .95))
    
    # Fill in table
    res_ind[1,1] = sd_data[snaps]
    res_ind[2.1] = skew_data[snaps]
    res_ind[3,1] = corr_data[snaps]
    res_ind[1,2] = sd_red_data[snaps]
    res_ind[2,2] = skew_red_data[snaps]
    res_ind[3,2] = corr_red_data[snaps]  
    res_ind[1,3] = SD_null_red[snaps,1]
    res_ind[2,3] = SK_null_red[snaps,1]
    res_ind[3,3] = CORR_null[snaps,1]  
    res_ind[1,4] = SD_null_red[snaps,3]
    res_ind[2,4] = SK_null_red[snaps,3]
    res_ind[3,4] = CORR_null_red[snaps,3] 
    
    # Plotting
    x11(width=6.774360 ,height=8.527267)
    
    par(mar=(c(5.1, 4.1, 4.1, 2.1)+0.5),oma=c(5,2,5,2),mfrow=c(3,2))
    layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE),widths=c(1,2), heights=c(1,1,1))
    
    # Original data
    image(Xax,Yax,detrenddata,col=gray.colors(50),xlab='X dimension',ylab='Y dimension',main='original data')
#     image.plot(add = TRUE, legend.only=TRUE, smallplot= c(0.85,0.9,0.35,0.7), zlim= c(min(data), max(data)),col = gray.colors(50), legend.shrink = 1,graphics.reset = TRUE) 

    # Rspectrum
    plot(rspectr1,type='l',ylab="",xlab="wavenumber",main="rho spectrum")
    lines(1:dim(rspectr_null)[2], rspectr_null[1,],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    lines(1:dim(rspectr_null)[2], rspectr_null[3,],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    legend("topright", "5th-95th %tile", fill = 2, col = 2, bt = "n")
    
    # 2D Discrete Fourier Transform
    image(axhalf,axhalf,lmdft,col=heat.colors(50),xlab="x wavenumber",ylab="y wavenumber",main='2dim DFT')
#     image.plot(add = TRUE, legend.only=TRUE, smallplot= c(0.85,0.9,0.35,0.7), zlim= c(min(lmdft), max(lmdft)),col = heat.colors(50), legend.shrink = 1, graphics.reset = TRUE) 
    
    # Thetaspectrum  
    plot(tspectr1,type='l',ylab="",xlab="wavenumber",main="theta spectrum",ylim=c(min(rspectr_null[1,]),max(rspectr_null[3,])))
    lines(1:dim(rspectr_null)[2], rspectr_null[1,],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    lines(1:dim(rspectr_null)[2], rspectr_null[3,],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    
    # Patch size distributions
    if (discrete==TRUE)
    {
      plot(log(patchD$bins[[1]]),log(patchD$cum_count[[1]]),xlab="log(Size)",ylab="log(Cumulative distribution)",pch=16,col="chartreuse4")
    }
    else
    {  
      plot(0,0,ylab="",xlab="",type="n",xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),frame.plot=TRUE,main='patch-size distribution')
      text(0.5,0.5,"no patch-size \n no discrete data")
    }
    
    # Spatial indicators
    plot(0,0,ylab="",xlab="",type="n",,xaxt="n",yaxt="n",xlim=c(0,1),ylim=c(0,1),frame.plot=FALSE, main = "spatial statistics")
    bcol=matrix(c(0,0,3,5,5,0,5,5,3,5,5,3),3,4)
    addtable2plot(-0.25,0.9,signif(res_ind,3),display.colnames=TRUE,display.rownames=TRUE,hlines=TRUE,vlines=TRUE,cex=1.25, xpad=0.5,ypad=0.5,
                  title="",bty="o", bg=bcol,xjust=0,yjust=0)
    
    mtext("Spatial Early-Warnings Each Snapshot",side=3,line=0.01, outer=TRUE, font =2)
    
  } #end of snap loop
  
  if (numsnaps>1)
  {
    x11()
    par(mfrow=c(2,2),mar=c(5, 4.2, 4, 2))
    plot(1:numsnaps,mean_data,type ="b",xlab="# snapshot",ylab="mean original data")
    plot(1:numsnaps,sd_data,type ="b",xlab="# snapshot",ylab="sd original data")
    plot(1:numsnaps,skew_data,type ="b",xlab="# snapshot",ylab="skewness original data")
    if (nullmodel != 0)
    {
      lines(1:numsnaps, SK_null[,1],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
      lines(1:numsnaps, SK_null[,3],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
      legend("topright", "5th-95th %tile", fill = 2, col = 2, bt = "n", cex = 0.8)
    }
    plot(1:numsnaps,corr_data,type ="b",xlab="# snapshot",ylab="correlation original data",ylim=c(min(c(CORR_null[,1],corr_data)),max(c(CORR_null[,3],corr_data))))
    lines(1:numsnaps, CORR_null[,1],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    lines(1:numsnaps, CORR_null[,3],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    legend("topright", "5th-95th %tile", fill = 2, col = 2, bt = "n", cex = 0.8)
    
    mtext("Trend Spatial Early-Warnings Original Data",side=3,line= - 3, outer=TRUE, font =2)
    
    x11()
    par(mfrow=c(2,2),mar=c(5, 4.2, 4, 2))
    plot(1:numsnaps,mean_data,type ="b",xlab="# snapshot",ylab="mean original data")
    plot(1:numsnaps,sd_red_data,type ="b",xlab="# snapshot",ylab="sd reduced data")
    lines(1:numsnaps, SD_null_red[,1],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    lines(1:numsnaps, SD_null_red[,3],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    legend("topright", "5th-95th %tile", fill = 2, col = 2, bt = "n", cex = 0.8)
    plot(1:numsnaps,skew_red_data,type ="b",xlab="# snapshot",ylab="skewness reduced data")
    lines(1:numsnaps, SK_null_red[,1],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    lines(1:numsnaps, SK_null_red[,3],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    legend("topright", "5th-95th %tile", fill = 2, col = 2, bt = "n", cex = 0.8)
    plot(1:numsnaps,corr_red_data,type ="b",xlab="# snapshot",ylab="correlation reduced data")
    lines(1:numsnaps, CORR_null_red[,1],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    lines(1:numsnaps, CORR_null_red[,3],lty="dashed",ylab="",xlab="",xaxt="n",col=2,las=1)
    legend("topright", "5th-95th %tile", fill = 2, col = 2, bt = "n", cex = 0.8)
    
    mtext("Trend Spatial Early-Warnings Reduced Data",side=3,line= - 3, outer=TRUE, font =2)  
  }
  
  # Output
  out1<-data.frame(mean_data, sd_data, skew_data, corr_data, sd_red_data, skew_red_data, corr_red_data)
  colnames(out1)<-c("mean","sd_full","sk_full","corr_full","sd_red","sk_red","corr_red")
  out5=data.frame(tspectr)
  out6=data.frame(rspectr)
  out2=data.frame(SD_null)
  out3=data.frame(SK_null)
  out4=data.frame(CORR_null)
  out21=data.frame(SD_null_red)
  out31=data.frame(SK_null_red)
  out41=data.frame(CORR_null_red)
  
  out=list("stats" = out1,"thetaspectr" = out5,"rhospectr" = out6, "SD_null" = out2, "SK_null" = out3, "CORR_null" = out4, "SD_null_red" = out21, "SK_null_red" = out31, "CORR_null_red" = out41)
  
  return(out)  
}	
