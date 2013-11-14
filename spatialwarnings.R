#' Description: Spatial Early Warning Signals
#'
#' \code{spatial_ews} wrapper function used to estimate statistical properties from spatial data and compare results to null models
#'
# Details:
#' see ref below
#'
# Arguments:
#'    @param rawmatrix is the input matrix (must be square matrix)
#'    @param subsize is the dimension of the submatrix used to subsample the rawmatrix either to reduce the size of the original matrix to  (DEFAULT value = 5)
#'    @param discrete logical. If TRUE the data represent discrete variables (like presence/absense), otherwise continuous data (like biomass density). Default is FALSE.
#'    @param nullmodel default value of 1 means we generate a random matrix with Gaussian distribution of same mean and variance as original dataany different value would mean the null model is a random matrix which preserves power spectra of original data.
#'    @param detrending logical. If TRUE data are detrended by removing the spatial mean. Default is FALSE.
#'    @param logtransform logical. If TRUE data are logtransformed prior to analysis as log(X+1). Default is FALSE.
#'    @param powerspectrum logical. If TRUE the power spectrum within each rolling window is plotted. Default is FALSE. 
#' 
# Returns:
#'   @return \code{spatial_ews} returns a matrix that contains:
#'   @return \item{tim}{the time index.}

#' In addition, \code{generic_ews} returns three plots. The first plot contains the original data, the detrending/filtering applied and the residuals (if selected), and all the moment statistics. For each statistic trends are estimated by the nonparametric Kendall tau correlation.  The second plot, if asked, quantifies resilience indicators fitting AR(n) selected by the Akaike Information Criterion. The third plot, if asked, is the power spectrum estimated by \code{\link{spec.ar}} for all frequencies within each rolling window.
#'  
#' @export
#' 
#' @author Vishewesha Guttal, Sonia Kefi, Vasilis Dakos \email{vasilis.dakos@@gmail.com}
#' @references Kefi S. et al (2013). ...
#' @seealso \code{\link{generic_ews}}; \code{\link{ddjnonparam_ews}}; \code{\link{bdstest_ews}}; \code{\link{sensitivity_ews}}; \code{\link{surrogates_ews}}; \code{\link{ch_ews}}; \code{\link{movpotential_ews}}; \code{\link{livpotential_ews}}; 
#'
#' @importFrom moments skewness
#' @importFrom moments kurtosis
#'
#' @examples 
#' data(foldbif)
#' out=spatial_ews(foldbif,winsize=50,detrending="gaussian",
#' bandwidth=5,logtransform=FALSE,interpolate=FALSE)
#' @keywords early-warning

# Author: Vasilis Dakos, Novemeber 9, 2013



#OUTPUT
spatialwarnings = function(fullmatrix, subsize=5, discrete=FALSE, nullmodel=1)
{

numiter=100;

library("moments");
library("fBasics");

source("./morancorrelation.R");
source("./reducedmatrix.R");

N=dim(fullmatrix)[1];

if (discrete==FALSE){
#Remove the spatial mean.	
detrenddata = fullmatrix - mean(as.vector(fullmatrix));
}
else 
detrenddata = fullmatrix;

#Reduce the matrix and compute indicators.

#Indicators for non-reduced data.
mean_data =  mean(as.vector(fullmatrix));
var_data = var(as.vector(detrenddata));
skew_data = skewness(as.vector(detrenddata));
corr_data = morancorrelation(detrenddata);

#Obtain Reduced Detrended Data.
red_detrend_data = reducedmatrix(detrenddata,subsize);

#Caclulate indicators for the reduced detrended fulldata.
var_red_data = var(as.vector(red_detrend_data));
skew_red_data = skewness(as.vector(red_detrend_data));
corr_red_data = morancorrelation(red_detrend_data);

#Mean and Standard deviation of the fulldata pattern
emptyspace = 1 - mean(as.vector(detrenddata))
sigma = sd(as.vector(detrenddata))

vartemp=numeric(numiter);
skewtemp=numeric(numiter);
corrtemp=numeric(numiter);

vartemp_red=numeric(numiter);
skewtemp_red=numeric(numiter);
corrtemp_red=numeric(numiter);

#ffttemp=numeric(numiter);
		
for (iter in 1:numiter)
{
		#Create the random matrix.
		if (discrete==TRUE)
		{
			randomdata = matrix (data = Heaviside(runif(N*N),emptyspace), nrow=N, ncol=N);
		}
		else
		{
			if (nullmodel==1)
				randomdata = matrix(data = rnorm(N*N, mean=0, sd=sigma), nrow=N, ncol=N);		
		}
		
		red_randdata = reducedmatrix(randomdata,subsize);
		
		vartemp[iter]=var(as.vector(randomdata));
		skewtemp[iter]=skewness(as.vector(randomdata));
		corrtemp[iter]=morancorrelation(randomdata);	
		
		vartemp_red[iter]=var(as.vector(red_randdata));
		skewtemp_red[iter]=skewness(as.vector(red_randdata));
		corrtemp_red[iter]=morancorrelation(red_randdata);	
		#ffttempmatrix=mvfft(red_randdata)
		#ffttemp[iter]=mean(abs(ffttempmatrix[2:floor(n/8),2:floor(n/8)]))		
}

var_null = mean(vartemp);
skew_null = mean(skewtemp);
corr_null = mean(corrtemp);

var_red_null = mean(vartemp_red);
skew_red_null = mean(skewtemp_red);
corr_red_null = mean(corrtemp_red);

sd_var_red_null = sd(vartemp);
sd_skew_red_null = sd(skewtemp);
sd_corr_red_null = sd(corrtemp);

#dft_null = mean(ffttemp);	


# Plotting
	# Generic Early-Warnings
	dev.new()
	par(mar=(c(0,2,0,1)+0),oma=c(7,2,3,1),mfrow=c(5,2))
	plot(timeindex,Y,type="l",ylab="",xlab="",xaxt="n",las=1,xlim=c(timeindex[1],timeindex[length(timeindex)]))
	
results = c(mean_data, var_data, skew_data, corr_data, var_red_data, skew_red_data, corr_red_data, var_null, skew_null, corr_null, var_red_null, skew_red_null, corr_red_null, sd_var_red_null, sd_skew_red_null, sd_corr_red_null);
	# Output
	out<-data.frame(timeindex[mw:length(nsmY)],nARR,nSD,nSK,nKURT,nCV,nRETURNRATE,nDENSITYRATIO,nACF)
	colnames(out)<-c("timeindex","ar1","sd","sk","kurt","cv","returnrate","densratio","acf1")
	return(out)

return(results)	
		
}	
