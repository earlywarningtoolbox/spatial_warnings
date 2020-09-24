# A simple R code to calculate early warning signals of critical transitions in space.

#Load libraries
library(moments)

#Define functions

#Function to calculate Moran's I coefficient OR Spatial autocorrelation between nearest neighbors.
#Can also be calculated using spatial statistics softwares.
corr_m <- function(input)
{
  m <- mean(as.vector(input))
  v <- var(as.vector(input))
  n <- (nrow(input)-1)
  
  moranI <- 0
  for (i in 2:n){
    for (j in 2:n) {
      moranI <- moranI + (input[i,j]-m)*(input[i,j-1]+input[i,j+1]+input[i-1,j]+input[i+1,j]-4*m)	
    }
  }
  moranI <- moranI/(4*v*(n-2)*(n-2))
  moranI
}

# Read data from the file. 
# Format: Each spatial snapshop is of size nxn matrix. There are t such matries, appended one after other.

fulldata <- read.table(file='CA_all.txt',header=FALSE,sep='\t')
data <- data.matrix(fulldata)

numrows<-nrow(data) 
numcol<-ncol(data)
nsnaps<-numrows/numcol #number of snapshots in the data set.

#To store moments of spatial variable as a function of time
mean_spatialvar <- mat.or.vec(nsnaps,1)
var_spatialvar <- mat.or.vec(nsnaps,1)
cvar_spatialvar <- mat.or.vec(nsnaps,1)
skew_spatialvar <- mat.or.vec(nsnaps,1)
corr_spatialvar <-mat.or.vec(nsnaps,1)
lmdft_spatialvar <- mat.or.vec(nsnaps,1)

mean_reduced <- mat.or.vec(nsnaps,1)
var_reduced <- mat.or.vec(nsnaps,1)
cvar_reduced <- mat.or.vec(nsnaps,1)
skew_reduced <- mat.or.vec(nsnaps,1)
corr_reduced <-mat.or.vec(nsnaps,1)
lmdft_reduced <- mat.or.vec(nsnaps,1)

coarse=10;
n=numcol/coarse;
reddata <- mat.or.vec(n,n);

#Calculate early warning signals as a function of time.
for (snap in 1:nsnaps)
{
	rowstart <- (snap-1)*numcol+1
	rowend <- snap*numcol
	datasnap <- data[rowstart:rowend, 1:numcol]
	
	mean_spatialvar[snap] <- mean(as.vector(datasnap))
	var_spatialvar[snap] <- var(as.vector(datasnap))
	cvar_spatialvar[snap] <- var_spatialvar[snap]/mean_spatialvar[snap];
    skew_spatialvar[snap] <- skewness(as.vector(datasnap))
    corr_spatialvar[snap] <- corr_m(datasnap)
     
    #TO do: DFT.
    dft <- fft(datasnap)
    lmdft <- Mod(dft)
    lmdft_spatialvar[snap] <- mean(lmdft[2:25,2:25])
    
    #Calculate reduced matrix by coarse graining over coarsexcoarse submatrix.
	for (i in 1:n)
	{
		for (j in 1:n)
		{
			submatrix = datasnap[((i-1)*coarse+1):(i*coarse),((j-1)*coarse+1):(j*coarse)];
			reddata[i,j] = mean(as.vector(submatrix));
		}
	}
    
	mean_reduced[snap] <- mean(as.vector(reddata))
	var_reduced[snap] <- var(as.vector(reddata))
	cvar_reduced[snap] <- var_reduced[snap]/mean_reduced[snap]
    skew_reduced[snap] <- skewness(as.vector(reddata))
    corr_reduced[snap] <- corr_m(reddata)
    dft <- fft(reddata)
    lmdft <- Mod(dft)
    lmdft_reduced[snap] <- mean(lmdft[2:n/2,2:n/2])
}




# [ADDED BY ALEX]
# There is only DFT-related code beyond this point with plotting stuff.
#   We do not want this for automated tests. 
# ------------------------------------------------------




# par(mfrow=c(3,2),mar=c(5, 4.2, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
# 
# params <- c(0.39, 0.40, 0.46, 0.475, 0.49, 0.50, 0.51, 0.53, 0.60, 0.65)
# 
# plot(params,mean_spatialvar)
# plot(params,var_spatialvar)
# plot(params,cvar_spatialvar)
# plot(params,skew_spatialvar)
# plot(params,corr_spatialvar)
# plot(params,lmdft_spatialvar)
# 
# dev.new()
# par(mfrow=c(3,2),mar=c(5, 4.2, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
# params <- c(0.39, 0.40, 0.46, 0.475, 0.49, 0.50, 0.51, 0.53, 0.60, 0.65)
# 
# plot(params,mean_reduced)
# plot(params,var_reduced)
# plot(params,cvar_reduced)
# plot(params,skew_reduced)
# plot(params,corr_reduced)
# plot(params,lmdft_reduced)
# 
# #---------------------------#
# #Plot DFT using a two dimensional color plot.
# 
# Xax <- (1:100)
# Yax <- (1:100)
# half <- 1+(100/2)
# axhalf <- (2:half)
# 
# data <- read.table(file='CA_b0.39.txt',header=FALSE,sep='\t')
# data39 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.40.txt',header=FALSE,sep='\t')
# data40 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.46.txt',header=FALSE,sep='\t')
# data46 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.475.txt',header=FALSE,sep='\t')
# data475 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.49.txt',header=FALSE,sep='\t')
# data49 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.50.txt',header=FALSE,sep='\t')
# data50 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.51.txt',header=FALSE,sep='\t')
# data51 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.53.txt',header=FALSE,sep='\t')
# data53 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.60.txt',header=FALSE,sep='\t')
# data60 <- data.matrix(data)
# 
# data <- read.table(file='CA_b0.65.txt',header=FALSE,sep='\t')
# data65 <- data.matrix(data)
# 
# dft <- fft(data39);
# mdft39 <- Mod(dft[2:half,2:half])
# lmdft39 <- log(mdft39)
# 
# dft <- fft(data40);
# mdft40 <- Mod(dft[2:half,2:half])
# lmdft40 <- log(mdft40)
# 
# dft <- fft(data46);
# mdft46 <- Mod(dft[2:half,2:half])
# lmdft46 <- log(mdft46)
# 
# dft <- fft(data475);
# mdft475 <- Mod(dft[2:half,2:half])
# lmdft475 <- log(mdft475)
# 
# dft <- fft(data49);
# mdft49 <- Mod(dft[2:half,2:half])
# lmdft49 <- log(mdft49)
# 
# dft <- fft(data50);
# mdft50 <- Mod(dft[2:half,2:half])
# lmdft50 <- log(mdft50)
# 
# dft <- fft(data51);
# mdft51 <- Mod(dft[2:half,2:half])
# lmdft51 <- log(mdft51)
# 
# dft <- fft(data53);
# mdft53 <- Mod(dft[2:half,2:half])
# lmdft53 <- log(mdft53)
# 
# dft <- fft(data60);
# mdft60 <- Mod(dft[2:half,2:half])
# lmdft60 <- log(mdft60)
# 
# dft <- fft(data65);
# mdft65 <- Mod(dft[2:half,2:half])
# lmdft65 <- log(mdft65)
# 
# par(mfrow=c(3,3),mar=c(5, 4.2, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
# 
# zrange <- range(c(data39,data40, data46, data475, data49, data50,data51,data53, data60,data65))
# 
# image(Xax,Yax,data39,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.39')
# 
# image(Xax,Yax,data40,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.40')
# 
# image.plot(Xax,Yax,data46,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.46')
# 
# image(Xax,Yax,data475,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.475')
# 
# image(Xax,Yax,data49,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.49')
# 
# image.plot(Xax,Yax,data51,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.51')
# 
# image(Xax,Yax,data53,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.53')
# 
# image(Xax,Yax,data60,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.60')
# 
# image.plot(Xax,Yax,data65,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
# main='b=0.65')
# 
# #Plot DFT
# par(mfrow=c(3,3),mar=c(5, 4, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
# 
# zrange <- range(c(mdft39,mdft49,mdft51,mdft53,mdft65,mdft40))
# 
# image(axhalf,axhalf,mdft39,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
# main='mdft: b=0.39')
# 
# image(axhalf,axhalf,mdft40,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='mdft: b=0.40')
# 
# image.plot(axhalf,axhalf,lmdft46,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='mdft: b=0.46')
# 
# image(axhalf,axhalf,mdft475,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='mdft: b=0.475')
# 
# image(axhalf,axhalf,mdft49,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='mdft: b=0.49')
# 
# image.plot(axhalf,axhalf,mdft50,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
# main='mdft: b=0.50')
# 
# image(axhalf,axhalf,mdft51,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
# main='mdft: b=0.51')
# 
# image(axhalf,axhalf,mdft60,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='mdft: b=0.60')
# 
# image.plot(axhalf,axhalf,mdft65,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='mdft: b=0.65')
# 
# #Plot mDFT with log for better clarity on patterns.
# 
# par(mfrow=c(3,3),mar=c(5, 4, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
# 
# zrange <- range(c(lmdft39,lmdft49,lmdft51,lmdft53))
# 
# image(axhalf,axhalf,lmdft39,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
# main='log(mdft): b=0.39')
# 
# image(axhalf,axhalf,lmdft40,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='log(mdft): b=0.40')
# 
# image.plot(axhalf,axhalf,lmdft46,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='log(mdft): b=0.46')
# 
# image(axhalf,axhalf,lmdft475,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='log(mdft): b=0.475')
# 
# image(axhalf,axhalf,lmdft49,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='log(mdft): b=0.49')
# 
# image.plot(axhalf,axhalf,lmdft50,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
# main='log(mdft): b=0.50')
# 
# image(axhalf,axhalf,lmdft51,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
# main='log(mdft): b=0.51')
# 
# image(axhalf,axhalf,lmdft60,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='log(mdft): b=0.60')
# 
# image.plot(axhalf,axhalf,lmdft65,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
# main='log(mdft): b=0.65')