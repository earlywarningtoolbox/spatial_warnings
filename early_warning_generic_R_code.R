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

fulldata <- read.table(file='BW_all.txt',header=FALSE,sep='\t')
data <- data.matrix(fulldata)

numrows<-nrow(data) 
numcol<-ncol(data)
nsnaps<-numrows/numcol #number of snapshots in the data set.

#To store moments of spatial variable as a function of time
mean_spatialvar <- mat.or.vec(nsnaps,1)
var_spatialvar <- mat.or.vec(nsnaps,1)
skew_spatialvar <- mat.or.vec(nsnaps,1)
corr_spatialvar <-mat.or.vec(nsnaps,1)
lmdft_spatialvar <- mat.or.vec(nsnaps,1)

#Calculate early warning signals as a function of time.
for (snap in 1:nsnaps)
{
	rowstart <- (snap-1)*numcol+1
	rowend <- snap*numcol
	datasnap <- data[rowstart:rowend, 1:numcol]
	
	mean_spatialvar[snap] <- mean(as.vector(datasnap))
	var_spatialvar[snap] <- var(as.vector(datasnap))
    skew_spatialvar[snap] <- skewness(as.vector(datasnap))
    corr_spatialvar[snap] <- corr_m(datasnap)
    
    #DFT
    dft <- fft(datasnap)
    lmdft <- log(Mod(dft))
    lmdft_spatialvar[snap] <- mean(lmdft[2:25,2:25])
}

par(mfrow=c(2,3),mar=c(5, 4.2, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
params <- c(1.1, 1.17, 1.25, 1.29, 1.34, 1.45, 1.56, 1.65, 1.7, 1.8)

plot(params,mean_spatialvar)
plot(params,var_spatialvar)
plot(params,skew_spatialvar)
plot(params,corr_spatialvar)
plot(params,lmdft_spatialvar)

#---------------------------#
#Plot DFT using a two dimensional color plot.

Xax <- (1:100)
Yax <- (1:100)
half <- 1+(100/2)
axhalf <- (2:half)

data <- read.table(file='BW_1.1.txt',header=FALSE,sep='\t')
data110 <- data.matrix(data)

data <- read.table(file='BW_1.17.txt',header=FALSE,sep='\t')
data117 <- data.matrix(data)

data <- read.table(file='BW_1.25.txt',header=FALSE,sep='\t')
data125 <- data.matrix(data)

data <- read.table(file='BW_1.29.txt',header=FALSE,sep='\t')
data129 <- data.matrix(data)

data <- read.table(file='BW_1.34.txt',header=FALSE,sep='\t')
data134 <- data.matrix(data)

data <- read.table(file='BW_1.45.txt',header=FALSE,sep='\t')
data145 <- data.matrix(data)

data <- read.table(file='BW_1.56.txt',header=FALSE,sep='\t')
data156 <- data.matrix(data)

data <- read.table(file='BW_1.65.txt',header=FALSE,sep='\t')
data165 <- data.matrix(data)

data <- read.table(file='BW_1.7.txt',header=FALSE,sep='\t')
data170 <- data.matrix(data)

data <- read.table(file='BW_1.8.txt',header=FALSE,sep='\t')
data180 <- data.matrix(data)

dft <- fft(data110);
mdft110 <- Mod(dft[2:half,2:half])
lmdft110 <- log(mdft110)

dft <- fft(data117);
mdft117 <- Mod(dft[2:half,2:half])
lmdft117 <- log(mdft117)

dft <- fft(data125);
mdft125 <- Mod(dft[2:half,2:half])
lmdft125 <- log(mdft125)

dft <- fft(data129);
mdft129 <- Mod(dft[2:half,2:half])
lmdft129 <- log(mdft129)

dft <- fft(data134);
mdft134 <- Mod(dft[2:half,2:half])
lmdft134 <- log(mdft134)

dft <- fft(data145);
mdft145 <- Mod(dft[2:half,2:half])
lmdft145 <- log(mdft145)

dft <- fft(data156);
mdft156 <- Mod(dft[2:half,2:half])
lmdft156 <- log(mdft156)

dft <- fft(data165);
mdft165 <- Mod(dft[2:half,2:half])
lmdft165 <- log(mdft165)

dft <- fft(data170);
mdft170 <- Mod(dft[2:half,2:half])
lmdft170 <- log(mdft170)

dft <- fft(data180);
mdft180 <- Mod(dft[2:half,2:half])
lmdft180 <- log(mdft180)

library(fields) #for using image.plot
par(mfrow=c(3,3),mar=c(5, 4.2, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)

zrange <- range(c(data110,data129,data156,data180))

image.plot(Xax,Yax,data180,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.8')

image(Xax,Yax,data170,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.7')

image.(Xax,Yax,data165,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.65')

image.plot(Xax,Yax,data156,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.56')

image(Xax,Yax,data145,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.45')

image(Xax,Yax,data129,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.34')

image.plot(Xax,Yax,data125,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.25')

image(Xax,Yax,data117,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.17')

image(Xax,Yax,data110,zlim=zrange,col=heat.colors(50),xlab='X',ylab='Y',
main='bw = 1.1')

par(mfrow=c(3,3),mar=c(5, 4, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)

zrange <- range(c(mdft110,mdft129,mdft156,mdft180,mdft117,mdft145))

image.plot(axhalf,axhalf,mdft180,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='mdft: bw = 1.80')

image(axhalf,axhalf,mdft170,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='mdft: bw = 1.70')

image(axhalf,axhalf,mdft180,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='mdft: bw = 1.65')

image.plot(axhalf,axhalf,mdft156,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='mdft: bw = 1.56')

image(axhalf,axhalf,mdft145,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='mdft: bw = 1.45')

image(axhalf,axhalf,mdft134,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='mdft: bw = 1.34')

image.plot(axhalf,axhalf,mdft125,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='mdft: bw = 1.25')

image(axhalf,axhalf,mdft117,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='mdft: bw = 1.17')

image(axhalf,axhalf,mdft110,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='mdft: bw = 1.1')

#Now plot logmdft.

par(mfrow=c(3,3),mar=c(5, 4, 4, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)

zrange <- range(c(lmdft110,lmdft129,lmdft156,lmdft180,lmdft117,lmdft145))

image.plot(axhalf,axhalf,lmdft180,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='log(mdft): bw = 1.80')

image(axhalf,axhalf,lmdft170,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='log(mdft): bw = 1.70')

image(axhalf,axhalf,lmdft180,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='log(mdft): bw = 1.65')

image.plot(axhalf,axhalf,lmdft156,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='log(mdft): bw = 1.56')

image(axhalf,axhalf,lmdft145,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='log(mdft): bw = 1.45')

image(axhalf,axhalf,lmdft134,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='log(mdft): bw = 1.34')

image.plot(axhalf,axhalf,lmdft125,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='log(mdft): bw = 1.25')

image(axhalf,axhalf,lmdft117,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='',
main='log(mdft): bw = 1.17')

image(axhalf,axhalf,lmdft110,zlim=zrange,col=heat.colors(50),xlab='X Freq',ylab='Y Freq',
main='log(mdft): bw = 1.1')