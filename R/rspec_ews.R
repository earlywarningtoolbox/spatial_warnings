#function to calculate r-spectrum and theta-spectrum. NOT THE MAIN FUNCTION, but called by it.
#Dependency for Indicator_SpectralFunctions
#Originally written by Vincent Deblauwe, novembre 2007 in Matlab.
#Translated to R by Vishwesha Guttal, Nov 2013.

#test=replicate(100,rnorm(100));

#test=read.table("SDF_R0.91.txt",sep="\t",header=FALSE)
#test=data.matrix(test)


#dependencies of this function - myfftshift_ews.R

rspec_ews = function(rawmatrix){

test= data.matrix(rawmatrix)

nr=dim(test)[1]
nc=dim(test)[2]

n0x = floor(nc/2)+1
n0y = floor(nr/2)+1

#Create distance and angle matrices
f1 = t(replicate(nr,seq(1,nc))) - n0x
f2 = replicate(nc,seq(1,nr)) - n0y
DIST = sqrt(f1^2 + f2^2)
ANGLE=atan2(-f2,f1)*180/pi

#Calculate DFT
mi=1
ma=min(c(n0x,n0y))
DISTMASK = DIST>=mi & DIST<=ma
tmp=fft(test)
tmpshift=myfftshift(tmp)
tmpshift[n0x,n0y]=0
aspectr2D=abs(tmpshift)^2/(n0x*n0y)^4

sig2=sum(aspectr2D[DISTMASK]) #Normalisation
aspectr2D=aspectr2D/sig2 #Normalisation

#Now calculate r-spectrum
STEP=1
ray=seq(mi,ma,STEP)
rspectr=numeric(length(ray))
for (i in 1:length(ray))
{
    m = DIST>=ray[i]-STEP/2 & DIST<ray[i]+STEP/2
    rspectr[i] = mean(aspectr2D[m])
}

#Now calculate theta-spectrum
DISTMASK = DIST>=mi & DIST<=ma
STEP=5 #increments of 5 degrees
anglebin=seq(STEP,180,STEP)
tspectr=numeric(length(anglebin))
for (i in 1:(length(tspectr)-1))
{
  m = which(DISTMASK & ANGLE>=anglebin[i]-STEP & ANGLE<anglebin[i])
  tspectr[i] = sum(aspectr2D[m])/length(m)
}

m = which(DISTMASK & ANGLE >=anglebin[length(anglebin)]-STEP & ANGLE <=anglebin[length(anglebin)])
tspectr[length(tspectr)] = sum(aspectr2D[m])/length(m)

out = list(tspec=tspectr, rspec=rspectr)
return(out)
}
