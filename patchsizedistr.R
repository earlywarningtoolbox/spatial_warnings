# Author: Sonia Kefi, Novemeber 9, 2013

patchsizedistr = function(fullmatrix,cutoff=1)
{
	require(SDMTools)
	
	xsize = dim(p)[1]
if (is.null(cutoff)){
	transmatrix = matrix(as.numeric(fullmatrix==1),nrow=xsize,ncol=xsize)
}	else{
	transmatrix = matrix(as.numeric(fullmatrix<cutoff),nrow=xsize,ncol=xsize)
}

ccl.mat = ConnCompLabel(transmatrix)
ps.data = PatchStat(ccl.mat)

size.patches = ps.data[6]

xmin <- 1
size.patches <- size.patches[size.patches >= xmin]
nmax <- max(size.patches)
l.size.patches <- length(size.patches)

bin <- unique(size.patches)
bin2 <- sort(bin)
l.bin <- length(bin)

size.count <- vector()

## Calculate counts in each bin
for(j in 1:l.bin-1){
	size.count[j] <- 0
	for(i in 1:l.size.patches){
		if( (size.patches[i]>=bin2[j]) && (size.patches[i]<bin2[j+1]) ) {
			size.count[j] <- size.count[j] + 1
		}
	}

}
	
j<-l.bin
size.count[j] <- 0
for(i in 1:l.size.patches){
		if( (size.patches[i]>=bin2[j])) {
			size.count[j] <- size.count[j] + 1
		}
	}
	
## Calculate the cumulative distributions
# i.e. the number of species that have more than x links 
cumu.count <- vector()
for(j in 1:l.bin){
	cumu.count[j] <- sum(size.count[j:l.bin])/(sum(size.count))
}
cumdistr = c(bin2,cumu.count)
return(size.patches)	
} 

## plot the cumulative distribution on a log-log scale		
# pdf("Fig_loglogCumuPlot_TPBb39.pdf")	 
plot(log(bin2),log(cumu.count),xlab="log(Size)",ylab="log(Cumulative distribution)",pch=16,col="chartreuse4")
dev.off()

#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
# Fit the distri with lm 
#----------------------------------------------------------------------------------------------
# We fit three functions, a power law, a power law with exponential cutoff, and an exponential distributions to the data after linearizing them

# y <- log(cumu.count[1:l.bin])
# x <- log(bin2[1:l.bin])
# mx <- max(x)

# ## Power law
# #model.1<-lm(y~x) 
# model.1<-lm(y~x-1) # the -1 tells that the fits has to pass through the origin but then what do the para mean?
# summary(model.1)

# ##Exponential
# #model.3<-lm(y~bin2)
# model.3<-lm(y~bin2-1)
# summary(model.3)

# ## Power law with exponential cutoff
# model.2<-nls(y~a2-b2*x-exp(x)/Sx,start=list(a2=1,b2=5,Sx=1))
# summary(model.2)
# # can we make sure this one goes through the origin?

# AIC(model.1,model.2,model.3) # the model with the lower AIC is the best

# pdf("Fig_loglogCumuPlot_LSE_TPBb39.pdf")	 
# plot(log(bin2),log(cumu.count),xlab="log(Size)",ylab="log(Cumulative distribution)",pch=16,col="chartreuse4")#,main="Least square regression")
# av1<-seq(0,mx,0.01)
# bv1<-predict(model.1,list(x=av1))
# lines(av1,bv1,col="blue3")
# bv3<-predict(model.3,list(bin2=exp(av1)))
# lines(av1,bv3,col="darkorange")
# bv2<-predict(model.2,list(x=av1))
# lines(av1,bv2,col="red")
# legend("bottomleft",0,c("PL","TPL","Exp"),pch=c(16,16,16),col=c("blue3","red","darkorange"))
# dev.off()
	
