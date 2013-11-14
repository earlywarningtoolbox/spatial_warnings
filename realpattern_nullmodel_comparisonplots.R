# Written by Vishwesha Guttal, Oct 2012 (approx).
# Plot spatial indicators along with null model for spatial early warning signals.

library("fBasics");#Needed if CA data is used where I use a heaviside function.
source("/Users/vishu/Dropbox/SantaFe_SpatialPaper/Datasets/spatialwarnings.R");

#CHANGE depending on which dataset is being used.
#BW Model (Continuous)
#setwd("/Users/vishu/Dropbox/SantaFe_SpatialPaper/Datasets/Dataset1_BW_extended");
#paramrange=seq(1.1, 2.0, 0.01); #CHANGE  depending on which dataset is being used.

#CHANGE: depending on which dataset is being used.
#CA model (Discrete)
 # setwd("/Users/vishu/Dropbox/SantaFe_SpatialPaper/Datasets/Dataset2_CA/Dataset2_CA_full");
 # nsnaps=200;
 # b=numeric(nsnaps);
 # bstart=0.390;
 # b[1]=bstart;
 # binc=0.001;
 # for (snap in 2:nsnaps)
 # {
	 # #b[snap]=bstart+binc*(snap-1);
	 # b[snap]=b[snap-1]+binc;
	 # if (b[snap]==0.396 || b[snap]==0.596 || b[snap]==0.406)
		# b[snap]=b[snap]+binc;
		
 # }
# paramrange=b;

#CHANGE: depending on which dataset is being used.
#Model 3: SDF model.
setwd("/Users/vishu/Dropbox/SantaFe_SpatialPaper/Datasets/Dataset3_SDF_extended");
paramrange=seq(0.01,2.0,0.01); #is the full range.

#Loop through data files.
#For each data file, get null model going.
warnings = matrix(nrow=length(paramrange),ncol=16);
submatrixdim=1;
cnt=0;
discrete=FALSE;  #Keep it false as default. For CA model, change below in the for loop.

begtime=Sys.time();
for (param in paramrange) #Loop over this, eventually.
{
	cnt=cnt+1;
	
	#CHANGE depending on which dataset is being used.
	#datatable = read.table(paste(c("BW_PotAna_",as.character(param),".txt"),collapse=""));	
	#datatable = read.table(paste(c("CA_PotAna_",as.character(param),".txt"),collapse=""));
	datatable = read.table(paste(c("SDF-PotAna_",as.character(param),".txt"),collapse=""));
	datamat = as.matrix(datatable)
	
	#CHANGE: The transformation below applies to CA model only.
	#Keep 1 as 1, but turn 2 and 3 to zero.
	# datamat = Heaviside(1-datamat,-0.1);
	# discrete=TRUE;

	#Back to common stuff here.
	warnings[cnt,] = spatialwarnings(datamat,submatrixdim,discrete)
	print(param)
	
}
print(Sys.time()-begtime)

results = cbind(paramrange,warnings);
#CHANGE
#write.table(results,file="BW_null_model_reduced5x5.txt",sep="\t",row.names=FALSE,col.names=FALSE)
#write.table(results,file="CA_null_model_reduced5x5.txt",sep="\t",row.names=FALSE,col.names=FALSE)
#write.table(results,file="BW_null_model_reduced5x5.txt",sep="\t",row.names=FALSE,col.names=FALSE)

#Plot results for UNREDUCED FULL data and UNREDUCED null model.
par(mfrow=c(2,2),oma = c(2, 0, 2, 0 ))
plot(paramrange,warnings[,1],,ylab="Mean cover") #Mean

#Variance
plot(paramrange,warnings[,2],ylim=c(0,max(warnings[,2])),ylab="Spatial variance")
points(paramrange,warnings[,8],col='blue',type='l')
legend("topright",c("Spatial-model","Null-model"),cex=0.1,col=c("black","blue"), pch=21:22, lty=1:2)

#Skewness
plot(paramrange,warnings[,3],ylim=c(min(warnings[,3]),max(warnings[,3])),,ylab="Spatial skewness")
points(paramrange,warnings[,9],col='blue',type='l')
legend("topright",c("Spatial-model","Null-model"),cex=0.1,col=c("black","blue"), pch=21:22, lty=1:2)

#Correlation
plot(paramrange,warnings[,4],ylim=c(0,max(warnings[,4])),,ylab="Spatial correlation")
points(paramrange,warnings[,10],col='blue',type='l')
legend("topright",c("Spatial-model","Null-model"),cex=0.1,col=c("black","blue"), pch=21:22, lty=1:2)
#CHANGE depending on the type of data.
#title("BW model with null model",outer=TRUE)
#title("CA model with null model",outer=TRUE)
title("SDF model with null model",outer=TRUE)

#Plot results for REDUCED data and REDUCED null model.
dev.new();
par(mfrow=c(2,2),oma = c(2, 0, 2, 0 ))
plot(paramrange,warnings[,1],,ylab="Mean cover") #Mean

#Variance
plot(paramrange,warnings[,5],ylim=c(0,max(warnings[,5])),ylab="Spatial variance")
points(paramrange,warnings[,11],col='blue',type='l')
legend("topright",c("Spatial-model","Null-model"),cex=0.1,col=c("black","blue"), pch=21:22, lty=1:2)

#Skewness
plot(paramrange,warnings[,6],ylim=c(min(warnings[,6]),max(warnings[,6])),,ylab="Spatial skewness")
points(paramrange,warnings[,12],col='blue',type='l')
legend("topright",c("Spatial-model","Null-model"),cex=0.1,col=c("black","blue"), pch=21:22, lty=1:2)

#Correlation
plot(paramrange,warnings[,7],ylim=c(0,max(warnings[,7])),,ylab="Spatial correlation")
#points(paramrange,warnings[,3],col='red')
points(paramrange,warnings[,13],col='blue',type='l')
legend("topright",c("Spatial-model","Null-model"),cex=0.1,col=c("black","blue"), pch=21:22, lty=1:2)

#CHANGE.
#title("BW model with null model (reduced-matrix-analysis)",outer=TRUE)
#title("CA model with null model (reduced-matrix-analysis)",outer=TRUE)
title("SDF model with null model (reduced-matrix-analysis)",outer=TRUE)