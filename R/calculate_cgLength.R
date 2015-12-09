## This code calculates the amount of coarse-graining required to calculate the spatial ews (especially variance) for binary data

N=512;                      # system size
maxCG=10;                   # maximum coarse-graining length
CG_range=1:maxCG;           # array containing all the coarse-graining lengths from 1 to maxCG 

variance<-numeric(maxCG)              # array to store the variance value for each coarse-graining length

randomdata = matrix(sample(data),N,N)  # random data is the reshuffled matrix with the same density. It acts as null
variance_null<-numeric(maxCG)         # array to store the variance value for each coarse-graining length calculated for null model


for (i in CG_range){

# coarse grain the data with coarse-graining length i  
 red_mat<-reducedmatrix_ews(data,i)  
 
# coarse-grain the null matrix 
 red_mat_null<-reducedmatrix_ews(randomdata,i)

# calculate the variance for  both original and null data with coarse-graining i
 variance[i]<-sd(red_mat)^2
 variance_null[i]<-sd(red_mat_null)^2
  
}

# plot variance vs cg length  
plot(CG_range,variance,col="dark red",type ="l",lwd=4,ylab="variance",xlab="C-G length",cex.lab=1.2,col.lab="black")
points(CG_range,variance_null,col="black",type ="l",lwd=4,ylab="variance",xlab="C-G length",cex.lab=1.2,col.lab="black")


# The optimum coarse-graining is when the difference between variance of original data and null data is significant 

diff<- variance-variance_null   # array of difference in variance
i<-which(diff==max(diff))
optCG<-CG_range[i]              # the value of optCG will give the optimum coarse-graining required for the given data
