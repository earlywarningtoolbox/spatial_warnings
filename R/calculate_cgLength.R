N=512;
maxCG=10; CG_range=1:maxCG;
randomdata = matrix(sample(data),N,N) 
variance<-numeric(maxCG)
variance_null<-numeric(maxCG)
for (i in CG_range){

 red_mat<-reducedmatrix_ews(data,i)  
 red_mat_null<-reducedmatrix_ews(randomdata,i)
 variance[i]<-sd(red_mat)^2
 variance_null[i]<-sd(red_mat_null)^2
  
}
plot(CG_range,variance,col="dark red",type ="l",lwd=4,ylab="variance",xlab="C-G length",cex.lab=1.2,col.lab="black")
points(CG_range,variance_null,col="black",type ="l",lwd=4,ylab="variance",xlab="C-G length",cex.lab=1.2,col.lab="black")

diff<- variance-variance_null
i<-which(diff==max(diff))
optCG<-CG_range[i]
