#Funtion lbl to label patches in a matrix.
#This function is a dependency for Indicators_FractalGeometry.R and FGcore.R
#This function is an extension of the bwlabel function from EBImage package.
#The only addition is the incorporation of periodic boundary conditions

require(EBImage) #This package requires Biocinstaller to install. 
                 #type the following commands to install 
                  #source("http://bioconductor.org/biocLite.R")
                  #biocLite()
                 #Once the installer is installed, type
                  #bioclite("EBImage")

lbl<-function(data){
A=bwlabel(data)
n=dim(data)[1]

for (j in 1:n){
  if((A[n,j]>0) & (A[1,j]>0) & (A[n,j]< A[1,j])){
  x=A[1,j]
  y=A[n,j]
    for (k in 1:n){
      for (l in 1:n){
        if(A[k,l]==x){
        A[k,l]=y
        }
      }
    }
  } 
  else if((A[n,j]>0) & (A[1,j]>0) & (A[n,j]> A[1,j])){
  x=A[n,j]
  y=A[1,j]
    for (k in 1:n){
      for (l in 1:n){
        if(A[k,l]==x){
        A[k,l]=y
        }
      }
    }
  }
}

for (i in 1:n){
  if((A[i,n]>0) & (A[i,1]>0) & (A[i,n]<A[i,1])){
  x=A[i,1]
  y=A[i,n]
    for (k in 1:n){
      for (l in 1:n){
        if(A[k,l]==x){
        A[k,l]=y
        }
      }
    }
  } 
  else if((A[i,n]>0) & (A[i,1]>0) & (A[i,n]>A[i,1])){
  y=A[i,1]
  x=A[i,n]
    for (k in 1:n){
      for (l in 1:n){
        if(A[k,l]==x){ 
        A[k,l]=y
        }
      }
    }
  }
}
  
return(A)
}