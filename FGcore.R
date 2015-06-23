FGcore<-function(data){
periFT=0
areaFT=0
a=lbl(data)# data is the matrix of occupied and unoccupied sites
b=a;
L=dim(a);

for (i in 2:(L[1]-1)){
  for (j in 2:(L[2]-1)){
    if (a[i,j]>0){
      if ((b[i-1,j]==a[i,j]) & (b[i,j-1]==a[i,j]) &  (b[i+1,j]==a[i,j]) & (b[i,j+1]==a[i,j])){
        a[i,j]=0
      }
    }
  }
}

for (i in 2:(L[1]-1)){
  if (a[i,1]>0){
    if ((b[i-1,1]==a[i,1]) & (b[i,2]==a[i,1]) & (b[i+1,1]==a[i,1]) & (b[i,L[1]]==a[i,1])){
      a[i,1]=0
    }
  }
  if (a[i,L[1]]>0){
    if (b[(i-1),L[1]]==a[i,L[1]] & b[i,1]==a[i,L[1]] & b[(i+1),L[1]]==a[i,L[1]] & b[i,(L[1]-1)]==a[i,L[1]]){
      a[i,L[1]]=0
    }
  }
  if (a[1,i]>0){
    if (b[1,i-1]==a[1,i] & b[2,i]==a[1,i] & b[1,i+1]==a[1,i] & b[L[1],i]==a[1,i]){
      a[1,i]=0
    }
  }
  if (a[L[1],i]>0){
    if (b[L[1],i-1]==a[L[1],i] & b[L[1],i+1]==a[L[1],i] & b[L[1]-1,i]==a[L[1],i] & b[1,i]==a[L[1],i]){
      a[L[1],i]=0
    }
  }
}

if (a[1,1]>0){
  if (b[1,2]==a[1,1] && b[1,L[1]]==a[1,1] && b[2,1]==a[1,1] && b[L[1],1]==a[1,1]){
    a[1,1]=0;
  }
}
if (a[L[1],1]>0){
  if (b[L[1]-1,1]==a[L[1],1] && b[1,1]==a[L[1],1] && b[L[1],2]==a[L[1],1] && b[L[1],L[1]]==a[L[1],1]){
    a[L[1],1]=0
  }
}
if (a[1,L[1]]>0){
  if (b[1,1]==a[1,L[1]] && b[1,L[1]-1]==a[1,L[1]] && b[2,L[1]]==a[1,L[1]] && b[L[1],L[1]]==a[1,L[1]]){
    a[1,L[1]]=0
  }
}
if (a[L[1],L[1]]>0){
  if (b[L[1]-1,L[1]]==a[L[1],L[1]] && b[L[1],1]==a[L[1],L[1]] && b[L[1],L[1]-1]==a[L[1],L[1]] && b[1,L[1]]==a[L[1],L[1]]){
    a(L(1),L(1))=0
  }
}

uniq=unique(as.vector(a))
uniq=uniq[uniq>0]
for (i in uniq){
  periFT = c(periFT,length(which(a==i)))
  areaFT = c(areaFT,length(which(b==i)))
}
periFT=periFT[periFT>0]
areaFT=areaFT[areaFT>0]
patchindex=1:length(uniq)
output=list(PatchArea=areaFT,PatchPerimeter=periFT)
}
