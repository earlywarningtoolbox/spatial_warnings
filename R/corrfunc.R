corrfunc<-function(data){
L= dim(data)[1]#system size
b = rep(1:L,L)
c=cbind(b,sort(b))

d=numeric(length=L^2)
for(i in 1:(L*L)){
  d[i]=sqrt((c[i,1]-L/2)^2 + (c[i,2]-L/2)^2)
}
fullmat=cbind(c,d)
fullmat=fullmat[order(fullmat[,3]),]
dists=unique(fullmat[,3])


CoRmatNT=numeric(length=L/2)
amat=data
# matlabpool ('open',4)
meanf=mean(mean(amat))
varf= sum((amat-meanf)^2)/(L^2-1) #var(amat(:))
for (i in 0:(L/2)){
#indices = randi(L*L,500,1)
indices=1:L*L
indexi=ceiling(indices/L)
indexj=indices%%L#mod(indices,L)
indexj[indexj==0]=L

if (i==0){
submat=matrix(fullmat[fullmat[,3]<=i,],1,length(fullmat[fullmat[,3]<=i,]))
} else{
upt=which(fullmat[,3]<=i)
dwt=which(fullmat[,3]>i-1)
submat=fullmat[intersect(dwt,upt),]
}
r=nrow(submat)
cordump=numeric(length=length(indices))
for (idx in 1:length(indexi)){
counter=0
caltd=amat[indexi[idx],indexj[idx]]-meanf
for (j in 1:r){
rowid=submat[j,1]+indexi[idx]-L/2
colid=submat[j,2]+indexj[idx]-L/2
rowid=rowid%%L #mod[rowid,L]
colid=colid%%L #mod[colid,L]
if (rowid ==0){
rowid=L
}
if (colid==0){
colid=L
}
counter=counter+(caltd*(amat[rowid,colid]-meanf))
}
cordump[idx]=(counter/(r))/varf
#  disp(i)
# disp(idx)
}
CoRmatNT[i+1]=mean(cordump)
}
return(corrln=CoRmatNT)
}
