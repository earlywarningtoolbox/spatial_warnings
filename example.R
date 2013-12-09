# example

# Data file having only one snapshot
# data=read.table(file='desertification_SDF_R0.91.txt',header=FALSE,sep='\t');

# Data file having many snapshots.
data=read.table(file='desertification_BW.txt',header=FALSE,sep='\t');

out=spatial_ews(data,discrete=FALSE,subsize=5,iter=20,nullmodel = 0)