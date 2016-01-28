#This is the code for ForestGap model computation of spatial indicators with d in x-axis
#It will compute, variance; skewness; Moran's I; and Power spectrum indicators. 

###########################

#Pre- loadings:
library(devtools) 
#install_github('fdschneider/spatial_warnings')
#library(spatialwarnings)
library(ggplot2)

setwd("~/Server_sync/sabiha_spews/mycodes")
load("result_grazing_processed.rda")

source("Sabiha_null_model_helpers.R")
#source("Sabiha_indicators_ALL.R")
source("Sabiha_Functions_separatedly.R")
source("plot_Figure2i.R")
source("Sabiha_reducedmatrix_ews.R")

#Select transects of interest (skip if already found)
merge_two_branches <- function(up, low) { 
  data.frame(branch = c(rep("upper", nrow(up$DatBif)), 
                        rep("lower", nrow(low$DatBif))), 
             rbind(up$DatBif, low$DatBif))
} 
merged_results <- merge_two_branches(upper_branch, lower_branch)

#######FIG 1.i
# Do a graph "à la flo" (hard to read for now, needs more tweaking)
ggplot() + 
  geom_raster(aes(x = m0, y = g), fill = "grey70",
              data = subset(lower_branch$DatBif, mean_cover_. < .05)) + 
  geom_contour(aes(x = m0, y = g, z = mean_cover_.), 
               data = subset(upper_branch$DatBif), 
               color = "black", linesize = .1) 


#######ONLY DO IF EXPLORING THE MODEL


####################################3duplicate through g (m0 in x-axis)
#Do it over g (m0 in x-axis)
unyg<-unique(merged_results$g)
a2=list()
for (i in 1:length(unyg)){
  data_subset <- subset(merged_results, g==unyg[i])
  a2[[i]]<-ggplot(data_subset) + 
    geom_point(aes(m0, mean_cover_., color = branch)) +
    ggtitle(paste("g =",unyg[i]))
  print(a2[[i]])
  
}
###########################################################################
###########################################################################


#Subset the transects of interest:
unyg<- unique(upper_branch$DatBif$g)# re-run if you skipped the code above
unym0<- unique(upper_branch$DatBif$m0)# re-run if you skipped the code above
gSEL<-c(0.0,0.175)

closest=function(x,y) y[which.min(abs(y-x))]
idx<-as.vector(sapply(gSEL,function (x,upper_branch) which(with(upper_branch$DatBif,g==closest(x,g))), upper_branch=upper_branch))
Transits_dat<- upper_branch$DatBif[idx,]

landscapes<-'['(upper_branch[[2]],idx)
#Re-ordering landscapes to be only a list and not a list of a list
l=list()
n=1;
allo_temp<-c(1:(length(landscapes)*10))
Res_dat=data.frame(code=allo_temp,g=allo_temp, m0=allo_temp, replicate=allo_temp,cover=allo_temp)
for (i in 1:length(landscapes)){
  for (k in 1:10){
    l[[n]]<-landscapes[[i]][[k]]
    Res_dat$code[n]<-i
    Res_dat$m0[n]<-Transits_dat$m0[i]
    Res_dat$g[n]<-Transits_dat$g[i]
    Res_dat$replicate[n]<-k
    Res_dat$cover[n]<-Transits_dat$mean_cover_.[i]
    n=n+1
  }
}
Res_dat$code<-as.factor(Res_dat$code)

## Define subsize for coarse-graining and the number of replicates
subsize=5; nreplicates=10;
#####################################################################################
#Get mean
#Meanlist<-indicator_mean(l,subsize = subsize,  nreplicates = nreplicates)
#MeanDF<-t(sapply(Meanlist,function(x) cbind(x[[1]],x[[2]])))

#Get variance:
######
#1. Compute variance:
Varlist<-indicator_variance(l,subsize = subsize,  nreplicates = nreplicates)
#2. Convert into a matrix:
VarDF<-t(sapply(Varlist,function(x) cbind(x[[1]],x[[2]])))
#3. Add to Result matrix:
Res_dat$var<-VarDF[,1]
Res_dat$null_var<-VarDF[,2]

##############
#Get Moran's I:

######
#1. Compute Moran's I:
Moranlist<-indicator_moran(l,subsize = subsize, nreplicates = nreplicates)
#2. Convert into a matrix:
MoranDF<-t(sapply(Moranlist,function(x) cbind(x[[1]],x[[2]])))
#3. Add to Result matrix:
Res_dat$moran<-MoranDF[,1]
Res_dat$null_moran<-MoranDF[,2]

##############
#Get Skewness:
######
#1. Compute Skewness:
Skewlist<-indicator_skewness(l,subsize = subsize, nreplicates = nreplicates)
#2. Convert into a matrix:
SkewDF<-t(sapply(Skewlist,function(x) cbind(x[[1]],x[[2]])))
#3. Add to Result matrix:
Res_dat$Skew<-SkewDF[,1]
Res_dat$null_Skew<-SkewDF[,2]

##############
#Get Spectrum:
######
#1. Compute Spectrum:
low_range<-c(1,0.2*floor((nrow(l[[1]])/2)))   ## small distance = high frequency
high_range<-c(0.8*floor(nrow(l[[1]])/2),floor(nrow(l[[1]])/2))
Pspeclist<-indicator_sdr(l,low_range = low_range,high_range = high_range,nreplicates = nreplicates)
#2. Convert into a matrix:
PspecDF<-t(sapply(Pspeclist,function(x) cbind(x[[1]],x[[2]])))
#3. Add to Result matrix:
Res_dat$SDR<-PspecDF[,1]
Res_dat$null_SDR<-PspecDF[,2]

#######################################################################################3



###Reshapping the data
#create a dataframe template without replicates
MATRIX<-subset(Res_dat, select = c(1,2,3,5))
MATRIX<-MATRIX[!duplicated(MATRIX),]
#Extract as matrix the indicators
fg<-data.matrix(Res_dat[,c(6:13)])
#Compute mean of replicates by combination of parameters
meanfg<-t(sapply(MATRIX$code,function(x,y,z) apply(y[z==x,],2,mean),y=fg,z=Res_dat$code))
#Compute CI of replicates by combination of parameters
fun=function(x) {if(sum(is.na(x))<8 && length(unique(x))>1){mean(x)-t.test(x)$conf.int[1]}else{NA}}
CIfg<-t(sapply(MATRIX$code,function(x,y,z) apply(y[z==x,],2,fun),y=fg,z=Res_dat$code))
#Adding variables to MATRIX
MATRIX$var_mean<-meanfg[,1]
MATRIX$var_CI<-CIfg[,1]
MATRIX$nullvar_mean<-meanfg[,2]
MATRIX$nullvar_CI<-CIfg[,2]

MATRIX$Moran_mean<-meanfg[,3]
MATRIX$Moran_CI<-CIfg[,3]
MATRIX$nullMoran_mean<-meanfg[,4]
MATRIX$nullMoran_CI<-CIfg[,4]

MATRIX$Skew_mean<-meanfg[,5]
MATRIX$Skew_CI<-CIfg[,5]
MATRIX$nullSkew_mean<-meanfg[,6]
MATRIX$nullSkew_CI<-CIfg[,6]

MATRIX$SDR_mean<-meanfg[,7]
MATRIX$SDR_CI<-CIfg[,7]
MATRIX$nullSDR_mean<-meanfg[,8]
MATRIX$nullSDR_CI<-CIfg[,8]

#Define the 2 transits from MATRIX
SEL=unique(MATRIX[,2])
DBplot1<-subset(MATRIX,MATRIX[,2]==SEL[1]) #database of transit 1
DBplot2<-subset(MATRIX,MATRIX[,2]==SEL[2]) #database of transit 2



plot_Figure2i(DBplot1,DBplot2)


remove(Res_dat,landscapes,Varlist,Skewlist,Moranlist,subsize,DBplot1,DBplot2,MATRIX,MoranDF,PspecDF,SkewDF,Pspeclist,dSEL,l)
