#This is the code for ForestGap model computation of spatial indicators
#It will compute, variance; skewness; Moran's I; and Power spectrum indicators. 

###########################

#Pre- loadings:
library(devtools) 
install_github('fdschneider/spatial_warnings')
library(spatialwarnings)
library(ggplot2)

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
  geom_raster(aes(x = delta, y = d), fill = "grey70",
              data = subset(lower_branch$DatBif, mean_cover_. < .05)) + 
  geom_contour(aes(x = delta, y = d, z = mean_cover_.), 
               data = subset(upper_branch$DatBif), 
               color = "black", linesize = .1) 


#######ONLY DO IF EXPLORING THE MODEL
# Let's extract a 1D bifurcation diagram over delta (d in x-axis). 
unydel<-unique(merged_results$delta)
a1=list()
for (i in 1:length(unydel)){
  data_subset <- subset(merged_results, delta==unydel[i])
  a1[[i]]<-ggplot(data_subset) + 
    geom_point(aes(d, mean_cover_., color = branch)) +
    ggtitle(paste("delta =",unydel[i]))
  print(a1[[i]])
  
}

####################################3duplicate through d (delta in x-axis)
#Do it over d (delta in x-axis)
unyd<-unique(merged_results$d)
a2=list()
for (i in 1:length(unyd)){
  data_subset <- subset(merged_results, d==unyd[i])
  a2[[i]]<-ggplot(data_subset) + 
    geom_point(aes(delta, mean_cover_., color = branch)) +
    ggtitle(paste("d =",unyd[i]))
  print(a2[[i]])
  
}
###########################################################################
###########################################################################


#Subset the transects of interest:
unydel<- unique(upper_branch$DatBif$delta)# re-run if you skipped the code above
unyd<- unique(upper_branch$DatBif$d)# re-run if you skipped the code above

deltaSEL<-unydel          #This means we want all delta values (delta will be x-axis)
dSEL<-c(unyd[2],unyd[9])  #We want d values contained in the second (hysteresis) and 
#ninth (continuous transition) unique values of d 
#(see bifurcation diagrams 1D plotted previously). 
#To see actual values type unyd[2] or unyd[9]


idx<-which(with(upper_branch$DatBif, d==dSEL & delta==deltaSEL))
Transits_dat<- subset(upper_branch$DatBif, d==dSEL & delta==deltaSEL)

landscapes<-'['(upper_branch[[2]],idx)
#Re-ordering landscapes to be only a list and not a list of a list
l=list()
n=1;
allo_temp<-c(1:(length(landscapes)*10))
Res_dat=data.frame(code=allo_temp,d=allo_temp, delta=allo_temp,replicate=allo_temp,cover=allo_temp)
for (i in 1:length(landscapes)){
  for (k in 1:10){
    l[[n]]<-landscapes[[i]][[k]]
    Res_dat$code[n]<-i
    Res_dat$d[n]<-Transits_dat$d[i]
    Res_dat$delta[n]<-Transits_dat$delta[i]
    Res_dat$replicate[n]<-k
    Res_dat$cover[n]<-Transits_dat$mean_cover_.[i]
    n=n+1
  }
}
Res_dat$code<-as.factor(Res_dat$code)
Res_dat<-indicators_ALL(l, Res_dat,subsize = 4, nreplicates = 5)


plot_Figure2i(Res_dat)


