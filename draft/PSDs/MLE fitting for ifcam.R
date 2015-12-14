#Choose the folder with the patch size distribution vectors
Gallpatches<-list.files(getwd(),recursive=TRUE)
Gallpatches1<-c(Gallpatches[9:12],Gallpatches[1:4],Gallpatches[5:8])
Gallpatches<-Gallpatches1
rm(Gallpatches1)
GVps<-lapply(Gallpatches,function(x){read.csv(x,header=TRUE)[,2]})


GpowAf = lapply(GVps,function(x){displ$new(as.vector(t(x)))})
Gminx = lapply(GpowAf,function(x){estimate_xmin(x)})
for(i in 1:length(GVps)){GpowAf[[i]]$setXmin(Gminx[[i]])}
GestPAf=lapply(GpowAf,function(x){estimate_pars(x)})
for(i in 1:length(GVps)){GpowAf[[i]]$setPars(GestPAf[[i]]$pars)}

GexpAf = lapply(GVps,function(x){disexp$new(as.vector(t(x)))})
for(i in 1:length(GVps)){GexpAf[[i]]$setXmin(GpowAf[[i]]$getXmin())}
GestEAf=lapply(GexpAf,function(x){estimate_pars(x)})
for(i in 1:length(GVps)){GexpAf[[i]]$setPars(GestEAf[[i]]$pars)}

GLnmAf = lapply(GVps,function(x){dislnorm$new(as.vector(t(x)))})
for(i in 1:length(GVps)){GLnmAf[[i]]$setXmin(GpowAf[[i]]$getXmin())}
GestLNAf=lapply(GLnmAf,function(x){estimate_pars(x)})
for(i in 1:length(GVps)){GLnmAf[[i]]$setPars(GestLNAf[[i]]$pars)}

GAfexp=list()
GAfpow=list()
GAflnorm=list()
GAfpowexp=list()
for (i in 1:length(GVps)){
  GAfexp[[i]] = exp.fit(GVps[[i]],Gminx[[i]]$xmin) # exponential
  GAfpow[[i]] = pareto.fit(GVps[[i]],Gminx[[i]]$xmin) # power law
  GAflnorm[[i]] = lnorm.fit(GVps[[i]],Gminx[[i]]$xmin) # log-normal
  GAfpowexp[[i]] = powerexp.fit(GVps[[i]],Gminx[[i]]$xmin) # power law with exponential cut-off
}

sink("GRAZINGpsdfit.txt", append=TRUE, split=TRUE) 
for(i in 1:length(GVps)){
  print(paste("Grazing model g",rev(gvec)[i]," m", rev(mvec)[i], sep=""))
  ######
  com=compare_distributions(GpowAf[[i]], GexpAf[[i]])
  print("com=compare_distributions(GpowAf[[i]], GexpAf[[i]])")
  print("com$test_statistic")
  print(com$test_statistic) # vuong test statistic (pvals calculated from this)
  print("com$p_two_sided")
  print(com$p_two_sided )
  # p = 1 implies that both distributions are equally far from the data
  # p = 0 implies that one of the distributions fits the data significantly better
  print("com$p_one_sided")
  print(com$p_one_sided)
  cat("========================================\n")
  com=compare_distributions(GpowAf[[i]], GLnmAf[[i]]) 
  print("com=compare_distributions(GpowAf[[i]], GLnmAf[[i]]) ")
  print("com$test_statistic")
  print(com$test_statistic) # vuong test statistic (pvals calculated from this)
  print("com$p_two_sided ")
  print(com$p_two_sided)
  # p = 1 implies that both distributions are equally far from the data
  # p = 0 implies that one of the distributions fits the data significantly better
  print("com$p_one_sided") 
  print(com$p_one_sided)
  # p = 1 implies that the first distribution is the better fit
  # p = 0 implies that the first distribution can be rejected in favour of the second
  cat("========================================\n")
  com=compare_distributions(GexpAf[[i]], GLnmAf[[i]]) 
  print("com=compare_distributions(GexpAf[[i]], GLnmAf[[i]]) ")
  print("com$test_statistic")
  print(com$test_statistic) # vuong test statistic (pvals calculated from this)
  print("com$p_two_sided ")
  print(com$p_two_sided )
  # p = 1 implies that both distributions are equally far from the data
  # p = 0 implies that one of the distributions fits the data significantly better
  print("com$p_one_sided") 
  print(com$p_one_sided)
  # p = 1 implies that the first distribution is the better fit
  # p = 0 implies that the first distribution can be rejected in favour of the second
  cat("========================================\n")
  print("power.powerexp.lrt(GAfpow[[i]],GAfpowexp[[i]])")
  print(power.powerexp.lrt(GAfpow[[i]],GAfpowexp[[i]]) )# compare pareto and powerexp
  # gives only a single p-value. If p = 0, powerexp is better
  print("exp.powerexp.lrt(GAfexp[[i]],GAfpowexp[[i]])")
  print(exp.powerexp.lrt(GAfexp[[i]],GAfpowexp[[i]]))
  # If it is an underlying exponential distribution (extreme case), powerexp will be chosen for
  # large sample sizes. In such a case, the rate of decline will be similar
  print("vuong(lnorm.powerexp.llr(GVps[[i]], GAflnorm[[i]], GAfpowexp[[i]], Gminx[[i]])")
  print(vuong(lnorm.powerexp.llr(GVps[[i]], GAflnorm[[i]], GAfpowexp[[i]], Gminx[[i]]$xmin))) # compare lnorm and powerexp
  # 2 p values similar to the previous analysis
  cat("=======================================================================\n")
  cat("=======================================================================\n")
}



# Plot disctrete fits
#names=character(length=length(GVps))
for (u in 1:length(GVps)){
  #names[u]<-paste("Non-Grass Vegetation Plot",u,sep="")
  dat = plot(GpowAf[[u]])
  po = lines(GpowAf[[u]])
  ex = lines(GexpAf[[u]])
  lno = lines(GLnmAf[[u]])
  
  fin = rbind(po,ex,lno)
  fin$type = c(rep("Power law",length(po$x)),rep("Exponential",length(ex$x)),rep("Log-normal",length(lno$x)))
  
  ggp = ggplot(dat, aes(x=x, y=y))  +
    geom_point(pch = 16, col = "black") +
    geom_line(data=fin, aes(x = x, y = y, col = type)) +
    #stat_smooth(data = fin, aes(x = x, y = y, col = type), se = F) +
    xlab("Patch Size") +
    ylab("Inverse CDF") +
    scale_colour_hue(name="Distribution", 
                     breaks=c("Exponential","Log-normal","Power law"),
                     labels=c("Exponential", "Log-normal", "Power law"),
                     l=40) +
    theme_bw() 
  ggp<-ggp + 
    ggtitle(paste("Grazing model g",rev(gvec)[u]," m",rev(mvec)[u],sep="")) +
    theme(plot.title=element_text(size = 24, face="bold")) +
    theme(axis.title.x = element_text(vjust = 0.3, size = 24, face = "bold"), axis.text.x = element_text(size = 22), axis.title.y = element_text(face = "bold", vjust = 0.3, angle = 90, size = 24), axis.text.y = element_text(size = 22)) +
    theme(legend.title = element_text(size = 22, face = "bold"), legend.text = element_text(size = 22))+
    theme(legend.justification=c(1,1), legend.position=c(0.95,0.95)) +
    #ggtitle(paste("Non-Grass Vegetation Plot",u,sep="")) +
    scale_x_log10()+
    scale_y_log10(breaks = c(0.0001,0.001,0.01,0.1,1), limits = c(0.0001,1.01)) #+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )
  #print(ggp)
  ggsave(filename=paste("GrazingPSD_g",rev(gvec)[u],"m",rev(mvec)[u],".jpg",sep=""), ggp, scale = 0.8)
  dev.off()
}
