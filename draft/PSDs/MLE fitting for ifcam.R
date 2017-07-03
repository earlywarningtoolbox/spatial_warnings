setwd('./draft/PSDs')


# load('./.misc_data.RData', verbose = TRUE)

# Load code
# sapply(dir('./pli/', recursive = TRUE, full.names = TRUE, pattern = "*.R"), 
#        source)

# Load packages
library(plyr)
library(ggplot2)

# Load powerlaw-related dependencies
library(poweRlaw)
sapply(dir('./pli/', pattern = '*.R$', full = TRUE), source)

#Choose the folder with the patch size distribution vectors
Gallpatches <- dir(getwd(), recursive=TRUE, pattern = '*.csv$')
GVps <- lapply(Gallpatches,function(x){read.csv(x, header=TRUE)[ ,2]})





estimate_dist_params <- function(distrib_vec, disttype, xmin = NULL) { 
  dist <- disttype$new(distrib_vec)
  
  # Set xmin if unspecified
  if ( is.null(xmin) ) { 
    xmin <- estimate_xmin(dist)
  }  
  dist$setXmin(xmin)
  
  # Estimate PL parms
  parms_estimate <- estimate_pars(dist)
  dist$setPars(parms_estimate$pars)
  
  dist
}
  
replist <- function(l, n) { list(l)[rep(1, n)] } # same as rep() but list output

# We create and fit ...
# ... a power law
GpowAf <- llply(GVps, estimate_dist_params, displ,  .progress = 'time')
used_mins <- lapply(GpowAf, function(x) x$getXmin()) # retrieve minimas used in PL fit
# ... an exponential
GexpAf <- Map(estimate_dist_params, GVps, 
              disttype = replist(disexp, length(GVps)),
              xmin = used_mins)

# ... a lognormal
GLnmAf <- Map(estimate_dist_params, GVps, 
              disttype = replist(dislnorm, length(GVps)),
              xmin = used_mins)


# We also fit these three functions using the source code provided by 
#   Costal & al. We will need it for the pl with cutoff. 
GAfexp=list()
GAfpow=list()
GAflnorm=list()
GAfpowexp=list()
for (i in 1:length(GVps)){
  GAfexp[[i]]    <- exp.fit(GVps[[i]],      used_mins[[i]]) # exponential
  GAfpow[[i]]    <- pareto.fit(GVps[[i]],   used_mins[[i]]) # pareto ( == PL)
  GAflnorm[[i]]  <- lnorm.fit(GVps[[i]],    used_mins[[i]]) # log-normal
  GAfpowexp[[i]] <- powerexp.fit(GVps[[i]], used_mins[[i]]) # power law with exponential cut-off
}


sink("GRAZINGpsdfit.txt", append=TRUE, split=TRUE) 
for(i in 1:length(GVps)){
#   print(paste("Grazing model g",rev(gvec)[i]," m", rev(mvec)[i], sep=""))
  
  
  ## Compare distributions using the poweRlaw package for all but PLEXP
  
  # We compare PL and EXP
  com=compare_distributions(GpowAf[[i]], GexpAf[[i]])
  print("com$p_two_sided")
  print(com$p_two_sided )
  # p = 1 implies that both distributions are equally far from the data
  # p = 0 implies that one of the distributions fits the data significantly better
  print("com$p_one_sided")
  # p = 1 implies that the first distribution is the better fit
  # p = 0 implies that the first distribution can be rejected in favour of the second
  print(com$p_one_sided)
  
  # We compare PL and LN
  com=compare_distributions(GpowAf[[i]], GLnmAf[[i]]) 
  print("com$p_two_sided ")
  print(com$p_two_sided)
  # p = 1 implies that both distributions are equally far from the data
  # p = 0 implies that one of the distributions fits the data significantly better
  print("com$p_one_sided") 
  print(com$p_one_sided)
  # p = 1 implies that the first distribution is the better fit
  # p = 0 implies that the first distribution can be rejected in favour of the second
  
  # We compare EXP and LN
  com=compare_distributions(GexpAf[[i]], GLnmAf[[i]]) 
  print("com$p_two_sided ")
  print(com$p_two_sided )
  # p = 1 implies that both distributions are equally far from the data
  # p = 0 implies that one of the distributions fits the data significantly better
  print("com$p_one_sided") 
  print(com$p_one_sided)
  # p = 1 implies that the first distribution is the better fit
  # p = 0 implies that the first distribution can be rejected in favour of the second
  
  
  
  
  ## Compare distributions using the pli source code for PLEXP
  
  # We compare PLEXP and PL
  print("power.powerexp.lrt(GAfpow[[i]],GAfpowexp[[i]])")
  print(power.powerexp.lrt(GAfpow[[i]], GAfpowexp[[i]]) )# compare PL and PLEXP
  # gives only a single p-value. If p = 0, powerexp is better
  
  # We compare PLEXP and EXP
  print("exp.powerexp.lrt(GAfexp[[i]],GAfpowexp[[i]])")
  print(exp.powerexp.lrt(GAfexp[[i]], GAfpowexp[[i]]))
  
  # We compare PLEXP and LNORM
  # 
  # If it is an underlying exponential distribution (extreme case), powerexp will be chosen for
  # large sample sizes. In such a case, the rate of decline will be similar
  print("vuong(lnorm.powerexp.llr(GVps[[i]], GAflnorm[[i]], GAfpowexp[[i]], Gminx[[i]])")
  print(vuong(lnorm.powerexp.llr(GVps[[i]], GAflnorm[[i]], GAfpowexp[[i]], used_mins[[i]]))) # compare lnorm and powerexp
  # 2 p values similar to the previous analysis
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
