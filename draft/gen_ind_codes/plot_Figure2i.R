plot_Figure2i <- function(DBplot1,DBplot2){
  ######################
  library(ggplot2)
  #Define multiplot function to create the grid of plots in the end
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  ###individualplots
  
  p1<-ggplot( data=DBplot1 , aes(DBplot1[,3],var_mean))+
    geom_line()+
    geom_ribbon(data=DBplot1,aes(ymin= var_mean - var_CI , ymax= var_mean + var_CI), alpha=0.3) +
    ggtitle(paste("Variance in transit 1.", colnames(MATRIX)[2], " =",SEL[1])) +
    labs(x = colnames(DBplot1[3])) +
    geom_line(data=DBplot1, aes(DBplot1[,3],nullvar_mean))+
    geom_ribbon(data=DBplot1,aes(ymin= nullvar_mean - nullvar_CI , ymax= nullvar_mean + nullvar_CI), fill = "red",alpha=0.3)
  
  p2<-ggplot( DBplot2 , aes(DBplot2[,3],var_mean))+
    geom_line()+
    geom_ribbon(data=DBplot2,aes(ymin= var_mean - var_CI , ymax= var_mean + var_CI), alpha=0.3) +
    ggtitle(paste("Variance in transit 2.", colnames(MATRIX)[2], " =",SEL[2])) +
    labs(x = colnames(DBplot2[3])) +
    geom_line(data=DBplot2, aes(DBplot2[,3],nullvar_mean))+
    geom_ribbon(data=DBplot2,aes(ymin= nullvar_mean - nullvar_CI , ymax= nullvar_mean + nullvar_CI), fill = "red",alpha=0.3)
  
  
  p3<-ggplot( DBplot1 , aes(DBplot1[,3],Skew_mean))+
    geom_line()+
    geom_ribbon(data=DBplot1,aes(ymin= Skew_mean - Skew_CI , ymax= Skew_mean + Skew_CI), alpha=0.3) +
    ggtitle(paste("Skewness in transit 1.", colnames(MATRIX)[2], " =",SEL[1])) +
    labs(x = colnames(DBplot1[3])) +
    geom_line(data=DBplot1, aes(DBplot1[,3],nullSkew_mean))+
    geom_ribbon(data=DBplot1,aes(ymin= nullSkew_mean - nullSkew_CI , ymax= nullSkew_mean + nullSkew_CI), fill = "red",alpha=0.3)
  
  
  p4<-ggplot( DBplot2 , aes(DBplot2[,3],Skew_mean))+
    geom_line()+
    geom_ribbon(data=DBplot2,aes(ymin= Skew_mean - Skew_CI , ymax= Skew_mean + Skew_CI), alpha=0.3) +
    ggtitle(paste("Skewness in transit 2.", colnames(MATRIX)[2], " =",SEL[2])) +
    labs(x = colnames(DBplot2[3])) +
    geom_line(data=DBplot2, aes(DBplot2[,3],nullSkew_mean))+
    geom_ribbon(data=DBplot2,aes(ymin= nullSkew_mean - nullSkew_CI , ymax= nullSkew_mean + nullSkew_CI), fill = "red",alpha=0.3)
  
  p5<-ggplot( DBplot1 , aes(DBplot1[,3],Moran_mean))+
    geom_line()+
    geom_ribbon(data=DBplot1,aes(ymin= Moran_mean - Moran_CI , ymax= Moran_mean + Moran_CI), alpha=0.3) +
    ggtitle(paste("Moran's I in transit 1.", colnames(MATRIX)[2], " =",SEL[1])) +
    labs(x = colnames(DBplot1[3])) +
    geom_line(data=DBplot1, aes(DBplot1[,3],nullMoran_mean))+
    geom_ribbon(data=DBplot1,aes(ymin= nullMoran_mean - nullMoran_CI , ymax= nullMoran_mean + nullMoran_CI), fill = "red",alpha=0.3)
  
  
  p6<-ggplot( DBplot2 , aes(DBplot2[,3],Moran_mean))+
    geom_line()+
    geom_ribbon(data=DBplot2,aes(ymin= Moran_mean - Moran_CI , ymax= Moran_mean + Moran_CI), alpha=0.3) +
    ggtitle(paste("Moran'I in transit 2.", colnames(MATRIX)[2], " =",SEL[2])) +
    labs(x = colnames(DBplot2[3])) +
    geom_line(data=DBplot2, aes(DBplot2[,3],nullMoran_mean))+
    geom_ribbon(data=DBplot2,aes(ymin= nullMoran_mean - nullMoran_CI , ymax= nullMoran_mean + nullMoran_CI), fill = "red",alpha=0.3)
  
  p7<-ggplot( DBplot1 , aes(DBplot1[,3],SDR_mean))+
    geom_line()+ scale_y_continuous(limits=c(0,2))+
    geom_ribbon(data=DBplot1,aes(ymin= SDR_mean - SDR_CI , ymax= SDR_mean + SDR_CI), alpha=0.3) +
    ggtitle(paste("SDR in transit 1.", colnames(MATRIX)[2], " =",SEL[1])) +
    labs(x = colnames(DBplot1[3])) +
    geom_line(data=DBplot1, aes(DBplot1[,3],nullSDR_mean))+
    geom_ribbon(data=DBplot1,aes(ymin= nullSDR_mean - nullSDR_CI , ymax= nullSDR_mean + nullSDR_CI), fill = "red",alpha=0.3)
  
  
  p8<-ggplot( DBplot2 , aes(DBplot2[,3],SDR_mean))+
    geom_line()+ scale_y_continuous(limits=c(0,3))+
    geom_ribbon(data=DBplot2,aes(ymin= SDR_mean - SDR_CI , ymax= SDR_mean + SDR_CI), alpha=0.3) +
    ggtitle(paste("SDR in transit 2.", colnames(MATRIX)[2], " =",SEL[2])) +
    labs(x = colnames(DBplot2[3])) +
    geom_line(data=DBplot2, aes(DBplot2[,3],nullSDR_mean))+
    geom_ribbon(data=DBplot2,aes(ymin= nullSDR_mean - nullSDR_CI , ymax= nullSDR_mean + nullSDR_CI), fill = "red",alpha=0.3)
  
  
  
  multiplot(p1,p3,p5,p7,p2,p4,p6,p8,cols=2)
  
  
}