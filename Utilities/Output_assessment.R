
###### Figures and tables for the impact assessment following ICES Special Request Advice sr.2017.13

  # define directory to load figure and table data products
  pathdir_prodFT <- paste(pathdir_nogit,"Producing figures and tables",Assregion,AssYear,sep="/")

  # set directory for output
  setwd(paste(pathdir,"5 - Output",sep="/"))  
  dir.create(paste(Assregion))
  setwd(paste(pathdir,"5 - Output",Assregion,sep="/"))  
  dir.create(paste(AssYear))
  setwd(paste(pathdir,"5 - Output",Assregion,AssYear,sep="/"))
  
##### Figure A.1
  load(paste(pathdir_prodFT,"FigureA1.RData",sep="/"))

  sar    <- (map_plot(figA1,"surface_sar",AssYear,purples,Assregion))
  impact <- (map_plot(figA1,"impact",AssYear,bluegreen,Assregion))
  longevi <- (map_plot(figA1,"medlong",AssYear,sealand,Assregion))
  value  <- (map_plot(figA1,"total_value",AssYear,yellowred,Assregion))
  
  pdf(paste(Assregion,AssYear,"figureA1.pdf",sep="_"),width=12,height=9) 
  print(grid.arrange(sar,impact, longevi,value, nrow = 2))
  dev.off()
  
  jpeg(file = paste(Assregion,AssYear,"figureA1.jpeg",sep="_"), width=12, height=9,units ='in', res = 300)
  print(grid.arrange(sar,impact, longevi,value, nrow = 2))
  dev.off()
  
##### Figure A.2
  load(paste(pathdir_prodFT,"FigureA2.RData",sep="/"))
  minlong <- round(min(Habitat$longitude)-1)
  maxlong <- round(max(Habitat$longitude)+1)
  minlat  <- round(min(Habitat$latitude)-1)
  maxlat  <- round(max(Habitat$latitude)+1)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  figmap <- ggplot() + geom_point(data=Habitat, aes(x=longitude, y=latitude, colour=MSFD),shape=15,size=.05,na.rm=T)
  figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
  figmap <- figmap +  theme(plot.background=element_blank(),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=16),
                            axis.text.x   = element_text(size=16),
                            axis.title.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=11),
                            legend.title  = element_text(size=11))+
    scale_x_continuous(breaks=coordxmap)+
    scale_y_continuous(breaks=coordymap)+
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
    figmap <- figmap +   guides(colour = guide_legend(override.aes = list(size=5)))
    
    pdf(paste(Assregion,AssYear,"figureA2.pdf",sep="_"),width=12,height=6) 
    print(figmap)
    dev.off()

##### Figure A.3
  load(paste(pathdir_prodFT,"FigureA3.RData",sep="/"))
  
  sar        <- (map_plot(figA3,"surface_sar",AssYear,purples,Assregion))
  subsar     <- (map_plot(figA3,"subsurface_sar",AssYear,purples,Assregion))
 
  idx <- which(AssYear == AssPeriod)  
  nam <- paste("subsurface_sar",AssPeriod[-idx],sep="_")
  figA3[,nam][is.na(figA3[,nam])] <- 0
  figA3$dif_subsurface_sar <- figA3[,paste("subsurface_sar",AssYear,sep="_")] - apply(figA3[,nam], 1, mean)
  
  nam <- paste("surface_sar",AssPeriod[-idx],sep="_")
  figA3[,nam][is.na(figA3[,nam])] <- 0
  figA3$dif_surface_sar <- figA3[,paste("surface_sar",AssYear,sep="_")] - apply(figA3[,nam], 1, mean)
  
  minlong <- round(min(figA3$longitude)-1)
  maxlong <- round(max(figA3$longitude)+1)
  minlat  <- round(min(figA3$latitude)-1)
  maxlat  <- round(max(figA3$latitude)+1)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  quat<-c(-300,-2,-0.5,-0.1,0.1,0.5,2,300)
  figA3$cat<- as.factor(cut(figA3$dif_surface_sar,quat,right=T))
  label_all <- c("> -2", "-2 to -0.5", "-0.5 to -0.1","-0.1 to 0.1","0.1 to 0.5","0.5 to 2","> 2")
  idx <- which(!(table(figA3$cat)==0))
  label_sub <- label_all[idx]
  
  if((length(figA3$cat[is.na(figA3$cat)])>0)){
    label_sub <- c(label_sub,"no fishing")
  }
  
  colorchoice <- blueorange[idx]
  
  sdsar <- ggplot() + geom_point(data=figA3, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.05,na.rm=T)
  sdsar <- sdsar +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey") +
           scale_colour_manual(values=colorchoice,na.value = "grey50",name  = "Surface abrasion (dif)",
                        labels=label_sub)
  sdsar <- sdsar +  theme(plot.background=element_blank(),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=16),
                            axis.text.x   = element_text(size=16),
                            axis.title.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=11),
                            legend.title  = element_text(size=11))+
    scale_x_continuous(breaks=coordxmap)+
    scale_y_continuous(breaks=coordymap)+
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  sdsar <- sdsar +   guides(colour = guide_legend(override.aes = list(size=5)))
  
  figA3$cat<- as.factor(cut(figA3$dif_subsurface_sar,quat,right=T))
  label_all <- c("> -2", "-2 to -0.5", "-0.5 to -0.1","-0.1 to 0.1","0.1 to 0.5","0.5 to 2","> 2")
  idx <- which(!(table(figA3$cat)==0))
  label_sub <- label_all[idx]
  
  if((length(figA3$cat[is.na(figA3$cat)])>0)){
    label_sub <- c(label_sub,"no fishing")
  }
  
  colorchoice <- blueorange[idx]
  
  sdsubsar <- ggplot() + geom_point(data=figA3, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.05,na.rm=T)
  sdsubsar <- sdsubsar +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey") +
              scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="Subsurf abrasion (dif)",
                                  labels=label_sub)
  sdsubsar <- sdsubsar +  theme(plot.background=element_blank(),
                          panel.background=element_blank(),
                          axis.text.y   = element_text(size=16),
                          axis.text.x   = element_text(size=16),
                          axis.title.y  = element_text(size=16),
                          axis.title.x  = element_text(size=16),
                          panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                          legend.text   = element_text(size=11),
                          legend.title  = element_text(size=11))+
    scale_x_continuous(breaks=coordxmap)+
    scale_y_continuous(breaks=coordymap)+
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  sdsubsar <- sdsubsar +   guides(colour = guide_legend(override.aes = list(size=5)))

  pdf(paste(Assregion,AssYear,"figureA3.pdf",sep="_"),width=12,height=9) 
  print(grid.arrange(sar,sdsar, subsar,sdsubsar, nrow = 2))
  dev.off()
  
  jpeg(file = paste(Assregion,AssYear,"figureA3.jpeg",sep="_"), width=12, height=9,units ='in', res = 300)
  print(grid.arrange(sar,sdsar, subsar,sdsubsar, nrow = 2))
  dev.off()

##### Figure A.4
  load(paste(pathdir_prodFT,"FigureA4.RData",sep="/"))

  pdf(paste(Assregion,AssYear,"figureA4.pdf",sep="_"),width=8,height=4) 
  par(mfrow=c(1,3))
  
  #left panel
  ma <- round(max(A4fig[[1]][,1:5]))
  left<-as.data.frame(A4fig[[1]])
  plot(left[,1]~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,ma),
       ylab="Trawling intensity (y-1)",xlab="Year")
  lines(left[,2]~left$Year, col="red", type="o", lty=2)
  lines(left[,3]~left$Year, col="blue", type="o", lty=3)
  lines(left[,4]~left$Year, col="orange", type="o", lty=4)
  lines(left[,5]~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,ma/2,ma),las=1)

  # middle panel
  middle<-as.data.frame(A4fig[[2]])
  plot(middle[,1]~middle$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion fished",xlab="Year")
  lines(middle[,2]~middle$Year, col="red", type="o", lty=2)
  lines(middle[,3]~middle$Year, col="blue", type="o", lty=3)
  lines(middle[,4]~middle$Year, col="orange", type="o", lty=4)
  lines(middle[,5]~middle$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)

  # right panel
  right<-as.data.frame(A4fig[[3]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion fished with 90% effort",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  legend(Period[1],1,legend=colnames(right[1:5]),bty = "n",
         col=c("black", "red", "blue","orange","black"), lty=1:5, cex=0.8, x.intersp=2)

  dev.off()

# Figure A.5
  load(paste(pathdir_prodFT,"FigureA5.RData",sep="/"))
  
  pdf(paste(Assregion,AssYear,"figureA5.pdf",sep="_"),width=5.5,height=4.5) 
  plot(A5dat$sweptcumu~A5dat$indixcumu, xlab="Surface area \n(grid cells sorted from high to low trawling intensity)",
       ylab="Cumulative proportion",las=1,yaxt="n", lty=1, col="white", type="l")
  lines(x=c(-1,2),y=c(0.2,0.2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.4,0.4),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.6,0.6),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(-1,2),y=c(0.8,0.8),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.2,0.2),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.4,0.4),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.6,0.6),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  lines(x=c(0.8,0.8),y=c(-1,2),type="l",lty=4,lwd=0.2,col="light grey")
  
  lines(A5dat$sweptcumu~A5dat$indixcumu, lty=1, col="black",type="l")
  lines(A5dat$landcumu~A5dat$indixcumu, lty=2, col="blue",type="l")
  lines(A5dat$valuecumu~A5dat$indixcumu, lty=3, col="red",type="l")
  
  legend(0.5,0.8,legend=c("Swept area", "Landings","Value"),col=c("black","blue", "red"),lty=1:3, cex=0.8, 
         x.intersp=2,box.lty=0, bg=NULL)
  
  axis(2,c(0,0.2,0.4,0.6,0.8,1),las=1)
  dev.off()
  
# Figure A.6
# same as Figure A1
  
#Figure A.7
  load(paste(pathdir_prodFT,"FigureA7.RData",sep="/"))
  
  pdf(paste(Assregion,AssYear,"figureA7.pdf",sep="_"),width=7,height=5) 
  par(mfrow=c(1,2))
  
  #left panel
  left<-as.data.frame(A7fig[[1]])
  plot((1-left[,1])~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Impact (PD model)",xlab="Year")
  lines((1-left[,2])~left$Year, col="red", type="o", lty=2)
  lines((1-left[,3])~left$Year, col="blue", type="o", lty=3)
  lines((1-left[,4])~left$Year, col="orange", type="o", lty=4)
  lines((1-left[,5])~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  legend(Period[1],1,legend=colnames(right[1:5]),bty = "n",
         col=c("black", "red", "blue","orange","black"), lty=1:5, cex=0.8, x.intersp=0.2,y.intersp = 0.8)
  
  # right panel
  right<-as.data.frame(A7fig[[2]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion habitat with impact < 0.2",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  dev.off()

# Figure A.8
  load(paste(pathdir_prodFT,"FigureA8_A9.RData",sep="/"))
  
  dat<-cbind(A8_A9fig[,paste("state_OTCRU",AssYear,sep="_")],A8_A9fig[,paste("state_OTREST",AssYear,sep="_")],A8_A9fig[,paste("state_TBB",AssYear,sep="_")])
  dat <- 1-dat # get impact
  
  pdf(paste(Assregion,AssYear,"figureA8.pdf",sep="_"),width=5.5,height=4.5) 
  barplot(dat,beside=T,yaxt="n",xaxt="n",ylab="Impact (1-B/K)",ylim=c(0,0.4),xlab="Metier")
  legend("topleft",
         as.character(A8_A9fig$MSFD),
         fill = gray.colors(4),bty = "n")
  axis(1,c(3,8,13),c("OT_cru","OT_rest","TBB_all"))
  axis(2,c(0,0.2,0.4),las=1)
  box()
  dev.off()
  
# Figure A.9
  OT_CRU_col <- which(names(A8_A9fig) %in%  paste(rep("state_OTCRU",length(Period)),Period,sep="_"))
  OT_REST_col <- which(names(A8_A9fig) %in%  paste(rep("state_OTREST",length(Period)),Period,sep="_"))
  TBB_col <- which(names(A8_A9fig) %in%  paste(rep("state_TBB",length(Period)),Period,sep="_"))
  
  pdf(paste(Assregion,AssYear,"figureA9.pdf",sep="_"),width=6,height=6) 
  par(mfrow=c(2,2))
  
  plot(Period,1-A8_A9fig[1,OT_CRU_col],type="o",ylim=c(0,0.4),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[1,1]))
  lines(Period,1-A8_A9fig[1,OT_REST_col],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[1,TBB_col],lwd=2,col="blue",type="o")
  axis(1,2009:2016)
  axis(2,c(0,0.2,0.4),las=1)
  legend(x=Period[4],y=0.38,c("OT-cru","OT-rest","TBB-all"),col=c("black","red","blue"),
         lty=1,bty = "n",lwd=2,y.intersp=0.8)
  
  plot(Period,1-A8_A9fig[2,OT_CRU_col],type="o",ylim=c(0,0.4),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[2,1]))
  lines(Period,1-A8_A9fig[2,OT_REST_col],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[2,TBB_col],lwd=2,col="blue",type="o")
  axis(1,2009:2016)
  axis(2,c(0,0.2,0.4),las=1)
  
  plot(Period,1-A8_A9fig[3,OT_CRU_col],type="o",ylim=c(0,0.4),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[3,1]))
  lines(Period,1-A8_A9fig[3,OT_REST_col],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[3,TBB_col],lwd=2,col="blue",type="o")
  axis(1,2009:2016)
  axis(2,c(0,0.2,0.4),las=1)
  
  plot(Period,1-A8_A9fig[4,OT_CRU_col],type="o",ylim=c(0,0.4),xaxt="n",yaxt="n",xlab="year",
       ylab="Impact (1-B/K)",lwd=2, main=as.character(A8_A9fig[4,1]))
  lines(Period,1-A8_A9fig[4,OT_REST_col],lwd=2,col="red",type="o")
  lines(Period,1-A8_A9fig[4,TBB_col],lwd=2,col="blue",type="o")
  axis(1,2009:2016)
  axis(2,c(0,0.2,0.4),las=1)

  dev.off()

# Figure A.10
  load(paste(pathdir_prodFT,"FigureA10.RData",sep="/"))
  state    <- (map_plot(A10fig,"state",AssYear,blueorange,Assregion))
  avgstate <- (map_plot(A10fig,"state",AssPeriod,blueorange,Assregion))

  pdf(paste(Assregion,AssYear,"figureA10.pdf",sep="_"),width=12,height=5.5) 
  print(grid.arrange(state,avgstate, nrow = 1))
  dev.off()
  
  jpeg(file = paste(Assregion,AssYear,"figureA10.jpeg",sep="_"), width=12, height=5,units ='in', res = 300)
  print(grid.arrange(state,avgstate, nrow = 1))
  dev.off()
  

# Figure A.11
  # not included

# Figure A.12
#  load(paste(pathdir_prodFT,"FigureA12.RData",sep="/"))
  
#  pdf(paste(Assregion,AssYear,"figureA12.pdf",sep="_"),width=5.5,height=4.5) 
#  plot(A12fig$footprop~A12fig$state_1,lwd=2,type="l",xlab="Threshold of status (> B/K)",
#       ylab="Proportion of footprint", main="Proportion of c-squares status > B/K threshold",yaxt="n")
#  lines(A12fig$footprop~A12fig$state_0.25,lwd=1)
#  lines(A12fig$footprop~A12fig$state_0.5,lwd=1)
#  lines(A12fig$footprop~A12fig$state_0.75,lwd=1)
#  lines(A12fig$footprop~A12fig$state_1.25,lwd=1)
#  lines(A12fig$footprop~A12fig$state_2,lwd=1)
#  lines(A12fig$footprop~A12fig$state_4,lwd=1)
#  text(0.2,0.65,"4x")
#  text(0.9,0.9,"0.25x")
#  axis(2,c(0,0.5,1),las=1)
#  dev.off()
  
# Figure A.13
  load(paste(pathdir_prodFT,"FigureA13.RData",sep="/"))
  
  minlong <- round(min(A13fig$longitude)-1)
  maxlong <- round(max(A13fig$longitude)+1)
  minlat  <- round(min(A13fig$latitude)-1)
  maxlat  <- round(max(A13fig$latitude)+1)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  quat<-c(-1,0,100,1000,10000,100000,10000000)
  A13fig$cat<- as.factor(cut(A13fig$AVGvalue,quat,right=FALSE))
  
  A13fig <- subset(A13fig,A13fig$Depth > -200)
  
  figmap <- ggplot() + geom_point(data=A13fig, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.15,na.rm=T) +
    scale_colour_manual(values=yellowred,na.value = "white",name  ="Total value (euros)",
                        labels=c("0-100    ", expression(100-10^3),expression(10^3-10^4),expression(10^4-10^5)
                                 ,expression(paste(">",10^5,"     "))))
  figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
  figmap <- figmap +  theme(plot.background=element_blank(),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=16),
                            axis.text.x   = element_text(size=16),
                            axis.title.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=11),
                            legend.title  = element_text(size=11))+
    scale_x_continuous(breaks=coordxmap)+
    scale_y_continuous(breaks=coordymap)+
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
  figmap<- figmap +   guides(colour = guide_legend(override.aes = list(size=5)))
  
  # select all areas 5% 
  test <- subset(A13fig,A13fig$inout5 == 0)
  test$inout5 [test$inout5  == 0] <- NA
  figmap_05 <- figmap +  geom_point(data=test, aes(x=longitude, y=latitude, colour=factor(inout5)),na.rm=T,shape=15,size=.15)

  # select all areas 10% 
  test <- subset(A13fig,A13fig$inout10 == 0)
  test$inout10 [test$inout10  == 0] <- NA
  figmap_10 <- figmap +  geom_point(data=test, aes(x=longitude, y=latitude, colour=factor(inout10)),na.rm=T,shape=15,size=.15)

  pdf(paste(Assregion,AssYear,"figureA13.pdf",sep="_"),width=12,height=5.5) 
  print(grid.arrange(figmap_05,figmap_10, nrow = 1))
  dev.off()
  
  jpeg(file = paste(Assregion,AssYear,"figureA13.jpeg",sep="_"), width=12, height=5.5,units ='in', res = 300)
  print(grid.arrange(figmap_05,figmap_10, nrow = 1))
  dev.off()
  
####################################################
# Table A1
  load(paste(pathdir_prodFT,"TableA1.RData",sep="/"))
  col1 <- c("1. Intensity", "2. Proportion of cells fished", "3. Proportion of area fished", "4. Aggregation of fishing pressure",
          "5. Persistently unfished areas", "6. Average impact", "7. Proportion of area with impact < 0.2")
  A1table <- data.frame(Indicators = col1, values = A1table)
  write.csv(A1table, file= paste(Assregion,AssYear,"Table_1.csv",sep="_"), row.names=FALSE)

# Table A2 
# no data, not included

# Table A3
  load(paste(pathdir_prodFT,"TableA3.RData",sep="/"))
  colnames(A3table) <- c("MSFD habitat code","Extent of habitat (10^3 km^2)", "Landings 10^3 tonnes", "Value 10^6 euro",
                         "Number of grid cells","Swept area 10^3 km^2","Proportion of grid cells fished (indicator 2)",
                       "Proportion of area fished (indicator 3)","Fishing intensity per year (indicator 1)",
                       "Average impact (indicator 6)", "Proportion of habitat fished with 90% of effort (indicator 4)")
  write.csv(A3table, file= paste(Assregion,AssYear,"Table_3.csv",sep="_"), row.names=FALSE)
  
# Table A4
  load(paste(pathdir_prodFT,"TableA4.RData",sep="/"))
  col1 <- c("Area fished (10^3 km^2)", "Intensity (indicator 1)", "Aggregation of fishing pressure, smallest prop of grid cells with 90% effort (indicator 4)",
  "average impact (indicator 6)", "Landings 10^3 tonnes","Value 10^6 euro","Average impact/landings ratio (10^-2)",
  "Average impact/value ratio (10^-2)")
  A4table <- data.frame(Metier = col1, OT_CRU = A4table[,1], OT_REST = A4table[,2], TBB_ALL = A4table [,3])
  write.csv(A4table, file= paste(Assregion,AssYear,"Table_4.csv",sep="_"), row.names=FALSE)
  
# Table A5
  load(paste(pathdir_prodFT,"TableA5.RData",sep="/"))
  colnames(A5table) <- c("MSFD habitat code", "Total area km^2","Total value euro","Area with the least value constituting 5% of the swept area",
                       "Value of the lowest 5% area", "Fraction of total area with the least value constituting 5% of the swept area",
                       "Fraction of total value from the lowest 5% area","Area with the least value constituting 10% of the swept area",
                       "Value of the lowest 10% area", "Fraction of total area with the least value constituting 10% of the swept area",
                       "Fraction of total value from the lowest 10% area")
  write.csv(A5table, file= paste(Assregion,AssYear,"Table_5.csv",sep="_"), row.names=FALSE)
  