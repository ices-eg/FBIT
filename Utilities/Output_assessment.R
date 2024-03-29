
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
  impact <- (map_plot(figA1,"impact",AssYear,sealand,Assregion))
  longevi <- (map_plot(figA1,"medlong",AssYear,sealand,Assregion))
  value  <- (map_plot(figA1,"total_value",AssYear,yellowred,Assregion))
  
  #pdf(paste(Assregion,AssYear,"figureA1.pdf",sep="_"),width=12,height=9) 
  #print(grid.arrange(sar,impact, longevi,value, nrow = 2))
  #dev.off()
  
  png(paste(Assregion,AssYear,"figureA1.png",sep="_"),width=12,height=9, units = "in", res = 150 ) 
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
  
  nb.cols <- length(unique(Habitat$MSFD))
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  
  figmap <- ggplot() + geom_point(data=Habitat, aes(x=longitude, y=latitude, colour=MSFD),
                                  shape=15,size=.2,na.rm=T ) +  scale_colour_manual(values=mycolors,na.value = "grey50")
  figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
  figmap <- figmap +  theme(plot.background=element_blank(),
                            panel.background=element_blank(),
                            axis.text.y   = element_text(size=16),
                            axis.text.x   = element_text(size=16),
                            axis.title.y  = element_text(size=16),
                            axis.title.x  = element_text(size=16),
                            panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
                            legend.text   = element_text(size=6),
                            legend.title  = element_text(size=6))+
    scale_x_continuous(breaks=coordxmap)+
    scale_y_continuous(breaks=coordymap)+
    coord_cartesian(xlim=c(coordslim[1], coordslim[2]), ylim=c(coordslim[3],coordslim[4]))
    figmap <- figmap +   guides(colour = guide_legend(override.aes = list(size=5))) 
    
    png(paste(Assregion,AssYear,"figureA2.png",sep="_"),width=10,height=6, units = "in", res = 150 ) 
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
  
  sdsar <- ggplot() + geom_point(data=figA3, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.15,na.rm=T)
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
  
  sdsubsar <- ggplot() + geom_point(data=figA3, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.15,na.rm=T)
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

## only save surface abrasion
  png(paste(Assregion,AssYear,"figureA3.png",sep="_"),width=12,height=5.5, units = "in", res = 150 ) 
  print(grid.arrange(sar,sdsar, nrow = 1))
  dev.off()

##### Figure A.4
  load(paste(pathdir_prodFT,"FigureA4.RData",sep="/"))
  
  png(paste(Assregion,AssYear,"figureA4.png",sep="_"),width=8,height=4, units = "in", res = 150) 
  par(mfrow=c(1,3))
  
  #left panel
  ma <- round(max(A4fig[[1]][,1:5]))
  left<-as.data.frame(A4fig[[1]])
  plot(left[,1]~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,ma+0.5),
       ylab="I-1: Average trawling intensity (y-1)",xlab="Year")
  lines(left[,2]~left$Year, col="red", type="o", lty=2)
  lines(left[,3]~left$Year, col="blue", type="o", lty=3)
  lines(left[,4]~left$Year, col="orange", type="o", lty=4)
  lines(left[,5]~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,ma/2,ma),las=1)
  
  # middle panel
  middle<-as.data.frame(A4fig[[2]])
  plot(middle[,1]~middle$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="I-3: Proportion of area fished",xlab="Year")
  lines(middle[,2]~middle$Year, col="red", type="o", lty=2)
  lines(middle[,3]~middle$Year, col="blue", type="o", lty=3)
  lines(middle[,4]~middle$Year, col="orange", type="o", lty=4)
  lines(middle[,5]~middle$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  # right panel
  right<-as.data.frame(A4fig[[3]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="I-4: Smallest  prop. of area with 90% of effort",xlab="Year")
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
  
  png(paste(Assregion,AssYear,"figureA5.png",sep="_"),width=5.5,height=4.5, units = "in", res = 150) 
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
  
##### Figure A.6
  load(paste(pathdir_prodFT,"FigureA6.RData",sep="/"))
  
  impact <- (map_plot(figA6,"impact",AssYear,sealand,Assregion))
  
  idx <- which(AssYear == AssPeriod)  
  nam <- paste("state",AssPeriod[-idx],sep="_")
  #figA6[,nam][is.na(figA6[,nam])] <- 0
  figA6$dif_state <- figA6[,paste("state",AssYear,sep="_")] - apply(figA6[,nam], 1, mean)
  figA6$dif_impact <-  figA6$dif_state*(-1)
  
  minlong <- round(min(figA6$longitude)-1)
  maxlong <- round(max(figA6$longitude)+1)
  minlat  <- round(min(figA6$latitude)-1)
  maxlat  <- round(max(figA6$latitude)+1)
  coordslim <- c(minlong,maxlong,minlat,maxlat)
  coordxmap <- round(seq(minlong,maxlong,length.out = 4))
  coordymap <- round(seq(minlat,maxlat,length.out = 4))
  
  quat<-c(-300,-0.5,-0.25,-0.1,0.1,0.25,0.5,300)
  figA6$cat<- as.factor(cut(figA6$dif_impact,quat,right=T))
  label_all <- c("> -0.5", "-0.5 to -0.25", "-0.25 to -0.1","-0.1 to 0.1","0.1 to 0.25","0.25 to 0.5","> 0.5")
  idx <- which(!(table(figA6$cat)==0))
  label_sub <- label_all[idx]
  figA6$cat[figA6$Depth< -200]<- NA
  
  if((length(figA6$cat[is.na(figA6$cat)])>0)){
    label_sub <- c(label_sub,"NA")
  }
  
  colorchoice <- blueorange[idx]
  
  sdimpact <- ggplot() + geom_point(data=figA6, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.15,na.rm=T)
  sdimpact <- sdimpact +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey") +
    scale_colour_manual(values=colorchoice,na.value = "grey50",name  = "Impact (dif)",
                        labels=label_sub)
  sdimpact <- sdimpact +  theme(plot.background=element_blank(),
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
  sdimpact <- sdimpact +   guides(colour = guide_legend(override.aes = list(size=5)))
  
  png(paste(Assregion,AssYear,"figureA6.png",sep="_"),width=12,height=5.5, units = "in", res = 150 ) 
  print(grid.arrange(impact,sdimpact, nrow = 1))
  dev.off()
  
#Figure A.7
  load(paste(pathdir_prodFT,"FigureA7.RData",sep="/"))
  
  png(paste(Assregion,AssYear,"figureA7.png",sep="_"),width=7,height=5, units = "in", res = 150) 
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
  load(paste(pathdir_prodFT,"FigureA8.RData",sep="/"))
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  nam <- paste("state",gears[1],AssYear,sep="_")
  Avgear <- data.frame(A8[, nam])
  
  for (q in 2:length(gears)){
    nam <- paste("state",gears[q],AssYear,sep="_")
    Avgear <- cbind(Avgear,data.frame(A8[, nam]))
  }
  colnames(Avgear) <- gears
  
  Avgear <- t(Avgear)
  colnames(Avgear) <- A8[,1]
  Avgear <- 1-Avgear # get impact
  Avgear <- t(Avgear)

  png(paste(Assregion,AssYear,"figureA8.png",sep="_"),width=12,height=4.5, units = "in", res = 150) 
  par(mfrow=c(1,1),mar=c(4,5,2,2)+0.1)
  
  barplot(Avgear,beside=T,yaxt="n",xaxt="n",ylab="Impact (PD method)",ylim=c(0,0.3),xlab="Metier")
  legend(30,0.25, as.character(A8$MSFD),
         fill = gray.colors(4),bty = "n")
  axis(1,c(seq(3, 48, length = 10)),gears) 
  axis(2,c(0,0.15,0.3),las=1)
  box()
  dev.off()
  
  
####################################################
# Table A1
  load(paste(pathdir_prodFT,"TableA1.RData",sep="/"))
  col1 <- c("Average intensity (I-1)", "Proportion of area in fished cells (I-2)", "Proportion of area fished per year (I-3)", 
            "Smallest  prop. of area in fished cells with 90% of fishing effort (I-4)","Proportion of area in unfished cells (I-5)", 
            "Average PD impact (I-6)", "Proportion of area with PD impact < 0.2 (I-7)")
  A1table <- round(A1table, digits = 2)  
  A1table <- data.frame(Indicators = col1, values = A1table)
  colnames(A1table) <- c("Indicators","0 to 200 m","200 to 800 m", "more than 800 m") 
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    A1table[6:7,3:4] <- NA
  }
  
  write.csv(A1table, file= paste(Assregion,AssYear,"Table_1.csv",sep="_"), row.names=FALSE)

# Table A2 
  load(paste(pathdir_prodFT,"TableA2.RData",sep="/"))
  A2table <- A2table[,c(1:6,9,8,7,10)]
  
  colnames(A2table) <- c("MSFD broad habitat type","Extent of habitat (1000 km2)", "Number of grid cells",
                         "Landings 1000 tonnes","Value 10^6^ euro","Swept area 1000 km2","Average fishing intensity (I-1)",
                         "Prop. of area in fished grid cells (I-2)", "Prop. of area fished per year (I-3)",
                         "Smallest  prop. of area with 90% of fishing effort (I-4)")
  
  A2table[,c(2:10)] <- round(A2table[,c(2:10)], digits = 2)
  write.csv(A2table, file= paste(Assregion,AssYear,"Table_2.csv",sep="_"), row.names=FALSE)

# Table A3
  load(paste(pathdir_prodFT,"TableA3.RData",sep="/"))
  A3table[,c(1:10)] <- round(A3table[,c(1:10)], digits = 2)
  write.csv(A3table, file= paste(Assregion,AssYear,"Table_3.csv",sep="_"), row.names=FALSE)
  
# Table A4
  load(paste(pathdir_prodFT,"TableA4.RData",sep="/"))
  A4table[,c(1:10)] <- round(A4table[,c(1:10)], digits = 3)
  write.csv(A4table, file= paste(Assregion,AssYear,"Table_4.csv",sep="_"), row.names=FALSE)
  