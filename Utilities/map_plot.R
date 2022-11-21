
# Get the world map
  worldMap <- map_data("world")
  
# get colorscales
  bluegreen <- c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  yellowred <- c("#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026")
  purples   <- c("#f2f0f7", "#dadaeb","#bcbddc","#9e9ac8","#756bb1","#54278f") 
  blueorange <- c("#feb24c","#fed976","#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  sealand <- c("#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58")
  
  map_plot<-function(filename,var, year, colorchoice,ecoregion){
    filedata <- filename

    minlong <- round(min(filedata$longitude)-1)
    maxlong <- round(max(filedata$longitude)+1)
    minlat  <- round(min(filedata$latitude)-1)
    maxlat  <- round(max(filedata$latitude)+1)
    coordslim <- c(minlong,maxlong,minlat,maxlat)
    coordxmap <- round(seq(minlong,maxlong,length.out = 4))
    coordymap <- round(seq(minlat,maxlat,length.out = 4))
    
    pointsize <- max(20/((maxlat-minlat)^1.5),20/((maxlong-minlong)^1.5))
    
    #if (Assunit == "Ecoregion"){
    #  pointsize <- .15   
    #} else if (Assunit =="EEZ"){
    #  pointsize <- .7  
    #} else if (Assunit =="OSPAR_subregion"){
    #  pointsize <- .15  
    #}
    
    if (var == "surface_sar"){  
      YearNames<-c()
      for (i in 1:length(year)){
        YearNames <- c(YearNames,paste(var,year[i],sep="_"))
      }
      if (length(year) == 1){
        tr<-filedata[,c(YearNames)]
        yr <- paste("Surface abrasion \n" ,year[1])
      } else {
        tr<-filedata[,c(YearNames)]
        tr[is.na(tr)]<-0
        tr<-rowMeans(tr[,c(YearNames)])  
        tr[tr==0]<-NA
        yr <- paste("Surface abrasion \n" ,year[1],"-",year[length(year)])
      }
     
      quat<-c(-1,0,0.1,0.5,1,5,10,100)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      label_all <- c("-1,0","0-0.1", "0.1-0.5","0.5-1","1-5","5-10",">10")
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]
      
      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"no fishing")
      }
      
      colorchoice <- colorchoice[idx-1]
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
                          scale_colour_manual(values=colorchoice,na.value = "grey50",name  =yr,
                          labels=label_sub)
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
      return(figmap)
    
    } else if (var == "subsurface_sar"){
      
      YearNames<-c()
      for (i in 1:length(year)){
        YearNames <- c(YearNames,paste(var,year[i],sep="_"))
      }
      if (length(year) == 1){
        tr<-filedata[,c(YearNames)]
        yr <- paste("Subsurf abrasion" ,year[1])
      } else {
        tr<-filedata[,c(YearNames)]
        tr[is.na(tr)]<-0
        tr<-rowMeans(tr[,c(YearNames)])  
        tr[tr==0]<-NA
        yr <- paste("Subsurf abrasion" ,year[1],"-",year[length(year)])
      }
      
      quat<-c(-1,0,0.1,0.5,1,5,10,100)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      
      label_all <- c("-1,0","0-0.1", "0.1-0.5","0.5-1","1-5","5-10",">10")
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]
      
      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"no fishing")
      }
      
      colorchoice <- colorchoice[idx-1]
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  =yr,
                            labels=label_sub)
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
      return(figmap)
      
    } else if (var == "total_weight"){ 
      
      YearNames<-c()
      for (i in 1:length(year)){
        YearNames <- c(YearNames,paste(var,year[i],sep="_"))
      }
      if (length(year) == 1){
        tr<-filedata[,c(YearNames)]
        yr <- paste("Total weight (kg) \n" ,year[1])
      } else {
        tr<-filedata[,c(YearNames)]
        tr[is.na(tr)]<-0
        tr<-rowMeans(tr[,c(YearNames)])  
        tr[tr==0]<-NA
        yr <- paste("Total weight (kg) \n" ,year[1],"-",year[length(year)])
      }
      
      quat<-c(-1,0,100,1000,10000,100000,10000000)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      label_all <- c("-1-0","0-100    ", expression(100-10^3),expression(10^3-10^4),expression(10^4-10^5),expression(paste(">",10^5,"     ")))
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]
      
      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"no fishing")
      }
      
      colorchoice <- colorchoice[idx-1]
      
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  =yr,
                            labels=label_sub)
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
      return(figmap)  
    
    } else if (var == "medlong"){ 
      
      tr <- filedata$medlong
      
      quat<-c(0.1,2.5,3,3.5,4,5,6,8,1000000)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      filedata$cat[filedata$Depth< -200]<- NA
      label_all <- c("<2.5", "2.5-3","3-3.5","3.5-4","4-5","5-6","6-8",">8")
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]

      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"NA")
      }
      
      colorchoice <- colorchoice[idx]
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="Median longevity \n (years)",
                            labels=label_sub)
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
      return(figmap)  
        
    
    } else if (var == "total_value"){  
      
      YearNames<-c()
      for (i in 1:length(year)){
        YearNames <- c(YearNames,paste(var,year[i],sep="_"))
      }
      if (length(year) == 1){
        tr<-filedata[,c(YearNames)]
        yr <- paste("Total value (euros) \n" ,year[1])
      } else {
        tr<-filedata[,c(YearNames)]
        tr[is.na(tr)]<-0
        tr<-rowMeans(tr[,c(YearNames)])  
        tr[tr==0]<-NA
        yr <- paste("Total value (euros) \n" ,year[1],"-",year[length(year)])
      }
      
      quat<-c(-1,0,100,1000,10000,100000,10000000)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      label_all <- c("-1-0","0-100    ", expression(100-10^3),expression(10^3-10^4),expression(10^4-10^5),expression(paste(">",10^5,"     ")))
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]
      
      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"no fishing")
      }
      
      colorchoice <- colorchoice[idx-1]
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  =yr,
                            labels=label_sub)
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
      return(figmap)  
    
      
      } else if (var == "state"){  
        
        YearNames<-c()
        for (i in 1:length(year)){
          YearNames <- c(YearNames,paste(var,year[i],sep="_"))
        }
        if (length(year) == 1){
          tr<-filedata[,c(YearNames)]
          yr <- paste("State (PD model) \n" ,year[1])
        } else {
          tr<-filedata[,c(YearNames)]
          tr[is.na(tr)]<-0
          tr<-rowMeans(tr[,c(YearNames)])  
          tr[tr==0]<-NA
          yr <- paste("State (PD model) \n" ,year[1],"-",year[length(year)])
        }
        
      quat<-c(-1,0,0.1,0.2,0.3,0.5,0.7,1.01)
      filedata$cat<- as.factor(cut(tr,quat,right=TRUE))
      filedata$cat[filedata$Depth< -200]<- NA
      label_all <- c("0","0-0.1","0.1-0.2","0.2-0.3","0.3-0.5","0.5-0.7","0.7-1")
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]
      
      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"NA")
      }
      
      colorchoice <- colorchoice[idx-1]
          
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=c(colorchoice),na.value = "grey50",name  = yr,
                            labels=label_sub)
      figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
      figmap <- figmap +   theme(panel.background=element_blank(),
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
      return(figmap)  
      
    } else if (var == "impact"){  
    var <- "state"
    
    YearNames<-c()
    for (i in 1:length(year)){
      YearNames <- c(YearNames,paste(var,year[i],sep="_"))
    }
    if (length(year) == 1){
      tr<-filedata[,c(YearNames)]
      yr <- paste("Impact (PD method) \n" ,year[1])
    } else {
      tr<-filedata[,c(YearNames)]
      #tr[is.na(tr)]<-0
      tr<-rowMeans(tr[,c(YearNames)])  
      #tr[tr==0]<-NA
      yr <- paste("Impact (PD method) \n" ,year[1],"-",year[length(year)])
      
    }
    
    tr <- 1-tr ### calculate impact
    
    quat<-c(-1,0,0.1,0.2,0.3,0.5,0.8,1.01)
    filedata$cat<- as.factor(cut(tr,quat,right=TRUE))
    filedata$cat[filedata$Depth< -200]<- NA
    label_all <- c("0","0-0.1","0.1-0.2","0.2-0.3","0.3-0.5","0.5-0.8","0.8-1")
    idx <- which(!(table(filedata$cat)==0))
    label_sub <- label_all[idx]
    
    if((length(filedata$cat[is.na(filedata$cat)])>0)){
      label_sub <- c(label_sub,"NA")
    }
    
    colorchoice <- colorchoice[idx]
    
    figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
      scale_colour_manual(values=colorchoice,na.value = "grey50",name  =yr,
                          labels=label_sub)
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
    return(figmap)  
    
    } else if (var == "state_uncertainty"){  
      
      YearNames<-c()
      for (i in 1:length(year)){
        YearNames <- c(YearNames,paste(var,year[i],sep="_"))
      }
      if (length(year) == 1){
        tr<-filedata[,c(YearNames)]
        yr <- paste("Uncertainty (Q95-Q5) \n" ,year[1])
      } else {
        tr<-filedata[,c(YearNames)]
        #tr[is.na(tr)]<-0
        tr<-rowMeans(tr[,c(YearNames)])  
        #tr[tr==0]<-NA
        yr <- paste("Uncertainty (Q95-Q5) \n" ,year[1],"-",year[length(year)])
      }
      
      quat<-c(-1,0,0.1,0.2,0.3,0.5,0.8,1.01)
      filedata$cat<- as.factor(cut(tr,quat,right=TRUE))
      filedata$cat[filedata$Depth< -200]<- NA
      label_all <- c("0","0-0.1","0.1-0.2","0.2-0.3","0.3-0.5","0.5-0.8","0.8-1")
      idx <- which(!(table(filedata$cat)==0))
      label_sub <- label_all[idx]
      
      if((length(filedata$cat[is.na(filedata$cat)])>0)){
        label_sub <- c(label_sub,"NA")
      }
      
      colorchoice <- colorchoice[idx]
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=c(colorchoice),na.value = "grey50",name  = yr,
                            labels=label_sub)
      figmap <- figmap +  geom_polygon(data = worldMap, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
      figmap <- figmap +   theme(panel.background=element_blank(),
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
      return(figmap)  
    
  } else if (var == "impact_IL"){  
    var <- "state_IL"
    
    YearNames<-c()
    for (i in 1:length(year)){
      YearNames <- c(YearNames,paste(var,year[i],sep="_"))
    }
    if (length(year) == 1){
      tr<-filedata[,c(YearNames)]
      yr <- paste("Impact (L1 method) \n" ,year[1])
    } else {
      tr<-filedata[,c(YearNames)]
      #tr[is.na(tr)]<-0
      tr<-rowMeans(tr[,c(YearNames)])  
      #tr[tr==0] <- NA
      yr <- paste("Impact (L1 method) \n" ,year[1],"-",year[length(year)])
      
    }
    
    tr <- 1-tr ### calculate impact
    
    quat<-c(-1,0,0.1,0.2,0.3,0.5,0.8,1.01)
    filedata$cat<- as.factor(cut(tr,quat,right=TRUE))
    filedata$cat[filedata$Depth< -200]<- NA
    label_all <- c("0","0-0.1","0.1-0.2","0.2-0.3","0.3-0.5","0.5-0.8","0.8-1")
    idx <- which(!(table(filedata$cat)==0))
    label_sub <- label_all[idx]
    
    if((length(filedata$cat[is.na(filedata$cat)])>0)){
      label_sub <- c(label_sub,"NA")
    }
    
    colorchoice <- colorchoice[idx]
    
    figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
      scale_colour_manual(values=colorchoice,na.value = "grey50",name  =yr,
                          labels=label_sub)
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
    figmap <- figmap + guides(colour = guide_legend(override.aes = list(size=5)))
    return(figmap)  
  }  
    
  }
  
