  
  # Get the world map
  worldMap <- map_data("world")
  
  # get colorscales
  bluegreen <- c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  yellowred <- c("#ffffb2","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026")
  purples   <- c("#f2f0f7", "#dadaeb","#bcbddc","#9e9ac8","#756bb1","#54278f") 
  
  map_plot<-function(filename,var,colorchoice){
    filedata <- as.data.frame(filename)
    filedata2<-data.frame(longitude = coordinates(filename)[,1], latitude = coordinates(filename)[,2])
    filedata<-cbind(filedata2,filedata)
    
    minlong <- round(filename@bbox[1,1]-1)
    maxlong <- round(filename@bbox[1,2]+1)
    minlat  <- round(filename@bbox[2,1]-1)
    maxlat  <- round(filename@bbox[2,2]+1)
    coordslim <- c(minlong,maxlong,minlat,maxlat)
    coordxmap <- round(seq(minlong,maxlong,length.out = 4))
    coordymap <- round(seq(minlat,maxlat,length.out = 4))
    
    pointsize <- 0.3
    
    if (var == "medLong"){  
      filedata$cat <- round(filedata[,c(var)])
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="median longevity")
      figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
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
      tr <- filedata[,(var)]
      quat<-c(-1,0,0.1,0.3,0.5,0.7,0.9,1.01)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=rev(colorchoice),na.value = "grey50",name  ="State (PD model)",
                            labels=c("0-0.1","0.1-0.3","0.3-0.5","0.5-0.7","0.7-0.9","0.9-1"))
      figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
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
    
    } else if (var == "SurfaceSAR"){
      tr <- filedata[,(var)]
      quat<-c(-1,0,0.1,0.5,1,5,10,100)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="Surf abrasion",
                            labels=c("0-0.1", "0.1-0.5","0.5-1","1-5","5-10",">10","no fishing"))
      figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
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
  
    } else if (var == "Subsurface"){
      
      tr <- filedata[,(var)]
      quat<-c(-1,0,0.1,0.5,1,5,10,100)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="Subsurf abrasion",
                            labels=c("0-0.1", "0.1-0.5","0.5-1","1-5","5-10",">10","no fishing"))
      figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
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
      
    } else if (var == "totweight"){ 
      tr <- filedata[,(var)]
      quat<-c(-1,0,100,1000,10000,100000,10000000)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="Total weight (kg)",
                            labels=c("0-100    ", expression(100-10^3),expression(10^3-10^4),expression(10^4-10^5),expression(paste(">",10^5,"     ")),"no fishing"))
      figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
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
      
    } else if (var == "totvalue"){  
      
      tr <- filedata[,(var)]
      quat<-c(-1,0,100,1000,10000,100000,10000000)
      filedata$cat<- as.factor(cut(tr,quat,right=FALSE))
      
      figmap <- ggplot() + geom_point(data=filedata, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=pointsize,na.rm=T) +
        scale_colour_manual(values=colorchoice,na.value = "grey50",name  ="Total value (euros)",
                            labels=c("0-100    ", expression(100-10^3),expression(10^3-10^4),expression(10^4-10^5),expression(paste(">",10^5,"     ")),"no fishing"))
      figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")
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
      
       }
 }
  