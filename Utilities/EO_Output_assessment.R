
###### Figures and tables for ICES ecosystem overview

  # define directory to load figure and table data products
  pathdir_prodFT <- paste(pathdir_nogit,"Producing figures and tables",Assregion,AssYear,sep="/")

  # set directory for output
  setwd(paste(pathdir,"5 - Output",sep="/"))  
  dir.create(paste(Assregion))
  setwd(paste(pathdir,"5 - Output",Assregion,sep="/"))  
  dir.create(paste(AssYear))
  setwd(paste(pathdir,"5 - Output",Assregion,AssYear,sep="/"))
  
##### Figure EO - 1
  load(paste(pathdir_prodFT,"EO_Figure1.RData",sep="/"))

  sar    <- (map_plot(figEO1,"surface_sar",AssYear,purples,Assregion))
  impact <- (map_plot(figEO1,"impact",AssYear,sealand,Assregion))
  longevi <- (map_plot(figEO1,"medlong",AssYear,sealand,Assregion))
  uncert  <- (map_plot(figEO1,"state_uncertainty",AssYear,sealand,Assregion))
  
  png(paste(Assregion,AssYear,"EO_figure1.png",sep="_"),width=12,height=9, units = "in", res = 150 ) 
  print(grid.arrange(longevi,sar,impact,uncert, nrow = 2))
  dev.off()
  
##### table 1
  load(paste(pathdir_prodFT,"EO_table1_200.Rdata",sep="/"))
  write.csv(tab200, file= paste(Assregion,AssYear,"EO_table_200.csv",sep="_"), row.names=FALSE)
  load(paste(pathdir_prodFT,"EO_table1_800.Rdata",sep="/"))
  write.csv(tab800, file= paste(Assregion,AssYear,"EO_table_800.csv",sep="_"), row.names=FALSE)
  
##### Figure EO - 2
  load(paste(pathdir_prodFT,"EO_Figure2.RData",sep="/"))
  
  png(paste(Assregion,AssYear,"EO_figure2.png",sep="_"),width=8,height=4, units = "in", res = 150) 
  par(mfrow=c(1,3))
  
  #left panel
  ma <- round(max(figEO2[[1]][,1:5]))
  left<-as.data.frame(figEO2[[1]])
  plot(left[,1]~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,ma+0.5),
       ylab="Average intensity (y-1)",xlab="Year")
  lines(left[,2]~left$Year, col="red", type="o", lty=2)
  lines(left[,3]~left$Year, col="blue", type="o", lty=3)
  lines(left[,4]~left$Year, col="orange", type="o", lty=4)
  lines(left[,5]~left$Year, col="black", type="o", lty=5)
  axis(2,c(0,ma/2,ma),las=1)
  
  # middle panel
  middle<-as.data.frame(figEO2[[2]])
  plot((middle[,1])~left$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Average impact",xlab="Year")
  lines((middle[,2])~middle$Year, col="red", type="o", lty=2)
  lines((middle[,3])~middle$Year, col="blue", type="o", lty=3)
  lines((middle[,4])~middle$Year, col="orange", type="o", lty=4)
  lines((middle[,5])~middle$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  # right panel
  right<-as.data.frame(figEO2[[2]])
  plot(right[,1]~right$Year,type="o",col="black",lwd=2, pch=16, yaxt="n",ylim=c(0,1),
       ylab="Proportion habitat with impact < 0.2",xlab="Year")
  lines(right[,2]~right$Year, col="red", type="o", lty=2)
  lines(right[,3]~right$Year, col="blue", type="o", lty=3)
  lines(right[,4]~right$Year, col="orange", type="o", lty=4)
  lines(right[,5]~right$Year, col="black", type="o", lty=5)
  axis(2,c(0,0.5,1),las=1)
  
  legend(Period[1],1,legend=colnames(right[1:5]),bty = "n",
         col=c("black", "red", "blue","orange","black"), lty=1:5, cex=0.8, x.intersp=0.2,y.intersp = 0.8)
  
dev.off()