
# set the folder
  dir.create(paste(pathdir_nogit,"/",EcoReg,sep=""))
  dir.create(paste(pathdir_nogit,"/",EcoReg,"/",datacall-1,sep=""))

# load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"region_grid_sensitivity.RData",sep="_"),sep="/")) 

# load total fishing data
   load(paste(pathdir_nogit,paste("fisheries_FBIT_VMSdatacall",datacall,".RData", sep=""),sep="/"))

  loopdata <- data.frame(Region@data$csquares)
  colnames(loopdata) <- "csquares"
  
  for(i in 1:length(Period)){
    Total <- subset(Fisheries,Fisheries$year == Period[i])
    colnames(Total)[3:6] <- paste(colnames(Total)[3:6], Period[i], sep = "_")
    loopdata <- cbind(loopdata, Total[match(loopdata$csquares,Total$c_square), c(3:6)])
  }
  
  Fisheries <- loopdata
  rownames(Fisheries) <- NULL
  setwd(paste(pathdir_nogit,"/",EcoReg,"/",datacall-1,sep=""))
  save(Fisheries,file=paste(EcoReg,"fisheries.RData",sep="_"))
  
# load data per metier
  load(paste(pathdir_nogit,paste("fisheries_metier_FBIT_VMSdatacall",datacall,".RData", sep=""),sep="/"))
  
  
  loopdata <- data.frame(Region@data$csquares)
  colnames(loopdata) <- "csquares"
  
  metier <- c("DRB_MOL","OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF","OT_DMF","OT_MIX","OT_MIX_DMF_BEN",
              "OT_MIX_DMF_PEL","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  metier_name <- metier
  
  for (i in 1: length(Period)){
    for (p in 1:length(metier)){
      Total <- subset(FisheriesMet,FisheriesMet$year == Period[i] & FisheriesMet$gear_category == metier[p])
      colnames(Total)[4:7] <- paste(metier_name[p],colnames(Total)[4:7], Period[i], sep = "_")
      loopdata <- cbind(loopdata, Total[match(loopdata$csquares,Total$c_square), c(4:7)])
    }}
      
FisheriesMet <- loopdata
save(FisheriesMet,file=paste(EcoReg,"fisheries_per_metier.RData",sep="_"))

##################################
#### now combine different metiers following TRADE3 document
####  

### combine OT_MIX
groups <- c("surface_sar","subsurface_sar","total_weight","total_value")
year   <- Period
combine <- c("OT_MIX","OT_MIX_DMF_BEN","OT_MIX_DMF_PEL")

sar    <- paste(combine[1],"surface_sar",year,sep="_")
ssar   <- paste(combine[1],"subsurface_sar",year,sep="_")
weight <- paste(combine[1],"total_weight",year,sep="_")
value  <- paste(combine[1],"total_value",year,sep="_")

for (i in 1:length(year)){
  nam <- paste(combine,groups[1],year[i], sep="_")
  FisheriesMet[,sar[i]] <- tt <- rowSums(FisheriesMet[,nam],na.rm=T)
  nam <- paste(combine,groups[2],year[i], sep="_")
  FisheriesMet[,ssar[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  nam <- paste(combine,groups[3],year[i], sep="_")
  FisheriesMet[,weight[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  nam <- paste(combine,groups[4],year[i], sep="_")
  FisheriesMet[,value[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
}

exclude <- c(paste(combine[2],"surface_sar",year,sep="_"),paste(combine[2],"subsurface_sar",year,sep="_"),
             paste(combine[2],"total_weight",year,sep="_"),  paste(combine[2],"total_value",year,sep="_"),
             paste(combine[3],"surface_sar",year,sep="_"),paste(combine[3],"subsurface_sar",year,sep="_"),
             paste(combine[3],"total_weight",year,sep="_"),  paste(combine[3],"total_value",year,sep="_"))
indexcol <- which(names(FisheriesMet) %in% exclude)
FisheriesMet <- FisheriesMet[,-indexcol]

### combine OT_CRU
combine <- c("OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF")

sar    <- paste(combine[1],"surface_sar",year,sep="_")
ssar   <- paste(combine[1],"subsurface_sar",year,sep="_")
weight <- paste(combine[1],"total_weight",year,sep="_")
value  <- paste(combine[1],"total_value",year,sep="_")

for (i in 1:length(year)){
  nam <- paste(combine,groups[1],year[i], sep="_")
  FisheriesMet[,sar[i]] <- tt <- rowSums(FisheriesMet[,nam],na.rm=T)
  nam <- paste(combine,groups[2],year[i], sep="_")
  FisheriesMet[,ssar[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  nam <- paste(combine,groups[3],year[i], sep="_")
  FisheriesMet[,weight[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  nam <- paste(combine,groups[4],year[i], sep="_")
  FisheriesMet[,value[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
}

exclude <- c(paste(combine[2],"surface_sar",year,sep="_"),paste(combine[2],"subsurface_sar",year,sep="_"),
             paste(combine[2],"total_weight",year,sep="_"),  paste(combine[2],"total_value",year,sep="_"),
             paste(combine[3],"surface_sar",year,sep="_"),paste(combine[3],"subsurface_sar",year,sep="_"),
             paste(combine[3],"total_weight",year,sep="_"),  paste(combine[3],"total_value",year,sep="_"))
indexcol <- which(names(FisheriesMet) %in% exclude)
FisheriesMet <- FisheriesMet[,-indexcol]

FisheriesMet[,2:ncol(FisheriesMet)][FisheriesMet[,2:ncol(FisheriesMet)] == 0] <- NA

save(FisheriesMet,file=paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"))
