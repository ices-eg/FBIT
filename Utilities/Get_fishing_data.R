
# load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"region_grid_sensitivity.RData",sep="_"),sep="/")) 

# get total surface and subsurface abrasion, weight and value of landings per year
  loopdata <- data.frame(Region@data$csquares)
  colnames(loopdata) <- "csquares"

  for (i in 1: length(Period)){
    Total   <- icesVMS::get_wgfbit_data1(EcoReg, Period[i])
    Total  <- aggregate(Total[, 1:5], by= list(Total$c_square), FUN=function(x){sum(x, na.rm=T)})
    colnames(Total)[2:6] <- paste(colnames(Total)[2:6], Period[i], sep = "_")
    loopdata <- cbind(loopdata, Total[match(loopdata$csquares,Total$Group.1), c(2:6)])
  }

  Fisheries <- loopdata
  setwd(pathdir_nogit)
  save(Fisheries,file=paste(EcoReg,"fisheries.RData",sep="_"))

# get fishing data specified per metier
  loopdata <- data.frame(Region@data$csquares)
  colnames(loopdata) <- "csquares"
  
  metier <- c("DRB_MOL","OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF","OT_DMF","OT_MIX","OT_MIX_DMF_BEN",
              "OT_MIX_DMF_PEL","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  metier_name <- metier
  
  for (i in 1: length(Period)){
    for (p in 1:length(metier)){
      Gear   <- icesVMS::get_wgfbit_data1(EcoReg, Period[i], benthis_metier = metier[p])
      
      if(length(Gear)>0){
        Gear  <- aggregate(Gear[, 1:5], by= list(Gear$c_square), FUN=function(x){sum(x, na.rm=T)})  
        colnames(Gear)[2:6] <- paste(metier_name[p],colnames(Gear)[2:6],Period[i],sep="_")
        loopdata<-cbind(loopdata, Gear[match(loopdata$csquares,Gear$Group.1), c(2:6)])
      } else {
        unavail <-  matrix(data = NA, ncol=5,nrow =nrow(loopdata))
        colnames(unavail) <- paste(metier_name[p],c("surface_sar", "subsurface_sar", "total_weight", "total_value", "mw_fishinghours"),Period[i],sep="_")
        loopdata <- cbind(loopdata,unavail)
      }
    }
  }

  FisheriesMet <- loopdata
  setwd(pathdir_nogit)
  save(FisheriesMet,file=paste(EcoReg,"fisheries_per_metier.RData",sep="_"))
  
  ##################################
  #### now combine different metiers following TRADE3 document
  ####  
  
  ### combine OT_MIX
  groups <- c("surface_sar","subsurface_sar","total_weight","total_value","mw_fishinghours")
  year   <- c(2009:2018)
  combine <- c("OT_MIX","OT_MIX_DMF_BEN","OT_MIX_DMF_PEL")
  
  sar    <- paste(combine[1],"surface_sar",year,sep="_")
  ssar   <- paste(combine[1],"subsurface_sar",year,sep="_")
  weight <- paste(combine[1],"total_weight",year,sep="_")
  value  <- paste(combine[1],"total_value",year,sep="_")
  mwhour <- paste(combine[1],"mw_fishinghours",year,sep="_")
  
  for (i in 1:length(year)){
    nam <- paste(combine,groups[1],year[i], sep="_")
    FisheriesMet[,sar[i]] <- tt <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[2],year[i], sep="_")
    FisheriesMet[,ssar[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[3],year[i], sep="_")
    FisheriesMet[,weight[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[4],year[i], sep="_")
    FisheriesMet[,value[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[5],year[i], sep="_")
    FisheriesMet[,mwhour[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  }
  
  exclude <- c(paste(combine[2],"surface_sar",year,sep="_"),paste(combine[2],"subsurface_sar",year,sep="_"),
               paste(combine[2],"total_weight",year,sep="_"),  paste(combine[2],"total_value",year,sep="_"),
               paste(combine[2],"mw_fishinghours",year,sep="_"),
               paste(combine[3],"surface_sar",year,sep="_"),paste(combine[3],"subsurface_sar",year,sep="_"),
               paste(combine[3],"total_weight",year,sep="_"),  paste(combine[3],"total_value",year,sep="_"),
               paste(combine[3],"mw_fishinghours",year,sep="_"))
  indexcol <- which(names(FisheriesMet) %in% exclude)
  FisheriesMet <- FisheriesMet[,-indexcol]
  
  ### combine OT_CRU
  combine <- c("OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF")
  
  sar    <- paste(combine[1],"surface_sar",year,sep="_")
  ssar   <- paste(combine[1],"subsurface_sar",year,sep="_")
  weight <- paste(combine[1],"total_weight",year,sep="_")
  value  <- paste(combine[1],"total_value",year,sep="_")
  mwhour <- paste(combine[1],"mw_fishinghours",year,sep="_")
  
  for (i in 1:length(year)){
    nam <- paste(combine,groups[1],year[i], sep="_")
    FisheriesMet[,sar[i]] <- tt <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[2],year[i], sep="_")
    FisheriesMet[,ssar[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[3],year[i], sep="_")
    FisheriesMet[,weight[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[4],year[i], sep="_")
    FisheriesMet[,value[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
    nam <- paste(combine,groups[5],year[i], sep="_")
    FisheriesMet[,mwhour[i]] <- rowSums(FisheriesMet[,nam],na.rm=T)
  }
  
  exclude <- c(paste(combine[2],"surface_sar",year,sep="_"),paste(combine[2],"subsurface_sar",year,sep="_"),
               paste(combine[2],"total_weight",year,sep="_"),  paste(combine[2],"total_value",year,sep="_"),
               paste(combine[2],"mw_fishinghours",year,sep="_"),
               paste(combine[3],"surface_sar",year,sep="_"),paste(combine[3],"subsurface_sar",year,sep="_"),
               paste(combine[3],"total_weight",year,sep="_"),  paste(combine[3],"total_value",year,sep="_"),
               paste(combine[3],"mw_fishinghours",year,sep="_"))
  indexcol <- which(names(FisheriesMet) %in% exclude)
  FisheriesMet <- FisheriesMet[,-indexcol]
  
  FisheriesMet[,2:ncol(FisheriesMet)][FisheriesMet[,2:ncol(FisheriesMet)] == 0] <- NA
  
  setwd(pathdir_nogit)
  save(FisheriesMet,file=paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"))
  