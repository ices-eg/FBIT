
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