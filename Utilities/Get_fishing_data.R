
# use icesVMS to get 
# total surface and subsurface abrasion, weight and value of landings per year
  
  #EcoReg <- c("Baltic Sea","Greater North Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
  Period <- 2009:(datacall-1)
  
  Total <- c()
  
  for (i in 1: length(Period)){
    Total_year   <- get_wgfbit_data3(year = Period[i], datacall=datacall)
    Total_comb   <- aggregate(list(Total_year$surface_sar,Total_year$subsurface_sar,
                                   Total_year$total_weight,Total_year$total_value,
                                   Total_year$mw_fishinghours),by=list(Total_year$c_square,Total_year$year),FUN=sum)
    colnames(Total_comb) <- c("c_square","year","surface_sar","subsurface_sar","total_weight","total_value","mw_fishinghours")
    Total <- rbind(Total, Total_comb)
  }

  Fisheries <- Total
  
  ### remove columns - save as small file
  setwd(pathdir_nogit)
  #Fisheries <- Fisheries[,c(1:4,6,7,10)]
  save(Fisheries,file=paste("fisheries_FBIT_VMSdatacall",datacall,".RData",sep=""))
    
# get fishing data specified per metier

  metier <- c("DRB_MOL","OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF","OT_DMF","OT_MIX","OT_MIX_DMF_BEN",
              "OT_MIX_DMF_PEL","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")

  Total <- c()
  
  for (i in 1: length(Period)){
    for (p in 1:length(metier)){
        Gear   <- get_wgfbit_data3( Period[i], benthis_metier = metier[p],datacall = datacall)
      if(length(Gear)>0){
        Total_gear   <- aggregate(list(Gear$surface_sar,Gear$subsurface_sar,
                                       Gear$total_weight,Gear$total_value,
                                       Gear$mw_fishinghours),by=list(Gear$c_square,Gear$year,Gear$gear_category),FUN=sum)
        colnames(Total_gear) <- c("c_square","year","gear_category","surface_sar","subsurface_sar","total_weight","total_value","mw_fishinghours")
        
        Total <- rbind(Total, Total_gear)
        }
      }}  
      
  FisheriesMet <- Total
  
  ### remove columns - save as small file
  setwd(pathdir_nogit)
  #FisheriesMet <- FisheriesMet[,c(1:4,6,7,10)]
  save(FisheriesMet,file=paste("fisheries_metier_FBIT_VMSdatacall",datacall,".RData", sep=""))
  