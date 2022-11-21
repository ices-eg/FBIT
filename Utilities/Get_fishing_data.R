
# use icesVMS to get 
# total surface and subsurface abrasion, weight and value of landings per year
  
  EcoReg <- c("Baltic Sea","Greater North Sea","Celtic Seas","Bay of Biscay and the Iberian Coast")
  Period <- 2009:datacall-1
  
  Total <- c()
  
  for (j in 1:length(EcoReg)) {
  for (i in 1: length(Period)){
    Total_year   <- icesVMS::get_wgfbit_data1(EcoReg[j], Period[i])
    Total <- rbind(Total, Total_year)
  }}

  Fisheries <- Total
  
  ### remove columns - save as small file
  setwd(pathdir_nogit)
  Fisheries <- Fisheries[,c(1:4,6,7,10)]
  save(Fisheries,file=paste("fisheries_EU_atlantic_FBIT_VMSdatacall",datacall,".RData",sep=""))
    
# get fishing data specified per metier

  metier <- c("DRB_MOL","OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF","OT_DMF","OT_MIX","OT_MIX_DMF_BEN",
              "OT_MIX_DMF_PEL","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")

  Total <- c()
  
  for (j in 1:length(EcoReg)) {
    for (i in 1: length(Period)){
      for (p in 1:length(metier)){
        Gear   <- icesVMS::get_wgfbit_data1(EcoReg[j], Period[i], benthis_metier = metier[p])
        if(length(Gear)>0){
          Total <- rbind(Total, Gear)
        }
      }}}  
      
  FisheriesMet <- Total
  
  ### remove columns - save as small file
  setwd(pathdir_nogit)
  FisheriesMet <- FisheriesMet[,c(1:4,6,7,10)]
  save(FisheriesMet,file=paste("fisheries_EU_atlantic_metier_FBIT_VMSdatacall",datacall,".RData", sep=""))
  