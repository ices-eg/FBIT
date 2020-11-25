
##################################################################
# calculate state for each year

# loop each year and calculate state 
  state_year <- c()
  ccname <- c()

  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
  state_year <- c()
  ccname <- c()
  
  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
  # depletion rates is based on  Hiddink et al. PNAS 2017 / Rijnsdorp et al. ICES 2020
  for (i in 1: length(Period)){
    loopdata <- FisheriesMet
    Depl_DRB_MOL          <- 0.200 * loopdata[,paste("DRB_MOL_surface_sar",Period[i],sep="_")]
    Depl_OT_CRU           <- 0.100 * loopdata[,paste("OT_CRU_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX_CRU       <- 0.100 * loopdata[,paste("OT_MIX_CRU_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX_CRU_DMF   <- 0.100 * loopdata[,paste("OT_MIX_CRU_DMF_surface_sar",Period[i],sep="_")]
    Depl_OT_DMF           <- 0.026 * loopdata[,paste("OT_DMF_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX           <- 0.074 * loopdata[,paste("OT_MIX_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX_DMF_BEN   <- 0.074 * loopdata[,paste("OT_MIX_DMF_BEN_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX_DMF_PEL   <- 0.074 * loopdata[,paste("OT_MIX_DMF_PEL_surface_sar",Period[i],sep="_")]
    Depl_OT_SPF           <- 0.009 * loopdata[,paste("OT_SPF_surface_sar",Period[i],sep="_")]
    Depl_SDN_DMF          <- 0.009 * loopdata[,paste("SDN_DMF_surface_sar",Period[i],sep="_")]
    Depl_SSC_DMF          <- 0.016 * loopdata[,paste("SSC_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_CRU          <- 0.060 * loopdata[,paste("TBB_CRU_surface_sar",Period[i],sep="_")]
    Depl_TBB_DMF          <- 0.140 * loopdata[,paste("TBB_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_MOL          <- 0.060 * loopdata[,paste("TBB_MOL_surface_sar",Period[i],sep="_")]
    
    ### calculate state for all gears
    Depl <- cbind(Depl_DRB_MOL,Depl_OT_CRU,Depl_OT_MIX_CRU,Depl_OT_MIX_CRU_DMF,
                  Depl_OT_DMF,Depl_OT_MIX,Depl_OT_MIX_DMF_BEN,Depl_OT_MIX_DMF_PEL,
                  Depl_OT_SPF,Depl_SDN_DMF,Depl_SSC_DMF,Depl_TBB_CRU,Depl_TBB_DMF,Depl_TBB_MOL)
    Depl_tot<-rowSums(Depl,na.rm=T)
    loopdata$Depl_tot <- Depl_tot
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_tot[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state",Period[i],sep="_"))
    
    ### calculate state for OT_CRU (combined)
    Depl_OT_CRU_comb <- rowSums(Depl[,c("Depl_OT_CRU","Depl_OT_MIX_CRU","Depl_OT_MIX_CRU_DMF")],na.rm=T)
    loopdata$Depl_OT_CRU <- Depl_OT_CRU_comb
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_OT_CRU[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_OT_CRU[j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OTCRU",Period[i],sep="_"))
    
    # calculate state for OT_others (combined)
    Depl_OT_MIX_comb <- rowSums(Depl[,c("Depl_OT_MIX","Depl_OT_MIX_DMF_BEN","Depl_OT_MIX_DMF_PEL",
                                        "Depl_OT_DMF","Depl_OT_SPF")],na.rm=T)
    loopdata$Depl_OT_MIX <- Depl_OT_MIX_comb
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_OT_MIX[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_OT_MIX[j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OTREST",Period[i],sep="_"))
    
    # calculate state for TBB (combined)
    Depl_TBB_comb <- rowSums(Depl[,c("Depl_TBB_CRU","Depl_TBB_DMF","Depl_TBB_MOL")],na.rm=T)
    loopdata$Depl_TBB_MIX <- Depl_TBB_comb
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_TBB_MIX[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_TBB_MIX[j],a=loopdata$slope[j],b=loopdata$intercept[j])
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB",Period[i],sep="_"))
    
  }
  
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)
    
  