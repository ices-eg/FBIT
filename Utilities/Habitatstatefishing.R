  
##################################################################
# calculate state for each year
  
# loop each year and calculate state 
  state_year <- c()
  ccname <- c()
  
  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
# depletion rates is based on  Hiddink et al. PNAS 2017 / Rijnsdorp et al. ICES 2020
  for (i in 1: length(Period)){
    loopdata <- FisheriesMet
    Depl_DRB_MOL          <- 0.200 * loopdata[,paste("DRB_MOL_surface_sar",Period[i],sep="_")]
    Depl_OT_CRU           <- 0.100 * loopdata[,paste("OT_CRU_surface_sar",Period[i],sep="_")]
    Depl_OT_DMF           <- 0.026 * loopdata[,paste("OT_DMF_surface_sar",Period[i],sep="_")]
    Depl_OT_MIX           <- 0.074 * loopdata[,paste("OT_MIX_surface_sar",Period[i],sep="_")]
    Depl_OT_SPF           <- 0.009 * loopdata[,paste("OT_SPF_surface_sar",Period[i],sep="_")]
    Depl_SDN_DMF          <- 0.009 * loopdata[,paste("SDN_DMF_surface_sar",Period[i],sep="_")]
    Depl_SSC_DMF          <- 0.016 * loopdata[,paste("SSC_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_CRU          <- 0.060 * loopdata[,paste("TBB_CRU_surface_sar",Period[i],sep="_")]
    Depl_TBB_DMF          <- 0.140 * loopdata[,paste("TBB_DMF_surface_sar",Period[i],sep="_")]
    Depl_TBB_MOL          <- 0.060 * loopdata[,paste("TBB_MOL_surface_sar",Period[i],sep="_")]
    
    H  <- 5.31 # recovery  = H/ longevity
    
  ### calculate state for all gears
    Depl <- cbind(Depl_DRB_MOL,Depl_OT_CRU,Depl_OT_DMF,Depl_OT_MIX,
                  Depl_OT_SPF,Depl_SDN_DMF,Depl_SSC_DMF,Depl_TBB_CRU,Depl_TBB_DMF,Depl_TBB_MOL)
    Depl_tot<-rowSums(Depl,na.rm=T)
    loopdata$Depl_tot <- Depl_tot
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_tot[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state",Period[i],sep="_"))
    
  ### calculate state for DRB_MOL
    loopdata$Depl_DRB_MOL <- Depl[,"Depl_DRB_MOL"]
    loopdata$Depl_DRB_MOL[is.na(loopdata$Depl_DRB_MOL)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_DRB_MOL[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_DRB_MOL[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_DRB_MOL",Period[i],sep="_"))
    
  ### calculate state for OT_CRU (combined)
    loopdata$Depl_OT_CRU <- Depl[,"Depl_OT_CRU"]
    loopdata$Depl_OT_CRU[is.na(loopdata$Depl_OT_CRU)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_OT_CRU[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_OT_CRU[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_CRU",Period[i],sep="_"))
    
  ### calculate state for OT_DMF
    loopdata$Depl_OT_DMF <- Depl[,"Depl_OT_DMF"]
    loopdata$Depl_OT_DMF[is.na(loopdata$Depl_OT_DMF)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_OT_DMF[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_OT_DMF[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_DMF",Period[i],sep="_"))
    
  ### calculate state for OT_MIX (combined)
    loopdata$Depl_OT_MIX <- Depl[,"Depl_OT_MIX"]
    loopdata$Depl_OT_MIX[is.na(loopdata$Depl_OT_MIX)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_OT_MIX[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_OT_MIX[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_MIX",Period[i],sep="_"))
    
  ### calculate state for OT_SPF
    loopdata$Depl_OT_SPF <- Depl[,"Depl_OT_SPF"]
    loopdata$Depl_OT_SPF[is.na(loopdata$Depl_OT_SPF)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_OT_SPF[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_OT_SPF[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_OT_SPF",Period[i],sep="_"))    
    
  ### calculate state for SDN_DMF
    loopdata$Depl_SDN_DMF <- Depl[,"Depl_SDN_DMF"]
    loopdata$Depl_SDN_DMF[is.na(loopdata$Depl_SDN_DMF)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_SDN_DMF[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_SDN_DMF[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_SDN_DMF",Period[i],sep="_"))   
    
  ### calculate state for SSC_DMF
    loopdata$Depl_SSC_DMF <- Depl[,"Depl_SSC_DMF"]
    loopdata$Depl_SSC_DMF[is.na(loopdata$Depl_SSC_DMF)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_SSC_DMF[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_SSC_DMF[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_SSC_DMF",Period[i],sep="_"))   
    
  ### calculate state for TBB_CRU
    loopdata$Depl_TBB_CRU <- Depl[,"Depl_TBB_CRU"]
    loopdata$Depl_TBB_CRU[is.na(loopdata$Depl_TBB_CRU)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_TBB_CRU[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_TBB_CRU[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB_CRU",Period[i],sep="_"))     
    
  ### calculate state for TBB_DMF
    loopdata$Depl_TBB_DMF <- Depl[,"Depl_TBB_DMF"]
    loopdata$Depl_TBB_DMF[is.na(loopdata$Depl_TBB_DMF)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_TBB_DMF[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_TBB_DMF[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB_DMF",Period[i],sep="_"))     
    
  ### calculate state for TBB_MOL
    loopdata$Depl_TBB_MOL <- Depl[,"Depl_TBB_MOL"]
    loopdata$Depl_TBB_MOL[is.na(loopdata$Depl_TBB_MOL)] <- 0
    
    # calculate slope and intercept and use RBS function
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
      if (loopdata$Depl_TBB_MOL[j] > 0 ) {
        state[j] <- RBS(Fd=loopdata$Depl_TBB_MOL[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
      }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state_TBB_MOL",Period[i],sep="_")) 
    
  }
  
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)
