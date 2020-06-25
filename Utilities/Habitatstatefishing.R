
##################################################################
# calculate state for each year

# loop each year and calculate state 
  state_year <- c()
  ccname <- c()

  Fisheries<-cbind(Fisheries, Region@data[match(Fisheries$csquares,Region@data$csquares), c("intercept","slope")])
  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])
  
  for (i in 1: length(Period)){
    loopdata <- Fisheries
    Depl_TBB  <- 0.14 * loopdata[,paste("TBB_surface_sar",Period[i],sep="_")]   ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OT   <- 0.06 * loopdata[,paste("OT_surface_sar",Period[i],sep="_")]    ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_TD   <- 0.20 * loopdata[,paste("TD_surface_sar",Period[i],sep="_")]   ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_sein <- 0.06 * loopdata[,paste("Seine_surface_sar",Period[i],sep="_")] ### unknown (now similar to otter trawling) 
    Depl <- cbind(Depl_TBB,Depl_OT,Depl_TD,Depl_sein)
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
  }
  
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)

# now get for different metiers as defined by FBIT
  state_TBB <- data.frame(Fisheries$csquares,matrix(data=1,nrow=nrow(Fisheries),ncol =length(Period)))
  state_OTCRU <- data.frame(FisheriesMet$csquares,matrix(data=1,nrow=nrow(FisheriesMet),ncol =length(Period)))
  state_OTREST <- data.frame(FisheriesMet$csquares,matrix(data=1,nrow=nrow(FisheriesMet),ncol =length(Period)))
  
  for (i in 1: length(Period)){
    
    # TBBALL
    Depl_TBB <- 0.14 * Fisheries[,paste("TBB_surface_sar",Period[i], sep = "_")]   ### data from Hiddink et al. PNAS 2017 Table S4
    for(j in 1:nrow(state_TBB)){
      if (!(is.na(Depl_TBB[j] ))) {
        state_TBB[j,(i+1)] <- RBS(Fd=Depl_TBB[j],a=Fisheries$slope[j],b=Fisheries$intercept[j])
    } }
  
   # OTCRU
    Depl_OTCRU <- 0.06 * FisheriesMet[,paste("OTCRU_surface_sar",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    for(j in 1:nrow(state_OTCRU)){
      if (!(is.na(Depl_OTCRU[j] ))) {
        state_OTCRU[j,(i+1)] <- RBS(Fd=Depl_OTCRU[j],a=FisheriesMet$slope[j],b=FisheriesMet$intercept[j])
      }}  
    
    # OTREST
    Depl_OTDMF <- 0.06 * FisheriesMet[,paste("OTDMF_surface_sar",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTMIX <- 0.06 * FisheriesMet[,paste("OTMIX_surface_sar",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTMIXDMFBEN <- 0.06 * FisheriesMet[,paste("OTMIXDMFBEN_surface_sar",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTMIXCRUDMF <- 0.06 * FisheriesMet[,paste("OTMIXCRUDMF_surface_sar",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTSPF <- 0.06 * FisheriesMet[,paste("OTSPF_surface_sar",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTcomb <- cbind(Depl_OTDMF,Depl_OTMIX,Depl_OTMIXDMFBEN,Depl_OTMIXCRUDMF,Depl_OTSPF)
    Depl_OTREST<-rowSums(Depl_OTcomb,na.rm=T)
    
    for(j in 1:nrow(state_OTREST)){
      if (Depl_OTREST[j]>0) {
        state_OTREST[j,(i+1)] <- RBS(Fd=Depl_OTREST[j],a=FisheriesMet$slope[j],b=FisheriesMet$intercept[j])
      }}
  }
  
  colnames(state_OTCRU) [2:(length(Period)+1)]<-  paste(rep("state_OTCRU",length(Period)),Period,sep="_")
  colnames(state_TBB) [2:(length(Period)+1)]<-  paste(rep("state_TBB",length(Period)),Period,sep="_")
  colnames(state_OTREST) [2:(length(Period)+1)]<-  paste(rep("state_OTREST",length(Period)),Period,sep="_")
  
  State_reg <- cbind(State_reg, state_OTCRU[match(State_reg[,1],state_OTCRU[,1]), c(2:(length(Period)+1))])
  State_reg <- cbind(State_reg, state_TBB[match(State_reg[,1],state_TBB[,1]), c(2:(length(Period)+1))])
  State_reg <- cbind(State_reg, state_OTREST[match(State_reg[,1],state_OTREST[,1]), c(2:(length(Period)+1))])
  