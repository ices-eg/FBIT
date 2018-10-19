
##################################################################
# Load fishing data and calculate state and impact for each year for 3 metier groups
  # otter trawl crustaceans -> "OT_CRU"
  # Beam trawls -> "Beam"
  # otter trawl others -> "OT_DMF","OT_MIX","OT_MIX_DMF_BEN","OT_MIX_CRU_DMF","OT_SPF"

# Calculate impact from continuous longevity composition Rijnsdorp et al (Ecol App 2018); Hiddink et al. (PNAS 2017); Hiddink et al. (J Applied Ecol 2018)
  betas <- c()
  betas[1] <- -5.682973    ### intercept
  betas[2] <-  3.502197    ### ln(longevity)
  betas[3] <- -0.082575    ## ln(trawling)
  betas[4] <-  0.021062    ### mud
  betas[5] <-  0.018634    ### gravel
  betas[6] <-  0.042260    ## ln(shear stress)
  betas[7] <- -0.118155    ## ln(trawling):ln(shear stress)
  betas[8] <- -0.019554    ## ln(longevity):Gravel

# load available fishing data: The shapefile datasets for the OSPAR region are available at: https://doi.org/10.17895/ices.pub.2861 ####
  workingdir<-paste(pathdir,"1 - Input env and fishing data/ICES.2017.OSPAR.Technical-Service-VMS-fishing-pressure",sep="/")
  
  # loop each year and get information per metier
  for (i in 1: length(Period)){
    OTCRU   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_OT_CRU_",Period[i],sep=""))
    colnames(OTCRU@data) <- paste("OTCRU",colnames(OTCRU@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, OTCRU@data[match(Region@data$squares,OTCRU@data[,paste("OTCRU_c_square",Period[i], sep = "_")]), c(5:8)])
    
    TBB   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Beam_",Period[i],sep=""))
    colnames(TBB@data) <- paste("TBB",colnames(TBB@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, TBB@data[match(Region@data$squares,TBB@data[,paste("TBB_c_square",Period[i], sep = "_")]), c(5:8)])
    
    OTDMF   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_OT_DMF_",Period[i],sep=""))
    colnames(OTDMF@data) <- paste("OTDMF",colnames(OTDMF@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, OTDMF@data[match(Region@data$squares,OTDMF@data[,paste("OTDMF_c_square",Period[i], sep = "_")]), c(5:8)])
    
    OTMIX   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_OT_MIX_",Period[i],sep=""))
    colnames(OTMIX@data) <- paste("OTMIX",colnames(OTMIX@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, OTMIX@data[match(Region@data$squares,OTMIX@data[,paste("OTMIX_c_square",Period[i], sep = "_")]), c(5:8)])
    
    OTMIXDMFBEN   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_OT_MIX_DMF_BEN_",Period[i],sep=""))
    colnames(OTMIXDMFBEN@data) <- paste("OTMIXDMFBEN",colnames(OTMIXDMFBEN@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, OTMIXDMFBEN@data[match(Region@data$squares,OTMIXDMFBEN@data[,paste("OTMIXDMFBEN_c_square",Period[i], sep = "_")]), c(5:8)])
    
    OTMIXCRUDMF   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_OT_MIX_CRU_DMF_",Period[i],sep=""))
    colnames(OTMIXCRUDMF@data) <- paste("OTMIXCRUDMF",colnames(OTMIXCRUDMF@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, OTMIXCRUDMF@data[match(Region@data$squares,OTMIXCRUDMF@data[,paste("OTMIXCRUDMF_c_square",Period[i], sep = "_")]), c(5:8)])
    
    OTSPF   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_OT_SPF_",Period[i],sep=""))
    colnames(OTSPF@data) <- paste("OTSPF",colnames(OTSPF@data), Period[i], sep = "_")
    Region@data<-cbind(Region@data, OTSPF@data[match(Region@data$squares,OTSPF@data[,paste("OTSPF_c_square",Period[i], sep = "_")]), c(5:8)])
    }
  
  # calculate slope and intercept and use RBS function
  slope <- betas[2]+betas[8]*Region@data$Gravel   # slope of binomial model
  intercept = betas[1]+ betas[3]*log(0+0.01) + betas[4]*Region@data$Mud + betas[5]*Region@data$Gravel + 
    betas[6]*log(Region@data$Shearstress+0.01) +  betas[7]*log(0+0.01)*log(Region@data$Shearstress+0.01)      # intercept of binomial model without fishing
  
  state_OTCRU <- data.frame(Region@data$squares,matrix(data=NA,nrow=nrow(Region@data),ncol =length(Period)))
  state_TBB <- data.frame(Region@data$squares,matrix(data=NA,nrow=nrow(Region@data),ncol =length(Period)))
  state_OTREST <- data.frame(Region@data$squares,matrix(data=NA,nrow=nrow(Region@data),ncol =length(Period)))
  
  # calculate depletion mortality and state
  for (i in 1: length(Period)){

    # TBBALL
    Depl_OTCRU <- 0.06 * Region@data[,paste("OTCRU_SurfaceSAR",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    for(j in 1:nrow(state_OTCRU)){
      if (!(is.na(Depl_OTCRU[j] ))) {
      state_OTCRU[j,(i+1)] <- RBS(Fd=Depl_OTCRU[j],a=slope[j],b=intercept[j])
    }}
    
    # TBBALL
    Depl_TBB <- 0.14 * Region@data[,paste("TBB_SurfaceSAR",Period[i], sep = "_")]   ### data from Hiddink et al. PNAS 2017 Table S4
    for(j in 1:nrow(state_TBB)){
      if (!(is.na(Depl_TBB[j] ))) {
        state_TBB[j,(i+1)] <- RBS(Fd=Depl_TBB[j],a=slope[j],b=intercept[j])
      }}
    
    # OTREST
    Depl_OTDMF <- 0.06 * Region@data[,paste("OTDMF_SurfaceSAR",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTMIX <- 0.06 * Region@data[,paste("OTMIX_SurfaceSAR",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTMIXDMFBEN <- 0.06 * Region@data[,paste("OTMIXDMFBEN_SurfaceSAR",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTMIXCRUDMF <- 0.06 * Region@data[,paste("OTMIXCRUDMF_SurfaceSAR",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTSPF <- 0.06 * Region@data[,paste("OTSPF_SurfaceSAR",Period[i], sep = "_")] ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OTcomb <- cbind(Depl_OTDMF,Depl_OTMIX,Depl_OTMIXDMFBEN,Depl_OTMIXCRUDMF,Depl_OTSPF)
    Depl_OTREST<-rowSums(Depl_OTcomb,na.rm=T)

    for(j in 1:nrow(state_OTREST)){
      if (Depl_OTREST[j]>0) {
        state_OTREST[j,(i+1)] <- RBS(Fd=Depl_OTREST[j],a=slope[j],b=intercept[j])
      }}
  }
  
  colnames(state_OTCRU) [2:(length(Period)+1)]<-  paste(rep("State_OTCRU",length(Period)),Period,sep="_")
  colnames(state_TBB) [2:(length(Period)+1)]<-  paste(rep("State_TBB",length(Period)),Period,sep="_")
  colnames(state_OTREST) [2:(length(Period)+1)]<-  paste(rep("State_OTREST",length(Period)),Period,sep="_")
  
  Region@data <- cbind(Region@data, state_OTCRU[match(Region@data$squares,state_OTCRU$Region.data.squares), c(2:(length(Period)+1))])
  Region@data <- cbind(Region@data, state_TBB[match(Region@data$squares,state_TBB$Region.data.squares), c(2:(length(Period)+1))])
  Region@data <- cbind(Region@data, state_OTREST[match(Region@data$squares,state_OTREST$Region.data.squares), c(2:(length(Period)+1))])
  
  
  
  
  
  
  
  

    