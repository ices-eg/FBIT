
##################################################################
# Load fishing data and calculate state and impact for each year

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
  
# loop each year and calculate state 
  state_year <- c()
  ccname <- c()
  
  for (i in 1: length(Period)){
    TBB   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Beam_",Period[i],sep=""))
    OT    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Otter_",Period[i],sep=""))
    TD    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Dredge_",Period[i],sep=""))
    Seine <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Seine_",Period[i],sep=""))
    
    loopdata<-Region@data
  
    loopdata<-cbind(loopdata, TBB@data[match(loopdata$squares,TBB@data$c_square), c(5)])
    colnames(loopdata)[16] <- "TBB_SurfSAR"
    loopdata<-cbind(loopdata, OT@data[match(loopdata$squares,OT@data$c_square), c(5)])
    colnames(loopdata)[17] <- "OT_SurfSAR"
    loopdata<-cbind(loopdata, TD@data[match(loopdata$squares,TD@data$c_square), c(5)])
    colnames(loopdata)[18] <- "TD_SurfSAR"
    loopdata<-cbind(loopdata, Seine@data[match(loopdata$squares,Seine@data$c_square), c(5)])
    colnames(loopdata)[19] <- "Seine_SurfSAR"
    
    Depl_TBB  <- 0.14 * loopdata$TBB_SurfSAR   ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_OT   <- 0.06 * loopdata$OT_SurfSAR    ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_TD   <- 0.20 * loopdata$TD_SurfSAR    ### data from Hiddink et al. PNAS 2017 Table S4
    Depl_sein <- 0.06 * loopdata$Seine_SurfSAR ### unknown (now similar to otter trawling) 
    Depl <- cbind(Depl_TBB,Depl_OT,Depl_TD,Depl_sein)
    Depl_tot<-rowSums(Depl,na.rm=T)
    loopdata$Depl_tot <- Depl_tot
   
    # calculate slope and intercept and use RBS function
    
    loopdata$slope <- betas[2]+betas[8]*loopdata$Gravel   # slope of binomial model
    loopdata$intercept = betas[1]+ betas[3]*log(0+0.01) + betas[4]*loopdata$Mud + betas[5]*loopdata$Gravel + 
      betas[6]*log(loopdata$Shearstress+0.01) +  betas[7]*log(0+0.01)*log(loopdata$Shearstress+0.01)      # intercept of binomial model without fishing
    
    state <- rep(1,nrow(loopdata))
    
    for(j in 1:nrow(loopdata)){
       if (loopdata$Depl_tot[j] > 0 ) {
      state[j] <- RBS(Fd=loopdata$Depl_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j])
    }}
    
    state_year <- cbind(state_year,state)
    ccname <- c(ccname,paste("state",Period[i],sep="_"))
  }
  
  colnames(state_year)<-ccname
  Region@data<-cbind(Region@data,state_year)
  
  ### now include total surface and subsurface abrasion, weight and value of landings per year
  for (i in 1: length(Period)){
    Total   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_total_",Period[i],sep=""))
    colnames(Total@data) <- paste(colnames(Total@data), Period[i], sep = "_")
    Region@data <- cbind(Region@data, Total@data[match(Region@data$squares,Total@data$c_square), c(5:8)])
   }
  