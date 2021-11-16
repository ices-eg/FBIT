
###### DATA PROCESSING

###### figures and tables for ICES WGFBIT
  SSAR_year <- paste("surface_sar",AssYear,sep="_")
  state_year <- paste("state",AssYear,sep="_")
  weight_year <- paste("total_weight",AssYear,sep="_")
  value_year <- paste("total_value",AssYear,sep="_")
  
  setwd(pathdir_nogit)  
  dir.create("Producing figures and tables")
  setwd(paste(pathdir_nogit,"Producing figures and tables",sep="/"))  
  dir.create(paste(Assregion))
  setwd(paste(pathdir_nogit,"Producing figures and tables",Assregion,sep="/"))  
  dir.create(paste(AssYear))
  setwd(paste(pathdir_nogit,"Producing figures and tables",Assregion,AssYear,sep="/"))
  
  #Region <- Region[!(is.na(Region$medlong)),]
  idx <- which(names(Region@data)== "long")
  colnames(Region@data)[idx]  <- "longitude"
  idx <- which(names(Region@data)== "lat")
  colnames(Region@data)[idx]  <- "latitude"
  
#####
# Figure A.1
################
  figA1 <- Region@data
  nam <- c(SSAR_year,weight_year,value_year)
  figA1 <- cbind(figA1, Fisheries[match(figA1$csquares,Fisheries$csquares), c(nam)])
  
  nam <- c(state_year)
  figA1 <- cbind(figA1, State_reg[match(figA1$csquares,State_reg$Fisheries.csquares), c(nam)])
  colnames(figA1)[ncol(figA1)] <- state_year
  
  save(figA1, file="FigureA1.RData")
  
#####
# Table A.1
################
  TA1dat_all <- Region@data
  
  # estimate three depth boundaries
  mindepth <- c(0, -200, -800)
  maxdepth <- c(-200, -800, -8000)
  
  A1table <- (matrix(data=NA, ncol=3,nrow=7))
  
  for(iDepth in 1:3){
    TA1dat <-  subset(TA1dat_all,TA1dat_all$Depth < mindepth[iDepth] & TA1dat_all$Depth >= maxdepth[iDepth])
    if(nrow(TA1dat) >0){
      nam <- c(SSAR_year)
      TA1dat <- cbind(TA1dat, Fisheries[match(TA1dat$csquares,Fisheries$csquares), c(nam)])
      colnames(TA1dat)[ncol(TA1dat)] <- SSAR_year
      TA1dat[,c(nam)][is.na(TA1dat[,c(nam)])] <- 0
      
      # indicator 1 intensity
      TA1dat$sweptarea <- TA1dat[,nam]*TA1dat$area_sqkm
      ind1 <- sum(TA1dat$sweptarea,na.rm=T)/sum(TA1dat$area_sqkm)
      
      # indicator 2 proportion of area within fished grid cells (fished irrespective of swept area)
      ind2 <-  ifelse(TA1dat[,nam] > 0,1,0)
      ind2 <- sum(ind2 * TA1dat$area_sqkm)/sum(TA1dat$area_sqkm)
      
      # indicator 3 proportion of area swept each year
      TA1dat$sweptarea2 <- TA1dat$sweptarea
      TA1dat$sweptarea2 <- ifelse(TA1dat$sweptarea > TA1dat$area_sqkm,TA1dat$area_sqkm,TA1dat$sweptarea)
      ind3 <- sum(TA1dat$sweptarea2,na.rm=T)/sum(TA1dat$area_sqkm)
      
      # indicator 4 aggregation of fishing pressure
      if(sum(TA1dat$sweptarea >0)){
        TA1dat <- TA1dat[order(TA1dat[,"sweptarea"],decreasing = T),]
        TA1dat$cumSSAR <- cumsum(TA1dat[,"sweptarea"])
        TA1dat$cumSSAR <- TA1dat$cumSSAR / sum(TA1dat[,"sweptarea"])
        ind4 <- min(which (TA1dat$cumSSAR > .9))/nrow(TA1dat)
      } else {ind4 <- NA}
      
      # indicator 5 proportion of area within persistently unfished grid cells
      SSARNames <- paste("surface_sar",AssPeriod,sep="_")
      TA1dat <- cbind(TA1dat, Fisheries[match(TA1dat$csquares,Fisheries$csquares), c(SSARNames)])
      TA1dat[,c(SSARNames)][is.na(TA1dat[,c(SSARNames)])] <- 0
      
      TA1dat_sub <- subset(TA1dat,TA1dat[,SSARNames[1]] == 0 & TA1dat[,SSARNames[2]] == 0 &
                             TA1dat[,SSARNames[3]] == 0 & TA1dat[,SSARNames[4]] == 0 &
                             TA1dat[,SSARNames[5]] == 0 & TA1dat[,SSARNames[6]] == 0)
      ind5 <- nrow(TA1dat_sub)/nrow(TA1dat) # unfished area is a grid cell with a swept area == 0
      
      # indicator 6 average impact - PD model
      nam <- c(state_year)
      TA1dat_PD <- cbind(TA1dat, State_reg[match(TA1dat$csquares,State_reg$Fisheries.csquares), c(nam)])
      colnames(TA1dat_PD)[ncol(TA1dat_PD)] <- state_year
      TA1dat_PD[,c(nam)][is.na(TA1dat_PD[,c(nam)])] <- 1
      TA1dat_PD$avgstate_weight <- TA1dat_PD[,nam]*TA1dat_PD$area_sqkm
      ind6_PD <- 1- sum(TA1dat_PD$avgstate_weight,na.rm=T)/sum(TA1dat_PD$area_sqkm)
        
      # indicator 7 proportion of area with impact < 0.2 - PD model
      ind7_PD <-  ifelse(TA1dat_PD[,nam] >= 0.8,1,0)
      ind7_PD <- sum(ind7_PD * TA1dat_PD$area_sqkm)/sum(TA1dat_PD$area_sqkm)
       
      A1table[,iDepth] <- c(ind1,ind2,ind3,ind4,ind5,ind6_PD,ind7_PD)
    }}
  
  save(A1table, file="TableA1.RData")

#####
# Figure A.2
################
  # get the dominant habitat per c-square
  domMSFD <- msfd_csq %>% group_by(csquares) %>% slice(which.max(area_km2))
  domMSFD <- as.data.frame(domMSFD[,c(1:2)])  
 
  Habitat <- cbind(Region@data, domMSFD[match(Region@data$csquares,domMSFD$csquares), c(2)])
  colnames(Habitat)[ncol(Habitat)] <- "MSFD"
  Habitat <- subset(Habitat, Habitat$Depth < 0 )
  Habitat$MSFD <- as.character(Habitat$MSFD)
  Habitat$MSFD[Habitat$MSFD=="Na"]=NA
  save(Habitat, file="FigureA2.RData")

#####
# Figure A.3
################
  figA3 <- Region@data
  nam <- paste("subsurface_sar",AssPeriod,sep="_")
  figA3 <- cbind(figA3, Fisheries[match(figA3$csquares,Fisheries$csquares), c(nam)])
  nam <- paste("surface_sar",AssPeriod,sep="_")
  figA3 <- cbind(figA3, Fisheries[match(figA3$csquares,Fisheries$csquares), c(nam)])
  save(figA3, file="FigureA3.RData")
  
#####
# Table A.2
################
  TA2dat <-  Region@data
  
  # account for area of MSFD habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  TA2dat <- merge(TA2dat,msfd_csq_new,by = "csquares", all.x =T)
  TA2dat$MSFD <- as.character(TA2dat$MSFD)
  TA2dat$MSFD[TA2dat$MSFD=="Na"]= "Unknown"
  TA2dat$grid <- 1
  #TA2dat$MSFD[TA2dat$MSFD=="<Na>"]= "Unknown"
  #TA2dat$MSFD[is.na(TA2dat$MSFD)]= "Unknown"
  
  nam <- c(SSAR_year,weight_year,value_year)
  TA2dat <- cbind(TA2dat, Fisheries[match(TA2dat$csquares,Fisheries$csquares), c(nam)])
  TA2dat[,c(nam)][is.na(TA2dat[,c(nam)])] <- 0
  TA2dat$avgsar    <- TA2dat[,SSAR_year] # get the c-sq average within c-sq per year
  TA2dat$avgweight <- TA2dat[,weight_year] * (TA2dat$area_km2 / TA2dat$area_sqkm) # distribute weight equally across MSFD habitats in each c-sq
  TA2dat$avgvalue  <- TA2dat[,value_year] * (TA2dat$area_km2 / TA2dat$area_sqkm) # distribute value equally across MSFD habitats in each c-sq
  
  TA2dat$sweptarea <- TA2dat[,"avgsar"]*TA2dat$area_km2
  
  # estimate proportion of area swept each year
  TA2dat$propswept <- TA2dat$sweptarea
  TA2dat$propswept <- ifelse(TA2dat$sweptarea > TA2dat$area_km2,TA2dat$area_km2,TA2dat$sweptarea)
  
  # estimate proportion of area fished (all area within c-squares with SAR > 0)
  TA2dat$propfished <- ifelse(TA2dat[,"avgsar"] > 0,1,0)
  TA2dat$propfished <- TA2dat$propfished * TA2dat$area_km2
  
  nam <- c("area_km2","grid", "avgweight", "avgvalue","sweptarea","propfished","propswept")
  indexcol <- which(names(TA2dat) %in% nam)
  A2table = aggregate( TA2dat[, indexcol], by= list(TA2dat$MSFD), FUN=function(x){sum(x)})
  names(A2table)[1] = 'MSFD'
  
  A2table <- as.data.frame(A2table)
  A2table <- A2table[order(A2table$sweptarea,decreasing = T),]
  A2table$MSFD <- as.character(A2table$MSFD)
  
  A2table$avgsar <- A2table$sweptarea/A2table$area_km2
  A2table$propfished <- A2table$propfished / A2table$area_km2
  A2table$propswept <- A2table$propswept / A2table$area_km2
  A2table$area_km2 <- A2table$area_km2/1000 
  A2table$sweptarea <- A2table$sweptarea/1000
  A2table$avgweight <- A2table$avgweight/1000000
  A2table$avgvalue <- A2table$avgvalue/1000000
  
  A2table$eff_fish <- NA
  llenght <- max(which(A2table$grid>20))
  for (i in 1:llenght){
    hab <- subset(TA2dat,TA2dat$MSFD == A2table[i,1])
    hab <- hab[order(hab[,"sweptarea"],decreasing = T),]
    hab$cumSSAR <- cumsum(hab[,"sweptarea"])
    hab$cumSSAR <- hab$cumSSAR / sum(hab[,"sweptarea"])
    A2table$eff_fish[i] <- ifelse(is.na(hab$cumSSAR[1]),NA,min(which (hab$cumSSAR > .9))/nrow(hab))
  }
  
  save(A2table, file="TableA2.RData")
  
#####
# Figure A.4
################
  A4dat <-  Region@data
  
  # get SAR for the whole period
  SSARNames <- paste("surface_sar",Period,sep="_") 
  A4dat <- cbind(A4dat, Fisheries[match(A4dat$csquares,Fisheries$csquares), c(SSARNames)])
  A4dat[,c(SSARNames)][is.na(A4dat[,c(SSARNames)])] <- 0
  
  # get data for left, middle and right panel
  A4left <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A4middle <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A4right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  # calculate indicator 1 average SAR all years all data
  indexcol <- which(names(A4dat) %in% SSARNames)
  A4dat[,indexcol] <- A4dat[,indexcol] * A4dat$area_sqkm
  All = apply(A4dat[, indexcol], 2,  FUN=function(x){sum(x)})
  A4left[,1] <- All/sum(A4dat$area_sqkm) 
  A4dat[,indexcol] <- A4dat[,indexcol] / A4dat$area_sqkm
  
  # calculate indicator 3 area swept per year
  A4dat_swept <- A4dat
  A4dat_swept[,indexcol] <- A4dat_swept[,indexcol] * A4dat_swept$area_sqkm
  for (j in 1: length(Period)){
    idx <- indexcol[j]
    A4dat_swept[,idx] <- ifelse(A4dat_swept[,idx] > A4dat_swept$area_sqkm,A4dat_swept$area_sqkm,A4dat_swept[,idx])
    A4middle[j,1]     <- sum(A4dat_swept[,idx],na.rm=T)/sum(A4dat_swept$area_sqkm)
  }
  
  # indicator 4 aggregation of fishing pressure per year
  A4dat_swept <- A4dat
  A4dat_swept[,indexcol] <- A4dat_swept[,indexcol] * A4dat_swept$area_sqkm
  for (j in 1: length(Period)){
    idx <- indexcol[j]
    A4dat_swept <- A4dat_swept[order(A4dat_swept[,idx],decreasing = T),]
    if(sum(A4dat_swept[,idx] > 0)){
      A4dat_swept$cumSSAR <- cumsum(A4dat_swept[,idx])
      A4dat_swept$cumSSAR <- A4dat_swept$cumSSAR / sum(A4dat_swept[,idx])
      A4right[j,1] <- min(which (A4dat_swept$cumSSAR > .9))/nrow(A4dat_swept)
    } else {A4right[j,1] <- NA} 
  }
  
  # now estimate again for most extensive MSFD habitats that are fished
  A4msfd <- A4dat
  
  # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  A4msfd <- merge(A4msfd,msfd_csq_new,by = "csquares", all.x =T)
  A4msfd$MSFD <- as.character(A4msfd$MSFD)
  A4msfd$MSFD[A4msfd$MSFD=="Na"]= "Unknown"
  
  mostcommonMSFD <- A2table[1:4,1]
  
  for (imsfd in 1:4){
    hab <- subset(A4msfd,A4msfd$MSFD == mostcommonMSFD[imsfd])
    
    # calculate indicator 1 average SAR all years all data
    indexcol <- which(names(hab) %in% SSARNames)
    hab[,indexcol] <- hab[,indexcol] * hab$area_km2
    All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
    A4left[,(imsfd+1)] <- All/sum(hab$area_km2) 
    hab[,indexcol] <- hab[,indexcol] / hab$area_km2
    
    # calculate indicator 3 area swept per year
    hab_swept <- hab
    hab_swept[,indexcol] <- hab_swept[,indexcol] * hab_swept$area_km2
    for (j in 1: length(Period)){
      idx <- indexcol[j]
      hab_swept[,idx] <- ifelse(hab_swept[,idx] > hab_swept$area_km2,hab_swept$area_km2,hab_swept[,idx])
      A4middle[j,(imsfd+1)]     <- sum(hab_swept[,idx],na.rm=T)/sum(hab_swept$area_km2)
    }
    
    # indicator 4 aggregation of fishing pressure per year
    hab_swept <- hab
    hab_swept[,indexcol] <- hab_swept[,indexcol] * hab_swept$area_km2
    for (j in 1: length(Period)){
      idx <- indexcol[j]
      hab_swept <- hab_swept[order(hab_swept[,idx],decreasing = T),]
      if(sum(hab_swept[,idx] > 0)){
        hab_swept$cumSSAR <- cumsum(hab_swept[,idx])
        hab_swept$cumSSAR <- hab_swept$cumSSAR / sum(hab_swept[,idx])
        A4right[j,(imsfd+1)] <- min(which (hab_swept$cumSSAR > .9))/nrow(hab_swept)
      } else {A4right[j,(imsfd+1)] <- NA} 
    }
  }
  
  A4left <- cbind(A4left,Period)  
  colnames(A4left) <-c("All",mostcommonMSFD,"Year")
  A4middle <- cbind(A4middle,Period); colnames(A4middle) <- colnames(A4left)  
  A4right <- cbind(A4right,Period); colnames(A4right) <- colnames(A4left)  
  A4fig <- list(A4left,A4middle,A4right)
  save(A4fig, file="FigureA4.RData")

#####
# Figure A.5
################
  A5dat <- Region@data
  
  nam <- c(SSAR_year,weight_year,value_year)
  A5dat <- cbind(A5dat, Fisheries[match(A5dat$csquares,Fisheries$csquares), c(nam)])
  A5dat[ ,c(nam)][is.na(A5dat[ ,c(nam)]) ] = 0 
  A5dat<-A5dat[order(-A5dat[,SSAR_year]),]
  A5dat$sweptcumu <-cumsum(A5dat[,SSAR_year])/sum(A5dat[,SSAR_year])
  A5dat$landcumu  <-cumsum(A5dat[,weight_year])/sum(A5dat[,weight_year])
  A5dat$valuecumu <-cumsum(A5dat[,value_year])/sum(A5dat[,value_year])
  A5dat$indixcumu <-(1:nrow(A5dat))/nrow(A5dat)

  save(A5dat, file="FigureA5.RData")

#####
# Table A.3
################
  datT3 <-Region@data
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  A3table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 5))
  
  for (pp in 1:length(gears)){
    datgear <- datT3
    
    nam <- c(paste(gears[pp],"surface_sar",AssYear,sep="_"))
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgsar <- datgear[,c(nam)]
    
    datgear$sweptarea <- datgear[,"avgsar"]*datgear$area_sqkm
    A3table[1,pp] <- sum(datgear$sweptarea,na.rm=T)/1000
    
    nam <- c(paste(gears[pp],"total_weight",AssYear,sep="_"))
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgweight <- datgear[,c(nam)]
    A3table[2,pp] <- sum(datgear$avgweight)/1000000
    
    nam <- c(paste(gears[pp],"total_value",AssYear,sep="_"))
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgvalue <- datgear[,c(nam)]
    A3table[3,pp] <- sum(datgear$avgvalue)/1000000
    
    A3table[4,pp] <- (sum(datgear$avgweight)/1000000) / (sum(datgear$sweptarea,na.rm=T)/1000)
    A3table[5,pp] <- (sum(datgear$avgvalue)/1000000) / (sum(datgear$sweptarea,na.rm=T)/1000)
  }
  
  colnames(A3table) <- gears
  rownames(A3table) <- c("Area swept (1000 km2)","Landings (1000 tonnes)","Value (10^6 euro)",
                         "Landings (1000 tonnes)/Area swept (1000 km2)","Value (10^6 euro)/Area swept (1000 km2)")
  
  save(A3table, file="TableA3.RData")
  
#####
# Figure A.6
#################
  figA6 <- Region@data
  nam <- paste("state",AssPeriod,sep="_")
  figA6 <- cbind(figA6, State_reg[match(figA6$csquares,State_reg$Fisheries.csquares), c(nam)])
  save(figA6, file="FigureA6.RData")
  
#####
# Figure A.7
################
  A7dat <- Region@data
  stateNames <- paste("state",Period,sep="_")
  A7dat <- cbind(A7dat, State_reg[match(A7dat$csquares,State_reg$Fisheries.csquares), c(stateNames)])
  A7dat[,c(stateNames)][is.na(A7dat[,c(stateNames)])] <- 1
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    A7dat <-  subset(A7dat,A7dat$Depth >= -200)
  }
  
# get data for left, right panel
  A7left  <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A7right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

# calculate average state 
  indexcol <- which(names(A7dat) %in% stateNames)
  A7dat[,indexcol] <- A7dat[,indexcol] * A7dat$area_sqkm
  All = apply(A7dat[, indexcol], 2,  FUN=function(x){sum(x)})
  A7left[,1] <- All/sum(A7dat$area_sqkm) 
  A7dat[,indexcol] <- A7dat[,indexcol] / A7dat$area_sqkm
  
  # calculate prop area >0.8 state
  for (j in 1: length(Period)){
    idx <- stateNames[j]
    looparea <-  ifelse(A7dat[,idx] >= 0.8,1,0)
    A7right[j,1] <- sum(looparea * A7dat$area_sqkm)/sum(A7dat$area_sqkm)
  }
  
  # now estimate again for most extensive MSFD habitats
  A7msfd <- A7dat
  
  # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  A7msfd <- merge(A7msfd,msfd_csq_new,by = "csquares", all.x =T)
  A7msfd$MSFD <- as.character(A7msfd$MSFD)
  A7msfd$MSFD[A7msfd$MSFD=="Na"]= "Unknown"
  mostcommonMSFD <- A2table[1:4,1]
  
  for (imsfd in 1:4){
    hab <- subset(A7msfd,A7msfd$MSFD == mostcommonMSFD[imsfd])
    
    # calculate average state
    indexcol <- which(names(hab) %in% stateNames)
    hab[,indexcol] <- hab[,indexcol] * hab$area_km2
    All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
    A7left[,(imsfd+1)] <- All/sum(hab$area_km2) 
    hab[,indexcol] <- hab[,indexcol] / hab$area_km2
    
    # calculate prop area >0.8 state
    for (j in 1: length(Period)){
      idx <- stateNames[j]
      looparea <-  ifelse(hab[,idx] >= 0.8,1,0)
      A7right[j,(imsfd+1)] <- sum(looparea * hab$area_km2)/sum(hab$area_km2)
    }}
  
  A7left <- cbind(A7left,Period)  
  colnames(A7left) <-c("All",mostcommonMSFD,"Year")
  A7right <- cbind(A7right,Period); colnames(A7right) <- colnames(A7left)  
  
  A7fig <- list(A7left,A7right)
  save(A7fig, file="FigureA7.RData")
  
######
# Figure A.8
################
  A8dat <- Region@data
  
  # account for area of MSDS habitat within csquares and make sure total area is < grid cell size 
  tnew <- aggregate(msfd_csq$area_km2, by=list(msfd_csq$csquares),FUN = sum)
  colnames(tnew) <- c("csquares","areanew")
  msfd_csq_new <- cbind(msfd_csq, tnew[match(msfd_csq$csquares,tnew$csquares), c("areanew")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "tot_area"
  A8dat <- merge(A8dat,msfd_csq_new,by = "csquares", all.x =T)
  A8dat$MSFD <- as.character(A8dat$MSFD)
  A8dat$MSFD[A8dat$MSFD=="Na"]= "Unknown"
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    A8dat <-  subset(A8dat,A8dat$Depth >= -200)
  }
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  nam <- paste("state",gears[1],AssYear,sep="_")
  A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  colnames(A8dat)[ncol(A8dat)] <- nam
  A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
  A8dat[,c(nam)] <- A8dat[,c(nam)]*A8dat$area_km2 
  Avgear <- aggregate( A8dat[, c(nam, "area_km2")], by= list(A8dat$MSFD), FUN=function(x){sum(x)})
  Avgear[,nam] <- Avgear[,nam]/Avgear[,"area_km2"]
  colnames(Avgear)[1] <- 'MSFD'
  Avgear <- Avgear[,-(ncol(Avgear))]
  AvMSFD_metier <- as.data.frame(Avgear)
  
  for (pp in 2:length(gears)){
    nam <- paste("state",gears[pp],AssYear,sep="_")
    A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
    colnames(A8dat)[ncol(A8dat)] <- nam
    A8dat[,c(nam)][is.na(A8dat[,c(nam)])] <- 1
    A8dat[,c(nam)] <- A8dat[,c(nam)]*A8dat$area_km2 
    Avgear <- aggregate( A8dat[, c(nam, "area_km2")], by= list(A8dat$MSFD), FUN=function(x){sum(x)})
    Avgear[,nam] <- Avgear[,nam]/Avgear[,"area_km2"]
    colnames(Avgear)[1] <- 'MSFD'
    Avgear <- Avgear[,-(ncol(Avgear))]
    AvMSFD_metier <- cbind(AvMSFD_metier, Avgear[match(AvMSFD_metier$MSFD,Avgear$MSFD), c(2)])
    colnames(AvMSFD_metier)[ncol(AvMSFD_metier)] <- nam
  }
  
  A8 <- subset(AvMSFD_metier, AvMSFD_metier$MSFD %in%  c(mostcommonMSFD))
  save(A8, file="FigureA8.RData")
    
#####
# Table A.4
################
  datT4 <- Region@data
  
  # remove areas deeper than 200 meter for overview of Greater North Sea
  if (Assregion == "Greater North Sea"){
    datT4 <-  subset(datT4,datT4$Depth >= -200)
  }
  
  gears <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_SPF","SDN_DMF","SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL")
  
  A4table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 2))
  
  for (pp in 1:length(gears)){
    datgear <- datT4
    
    nam <- paste(gears[pp],"surface_sar",AssYear,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgsar <- datgear[,nam]
    
    nam <- paste(gears[pp],"total_weight",AssYear,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgweight <- datgear[,nam]

    nam <- paste(gears[pp],"total_value",AssYear,sep="_")
    datgear <- cbind(datgear, FisheriesMet[match(datgear$csquares,FisheriesMet$csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgvalue <- datgear[,nam]
    
    nam <- paste(rep(paste("state",gears[pp],sep="_"),length(AssPeriod)),AssPeriod,sep="_")
    nam <- paste("state",gears[pp],AssYear,sep="_")
    datgear <- cbind(datgear, State_reg[match(datgear$csquares,State_reg$Fisheries.csquares), c(nam)])
    colnames(datgear)[ncol(datgear)] <- nam
    datgear[,c(nam)][is.na(datgear[,c(nam)])] <- 0
    datgear$avgimpact <- 1- datgear[,nam]
    datgear <- subset(datgear,datgear$avgsar >0)
    A4table[1,pp] <- (sum(datgear$avgweight)/1000000) / sum(datgear$avgimpact)
    A4table[2,pp] <- (sum(datgear$avgvalue)/1000000) / sum(datgear$avgimpact)
    }
  
  colnames(A4table) <- gears
  rownames(A4table) <- c("Landings (1000 tonnes)/PD impact","Value (10^6 euro)/PD impact")
  
  save(A4table, file="TableA4.RData")
  
rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion','AssYear','Period','AssPeriod','Assunit'))])

