
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
  
  Region <- Region[!(is.na(Region$medlong)),]
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
  TA1dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  
  nam <- c(SSAR_year)
  TA1dat <- cbind(TA1dat, Fisheries[match(TA1dat$csquares,Fisheries$csquares), c(nam)])
  colnames(TA1dat)[ncol(TA1dat)] <- SSAR_year
  
# indicator 1 intensity
  TA1dat$sweptarea <- TA1dat[,SSAR_year]*TA1dat$area_sqkm
  ind1 <- sum(TA1dat$sweptarea,na.rm=T)/sum(TA1dat$area_sqkm)

# indicator 2 proportion of grid cells fished (fished irrespective of swept area > 0)
  ind2 <- length(which(TA1dat[,SSAR_year]>0))/nrow(TA1dat)

# indicator 3 proportion of area fished
  TA1dat$sweptarea2 <- TA1dat$sweptarea
  TA1dat$sweptarea2 <- ifelse(TA1dat$sweptarea > TA1dat$area_sqkm,TA1dat$area_sqkm,TA1dat$sweptarea)
  ind3 <- sum(TA1dat$sweptarea2,na.rm=T)/sum(TA1dat$area_sqkm)

# indicator 4 aggregation of fishing pressure
  TA1dat[,SSAR_year][is.na(TA1dat[,SSAR_year])] <- 0
  TA1dat <- TA1dat[order(TA1dat[,SSAR_year],decreasing = T),]
  TA1dat$cumSSAR <- cumsum(TA1dat[,SSAR_year])
  TA1dat$cumSSAR <- TA1dat$cumSSAR / sum(TA1dat[,SSAR_year])
  ind4 <- min(which (TA1dat$cumSSAR > .9))/nrow(TA1dat)
  
# indicator 5 persistently unfished areas
  SSARNames <- paste("surface_sar",AssPeriod,sep="_")
  TA1dat <- cbind(TA1dat, Fisheries[match(TA1dat$csquares,Fisheries$csquares), c(SSARNames)])
  TA1dat[,c(SSARNames)][is.na(TA1dat[,c(SSARNames)])] <- 0
  
  TA1dat_sub <- subset(TA1dat,TA1dat[,SSARNames[1]] == 0 & TA1dat[,SSARNames[2]] == 0 &
                         TA1dat[,SSARNames[3]] == 0 & TA1dat[,SSARNames[4]] == 0 &
                         TA1dat[,SSARNames[5]] == 0 & TA1dat[,SSARNames[6]] == 0)
  ind5 <- nrow(TA1dat_sub)/nrow(TA1dat) # unfished area is a grid cell with a swept area == 0

# indicator 6 average impact
  nam <- c(state_year)
  TA1dat <- cbind(TA1dat, State_reg[match(TA1dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  colnames(TA1dat)[ncol(TA1dat)] <- state_year
  ind6 <- 1- mean(TA1dat[,state_year])

# indicator 7 proportion of area with impact < 0.2 
  ind7 <- length(which(TA1dat[,state_year] >= 0.8))/nrow(TA1dat)

  A1table <- c(ind1,ind2,ind3,ind4,ind5,ind6,ind7)
  save(A1table, file="TableA1.RData")

#####
# Figure A.2
################
  Habitat <- Region@data
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
# Table A.3
################
  TA3dat <- subset(Region@data,Region@data$Depth >= -200  & Region@data$Depth < 0)
  
  TA3dat$grid<-1
  TA3dat$MSFD <- as.character(TA3dat$MSFD)
  TA3dat$MSFD[TA3dat$MSFD=="Na"]= "Unknown"
  TA3dat$MSFD[TA3dat$MSFD=="<Na>"]= "Unknown"
  TA3dat$MSFD[is.na(TA3dat$MSFD)]= "Unknown"
  
  nam <- c(SSAR_year,weight_year,value_year)
  TA3dat <- cbind(TA3dat, Fisheries[match(TA3dat$csquares,Fisheries$csquares), c(nam)])
  TA3dat[,c(nam)][is.na(TA3dat[,c(nam)])] <- 0
  
  nam <- c(state_year)
  TA3dat <- cbind(TA3dat, State_reg[match(TA3dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  colnames(TA3dat)[ncol(TA3dat)] <- state_year
  
  TA3dat$sweptarea <- TA3dat[,SSAR_year]*TA3dat$area_sqkm
  TA3dat$propgridfished <- ifelse(TA3dat[,SSAR_year] > 0,1,0)
  TA3dat$propswept <- TA3dat$sweptarea
  TA3dat$propswept <- ifelse(TA3dat$sweptarea > TA3dat$area_sqkm,TA3dat$area_sqkm,TA3dat$sweptarea)
  
  nam <- c("area_sqkm","grid","sweptarea", paste(weight_year), paste(value_year),"propgridfished","propswept")
  indexcol <- which(names(TA3dat) %in% nam)
  A3table = aggregate( TA3dat[, indexcol], by= list(TA3dat$MSFD), FUN=function(x){sum(x, na.rm=T)})
  names(A3table)[1] = 'MSFD'
  
  A3table <- as.data.frame(A3table)
  A3table <- A3table[order(A3table$area_sqkm,decreasing = T),]
  A3table$MSFD <- as.character(A3table$MSFD)
  
  A3table$area_sqkm <- A3table$area_sqkm/1000 
  A3table$sweptarea <- A3table$sweptarea/1000
  A3table[,weight_year] <- A3table[,weight_year]/1000000
  A3table[,value_year] <- A3table[,value_year]/1000000
  A3table$propgridfished <- A3table$propgridfished / A3table$grid
  A3table$propswept <- (A3table$propswept/1000) / A3table$area_sqkm
  
  nam <- c(paste(SSAR_year), paste(state_year))
  indexcol <- which(names(TA3dat) %in% nam)
  A3tableb = aggregate( TA3dat[, indexcol], by= list(TA3dat$MSFD), FUN=function(x){mean(x, na.rm=T)})
  names(A3tableb)[1] = 'MSFD'
  
  A3tableb <- as.data.frame(A3tableb)
  A3tableb$impact <- 1-A3tableb[,state_year]
  A3table <- cbind(A3table,A3tableb[match(A3table$MSFD,A3tableb$MSFD),c(SSAR_year,"impact")])
  
  A3table$eff_fish <- NA
  llenght <- max(which(A3table$grid>20))
  for (i in 1:llenght){
    hab <- subset(TA3dat,TA3dat$MSFD == A3table[i,1])
    hab <- hab[order(hab[,SSAR_year],decreasing = T),]
    hab$cumSSAR <- cumsum(hab[,SSAR_year])
    hab$cumSSAR <- hab$cumSSAR / sum(hab[,SSAR_year])
    A3table$eff_fish[i] <- ifelse(is.na(hab$cumSSAR[1]),NA,min(which (hab$cumSSAR > .9))/nrow(hab))
  }
  
  save(A3table, file="TableA3.RData")

#####
# Figure A.4
################
  A4dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  SSARNames <- paste("surface_sar",Period,sep="_")
  A4dat <- cbind(A4dat, Fisheries[match(A4dat$csquares,Fisheries$csquares), c(SSARNames)])
  A4dat[,c(SSARNames)][is.na(A4dat[,c(SSARNames)])] <- 0

  # left panel
  # calculate for all data
  indexcol <- which(names(A4dat) %in% SSARNames)
  All = apply(A4dat[, indexcol], 2,  FUN=function(x){mean(x, na.rm=T)})

  # and calculate for most common habitat types
  nam <- c("MSFD",SSARNames)
  indexcol <- which(names(A4dat) %in% nam)
  table_MSFD <- sort(table(A4dat$MSFD),decreasing = T)
  mostcommonMSFD <- names(table_MSFD)[1:4]
  
  AvgMSFD<- A4dat %>% 
    select(all_of(indexcol)) %>%
    filter(MSFD %in% c(mostcommonMSFD))
  
  indexcol <- which(names(AvgMSFD) %in% SSARNames)
  AvgMSFD2 = aggregate(AvgMSFD[, indexcol], by= list(AvgMSFD$MSFD), FUN=function(x){mean(x, na.rm=T)})
  names(AvgMSFD2)[1]= 'MSFD'
  AvgMSFD2<-as.data.frame(AvgMSFD2)
  
  A4left <-cbind((All),t(AvgMSFD2[1:4,2:(length(Period)+1)]),Period)
  colnames(A4left) <-c("All",as.character(AvgMSFD2[1:4,1]),"Year")

# middle panel
  A4middle <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  rown<-c()
  for (i in 1:length(Period)){
    nyear <-  paste("surface_sar",Period[i],sep="_")
    
    A4middle[i,1] <- length(which(A4dat[,nyear]>0.1))/length(A4dat[,nyear])
    A4middle[i,2] <- length(which(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[1]]>0.1))/length(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[1]])
    A4middle[i,3] <- length(which(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[2]]>0.1))/length(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[2]])
    A4middle[i,4] <- length(which(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[3]]>0.1))/length(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[3]])
    A4middle[i,5] <- length(which(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[4]]>0.1))/length(A4dat[,nyear][A4dat$MSFD == mostcommonMSFD[4]])
    rown<-c(rown,paste("Prop_fished",Period[i],sep="_"))
  }
  A4middle<-cbind(A4middle,Period)
  colnames(A4middle) <-colnames(A4left)
  rownames(A4middle) <-rown

# right panel
  A4right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  rown<-c()
  for (i in 1:length(Period)){
    nyear <-  paste("surface_sar",Period[i],sep="_")
    grd<-min(which(cumsum(sort(A4dat[,nyear],dec=T))/sum(A4dat[,nyear])>0.9))
    A4right[i,1]<-grd/nrow(A4dat)
    
    hab1<-subset(A4dat,A4dat$MSFD == mostcommonMSFD[1])
    grd<-min(which(cumsum(sort(hab1[,nyear],dec=T))/sum(hab1[,nyear])>0.9))
    A4right[i,2]<-grd/nrow(hab1)
    
    hab2<-subset(A4dat,A4dat$MSFD == mostcommonMSFD[2])
    grd<-min(which(cumsum(sort(hab2[,nyear],dec=T))/sum(hab2[,nyear])>0.9))
    A4right[i,3]<-grd/nrow(hab2)
    
    hab3<-subset(A4dat,A4dat$MSFD == mostcommonMSFD[3])
    grd<-min(which(cumsum(sort(hab3[,nyear],dec=T))/sum(hab3[,nyear])>0.9))
    A4right[i,4]<-grd/nrow(hab3)
    
    hab4<-subset(A4dat,A4dat$MSFD == mostcommonMSFD[4])
    grd<-min(which(cumsum(sort(hab4[,nyear],dec=T))/sum(hab4[,nyear])>0.9))
    A4right[i,5]<-grd/nrow(hab4)
    
    rown<-c(rown,paste("Prop_90_effort",Period[i],sep="_"))
  }
  
  A4right<-cbind(A4right,Period)
  colnames(A4right) <-colnames(A4left)
  rownames(A4right) <-rown
  
  A4fig <- list(A4left,A4middle,A4right)
  save(A4fig, file="FigureA4.RData")

#####
# Figure A.5
################
  A5dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0 )
  nam <- c(SSAR_year,weight_year,value_year)
  A5dat <- cbind(A5dat, Fisheries[match(A5dat$csquares,Fisheries$csquares), c(nam)])
  A5dat[ ,c(nam)][is.na(A5dat[ ,c(nam)]) ] = 0 
  
  nam <- c(state_year)
  A5dat <- cbind(A5dat, State_reg[match(A5dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  colnames(A5dat)[ncol(A5dat)] <- state_year
  
  A5dat<-A5dat[order(-A5dat[,SSAR_year]),]
  A5dat$sweptcumu <-cumsum(A5dat[,SSAR_year])/sum(A5dat[,SSAR_year])
  A5dat$landcumu  <-cumsum(A5dat[,weight_year])/sum(A5dat[,weight_year])
  A5dat$valuecumu <-cumsum(A5dat[,value_year])/sum(A5dat[,value_year])
  A5dat$indixcumu <-(1:nrow(A5dat))/nrow(A5dat)

  save(A5dat, file="FigureA5.RData")

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
  A7dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  stateNames <- paste("state",Period,sep="_")
  A7dat <- cbind(A7dat, State_reg[match(A7dat$csquares,State_reg$Fisheries.csquares), c(stateNames)])
  A7dat[,c(stateNames)][is.na(A7dat[,c(stateNames)])] <- 1
  
# left panel
  indexcol <- which(names(A7dat) %in% stateNames)
  All = apply( A7dat[, indexcol], 2, FUN=function(x){mean(x)})

# and calculate for most common habitat types
  nam <- c("MSFD",stateNames)
  indexcol <- which(names(A7dat) %in% nam)
  AvgMSFD<- A7dat %>% 
    select(all_of(indexcol)) %>%
    filter(MSFD %in% c(mostcommonMSFD))
  indexcol <- which(names(AvgMSFD) %in% stateNames)
  AvgMSFD2 = aggregate( AvgMSFD[, indexcol], by= list(AvgMSFD$MSFD), FUN=function(x){mean(x)})
  names(AvgMSFD2)= 'MSFD'
  AvgMSFD2<-as.data.frame(AvgMSFD2)

  A7left <-cbind(All,t(AvgMSFD2[1:4,2:(length(Period)+1)]),Period)
  colnames(A7left) <-c("All",as.character(AvgMSFD2[1:4,1]),"Year")

# right panel
  A7right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

  mostcommonMSFD <- sort(mostcommonMSFD)
  rown<-c()
  for (i in 1:length(Period)){
    nyear <-  paste("state",Period[i],sep="_")
    
    A7right[i,1] <- length(which(A7dat[,nyear]>0.8))/length(A7dat[,nyear])
    A7right[i,2] <- length(which(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[1]]>0.8))/length(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[1]])
    A7right[i,3] <- length(which(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[2]]>0.8))/length(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[2]])
    A7right[i,4] <- length(which(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[3]]>0.8))/length(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[3]])
    A7right[i,5] <- length(which(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[4]]>0.8))/length(A7dat[,nyear][A7dat$MSFD == mostcommonMSFD[4]])
    rown<-c(rown,paste("State>0.8",Period[i],sep="_"))
  }
  A7right<-cbind(A7right,Period)
  colnames(A7right) <-colnames(A7left)
  rownames(A7right) <-rown
  
  A7fig <- list(A7left,A7right)
  save(A7fig, file="FigureA7.RData")

######
# Figure A.8 & A.9
################
  A8dat <- Region@data

  # OT_CRU
  nam <- paste(rep("state_OTCRU",length(Period)),Period,sep="_")
  A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  AvOTCRU = aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x, na.rm=T)})
  names(AvOTCRU)[1] = 'MSFD'
  AvOTCRU<-as.data.frame(AvOTCRU)

# OT_REST
  nam <- paste(rep("state_OTREST",length(Period)),Period,sep="_")
  A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  AvOTREST = aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x, na.rm=T)})
  names(AvOTREST)[1] = 'MSFD'
  AvOTREST<-as.data.frame(AvOTREST)

  # TBB all
  nam <-  paste(rep("state_TBB",length(Period)),Period,sep="_")
  A8dat <- cbind(A8dat, State_reg[match(A8dat$csquares,State_reg$Fisheries.csquares), c(nam)])
  AvTBBALL = aggregate( A8dat[, nam], by= list(A8dat$MSFD), FUN=function(x){mean(x, na.rm=T)})
  names(AvTBBALL)[1] = 'MSFD'
  AvTBBALL<-as.data.frame(AvTBBALL)

  AvMSFD_metier<-cbind(AvOTCRU,AvOTREST[,2:(length(Period)+1)],AvTBBALL[,2:(length(Period)+1)])
  A8_A9fig <- subset(AvMSFD_metier, AvMSFD_metier$MSFD %in%  c(mostcommonMSFD))
  save(A8_A9fig, file="FigureA8_A9.RData")

#####
# Table A.4
################
  datT4 <- subset(Region@data,Region@data$Depth >= -200  & Region@data$Depth < 0)
  
  ## get surface sar/ tot weight /tot value for OTREST
  gears     <- c("OT_MIX","OT_MIX_DMF_BEN","OT_MIX_DMF_PEL","OT_DMF","OT_SPF")
  gearnames <- c(paste(gears,"surface_sar",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$OTREST_SurfaceSAR <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste(gears,"total_weight",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$OTREST_totweight <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste(gears,"total_value",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$OTREST_totvalue <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste("state_OTREST",AssYear, sep="_"))
  datT4 <- cbind(datT4, State_reg[match(datT4$csquares,State_reg$Fisheries.csquares), c(gearnames)])
  colnames(datT4)[ncol(datT4)] <- "OTREST_state"
  
  ## get surface sar/ tot weight /tot value for OTCRU
  gears     <- c("OT_CRU","OT_MIX_CRU","OT_MIX_CRU_DMF")
  gearnames <- c(paste(gears,"surface_sar",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$OTCRU_SurfaceSAR <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste(gears,"total_weight",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$OTCRU_totweight <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste(gears,"total_value",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$OTCRU_totvalue <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste("state_OTCRU",AssYear, sep="_"))
  datT4 <- cbind(datT4, State_reg[match(datT4$csquares,State_reg$Fisheries.csquares), c(gearnames)])
  colnames(datT4)[ncol(datT4)] <- "OTCRU_state"
  
  ## get surface sar/ tot weight /tot value for TBB
  gears     <- c("TBB_CRU","TBB_DMF","TBB_MOL")
  gearnames <- c(paste(gears,"surface_sar",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$TBB_SurfaceSAR <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste(gears,"total_weight",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$TBB_totweight <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste(gears,"total_value",AssYear,sep="_"))
  datT4 <- cbind(datT4, FisheriesMet[match(datT4$csquares,FisheriesMet$csquares), c(gearnames)])
  datT4$TBB_totvalue <- rowSums(datT4[,gearnames],na.rm=T)
  
  gearnames <- c(paste("state_TBB",AssYear, sep="_"))
  datT4 <- cbind(datT4, State_reg[match(datT4$csquares,State_reg$Fisheries.csquares), c(gearnames)])
  colnames(datT4)[ncol(datT4)] <- "TBB_state"
  
  gears <- c("OTREST","OTCRU","TBB")
  A4table <- as.data.frame(matrix(data=NA, ncol=length(gears), nrow = 7))
  
  for (pp in 1:length(gears)){
    datgear <- datT4
    
    nam_sar <- paste(gears[pp],"SurfaceSAR",sep="_")
    datgear$sweptarea <- datgear[,nam_sar]*datgear$area_sqkm
    A4table[1,pp] <- sum(datgear$sweptarea,na.rm=T)/1000
    
    nam_weight <- paste(gears[pp],"totweight",sep="_")
    A4table[2,pp] <- sum(datgear[,nam_weight])/1000000
    
    nam_value <- paste(gears[pp],"totvalue",sep="_")
    A4table[3,pp] <- sum(datgear[,nam_value])/1000000
    
    nam_state <- paste(gears[pp],"state",sep="_")
    datgear$avgimpact <- 1- (datgear[,nam_state])
    datgear <- subset(datgear,datgear[,nam_sar] >0)
    A4table[4,pp] <- (sum(datgear[,nam_weight])/1000000) / sum(datgear$avgimpact)
    A4table[5,pp] <- (sum(datgear[,nam_value])/1000000) / sum(datgear$avgimpact)
    A4table[6,pp] <- (sum(datgear[,nam_weight])/1000000) / (sum(datgear$sweptarea,na.rm=T)/1000)
    A4table[7,pp] <- (sum(datgear[,nam_value])/1000000) / (sum(datgear$sweptarea,na.rm=T)/1000)
  }
  colnames(A4table) <- gears
  rownames(A4table) <- c("Area fished (1000 km2)","Landings (1000 tonnes)","Value (10^6 euro)",
                         "Landings (1000 tonnes)/impact","Value (10^6 euro)/impact",
                         "Landings (1000 tonnes)/Area fished (1000 km2)","Value (10^6 euro)/Area fished (1000 km2)")
  
  save(A4table, file="TableA4.RData")

#####
# Figure A.10
################
  A10fig <- Region@data
  stateNames <- paste("state",AssPeriod,sep="_")
  A10fig <- cbind(A10fig, State_reg[match(A10fig$csquares,State_reg$Fisheries.csquares), c(stateNames)])
  save(A10fig, file="FigureA10.RData")
  
#####
# Figure A.13
################
  A13fig <- Region@data

  valueNames <- paste("total_value",AssPeriod,sep="_")
  A13fig <- cbind(A13fig, Fisheries[match(A13fig$csquares,Fisheries$csquares), c(valueNames)])
  A13fig[,c(valueNames)][is.na(A13fig[,c(valueNames)])] <- 0

  SSARNames <- paste("surface_sar",AssPeriod,sep="_")
  A13fig <- cbind(A13fig, Fisheries[match(A13fig$csquares,Fisheries$csquares), c(SSARNames)])
  A13fig[,c(SSARNames)][is.na(A13fig[,c(SSARNames)])] <- 0

  A13fig$AVGvalue <- rowMeans(A13fig[,c(valueNames)])
  A13fig$AVGSAR <- rowMeans(A13fig[,c(SSARNames)])
  A13fig <- A13fig[order(A13fig$AVGvalue),]
  A13fig$cumSSAR <- cumsum(A13fig$AVGSAR)
  
  nb_5perc <- min(which(A13fig$cumSSAR > sum(A13fig$AVGSAR)*0.05))
  nb_10perc <- min(which(A13fig$cumSSAR > sum(A13fig$AVGSAR)*0.1))
  
  A13fig$inout5 <- 1
  A13fig$inout5[1:nb_5perc] <- 0
  
  A13fig$inout10 <- 1
  A13fig$inout10[1:nb_10perc] <- 0
  
  save(A13fig, file="FigureA13.RData")

#####
# Table A.5
################
  TA5dat <- A13fig
  TA5dat <- subset(TA5dat,TA5dat$Depth >= -200  & TA5dat$Depth < 0)
  TA5dat$MSFD <- as.character(TA5dat$MSFD)
  TA5dat$MSFD[TA5dat$MSFD=="Na"]= "Unknown"
  TA5dat$MSFD[TA5dat$MSFD=="<Na>"]= "Unknown"
  TA5dat$MSFD[is.na(TA5dat$MSFD)]= "Unknown"

  valueNames <- paste("total_value",AssPeriod,sep="_")
  Nam <- c("area_sqkm",valueNames)
  indexcol <- which(names(TA5dat) %in% Nam)
  A5table = aggregate( TA5dat[, Nam], by= list(TA5dat$MSFD), FUN=function(x){sum(x, na.rm=T)})
  names(A5table)[1] = 'MSFD'
  
  A5table<-as.data.frame(A5table)
  A5table$value_AVG<-rowMeans(A5table[,3:(length(AssPeriod)+2)])
  A5table <- A5table [,-c(3:(length(AssPeriod)+2))]

  sub5 <- subset(TA5dat,TA5dat$inout5 == 0)
  sub5table = aggregate( sub5[, indexcol], by= list(sub5$MSFD), FUN=function(x){sum(x, na.rm=T)})
  names(sub5table)[1] = 'MSFD'

  sub5table<-as.data.frame(sub5table)
  sub5table$value_AVG<-rowMeans(sub5table[,3:(length(AssPeriod)+2)])
  sub5table <- sub5table [,-c(3:(length(AssPeriod)+2))]

  A5table <- cbind(A5table,sub5table[match(A5table$MSFD,sub5table$MSFD),c(2:3)])
  colnames(A5table)[4:5] <-c("area_sqkm5","value_AVG_5")
  A5table$area_sqkm5perc <- A5table$area_sqkm5 / A5table$area_sqkm
  A5table$value_AVG_5perc <- A5table$value_AVG_5 / A5table$value_AVG

  sub10 <- subset(TA5dat,TA5dat$inout10 == 0)
  sub10table = aggregate( sub10[, indexcol], by= list(sub10$MSFD), FUN=function(x){sum(x, na.rm=T)})
  names(sub10table)[1] = 'MSFD'

  sub10table<-as.data.frame(sub10table)
  sub10table$value_AVG<-rowMeans(sub10table[,3:(length(AssPeriod)+2)])
  sub10table <- sub10table [,-c(3:(length(AssPeriod)+2))]

  A5table <- cbind(A5table,sub10table[match(A5table$MSFD,sub10table$MSFD),c(2:3)])
  colnames(A5table)[8:9] <-c("area_sqkm10","value_AVG_10")

  A5table$area_sqkm10perc <- A5table$area_sqkm10 / A5table$area_sqkm
  A5table$value_AVG_10perc <- A5table$value_AVG_10 / A5table$value_AVG
  
  A5table <- A5table[order(-A5table$area_sqkm),]
  save(A5table, file="TableA5.RData")

rm(list= ls()[!(ls() %in% c('pathdir','pathdir_nogit','Assregion','AssYear','Period','AssPeriod','Assunit'))])

