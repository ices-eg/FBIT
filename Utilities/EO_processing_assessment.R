
###### DATA PROCESSING for ICES ecosystem overview

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

#-------------------------------------------------------------------------------
# Status figure in latest year
# ------------------------------------------------------------------------------
figEO1 <- Region@data
nam <- c(SSAR_year,weight_year,value_year)
figEO1 <- cbind(figEO1, Fisheries[match(figEO1$csquares,Fisheries$csquares), c(nam)])

nam <- c(state_year)
figEO1 <- cbind(figEO1, State_reg[match(figEO1$csquares,State_reg$Fisheries.csquares), c(nam)])
colnames(figEO1)[ncol(figEO1)] <- state_year

library(matrixStats)

# Row quantiles
probs       <- c(0.05,0.5 ,0.95)
quant       <- rowQuantiles(as.matrix(State_reg_boot[,2:ncol(State_reg_boot)]), probs = probs)
state_quant <- data.frame(as.character(State_reg_boot$Fisheries.csquares),quant)
state_quant[,5] <- state_quant[,4]-state_quant[,2]
figEO1 <- cbind(figEO1, state_quant[match(figEO1$csquares,state_quant[,1]), c(5)])
colnames(figEO1)[ncol(figEO1)] <- paste("state_uncertainty",AssYear,sep="_")

save(figEO1, file="EO_Figure1.RData")

#-------------------------------------------------------------------------------
# Overview per habitat type in latest year 0 - 200m
# ------------------------------------------------------------------------------
# get SAR
TA1dat <- cbind(Region@data, Fisheries[match(Region@data$csquares,Fisheries$csquares), c(SSAR_year)])
colnames(TA1dat)[ncol(TA1dat)] <- SSAR_year
TA1dat[,c(SSAR_year)][is.na(TA1dat[,c(SSAR_year)])] <- 0

# get state
TA1dat <- cbind(TA1dat, State_reg[match(TA1dat$csquares,State_reg$Fisheries.csquares), c(state_year)])
colnames(TA1dat)[ncol(TA1dat)] <- state_year
TA1dat <- subset(TA1dat,!(is.na(TA1dat[,state_year])))

# get shallow
TA1dat_shallow <- subset(TA1dat,TA1dat$Depth > -200)

# area
td <- sum(TA1dat_shallow$area_sqkm,na.rm=T) /1000 #(1000 km2)
fd <- sum(TA1dat_shallow$area_sqkm,na.rm=T)/ sum(TA1dat$area_sqkm,na.rm=T)
out1 <- paste(round(td,digits = 0)," (",round(fd,digits = 2),")",sep="")

# frac untrawled within grid cells (irrespective of swept area)
fu <-  ifelse(TA1dat_shallow[,SSAR_year] == 0,1,0)
out2 <- round(sum(fu * TA1dat_shallow$area_sqkm)/sum(TA1dat_shallow$area_sqkm),digits=2)

# get mean SAR and CI
ms    <- mean(TA1dat_shallow[,SSAR_year]) # mean sar
n     <- length(TA1dat_shallow[,SSAR_year]) # Compute the size
stdev <- sd(TA1dat_shallow[,SSAR_year]) # Find the standard deviation
st_er <- stdev / sqrt(n) # Find the standard error
alpha <- 0.05
degfr <- n - 1
t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
margin_error <- t_scr * st_er
out3 <- paste(round(ms,digits = 1)," (",round(margin_error,digits =2),")",sep="")

# frac cells SAR > 0.5
fu <-  ifelse(TA1dat_shallow[,SSAR_year] >0.5,1,0)
out4 <- round(sum(fu * TA1dat_shallow$area_sqkm)/sum(TA1dat_shallow$area_sqkm),digits=2)

# get mean impact and CI
ms    <- mean(1-TA1dat_shallow[,state_year],na.rm=T) # mean state
n     <- length(TA1dat_shallow[,state_year]) # Compute the size
stdev <- sd(1-TA1dat_shallow[,state_year],na.rm=T) # Find the standard deviation
st_er <- stdev / sqrt(n) # Find the standard error
alpha <- 0.05
degfr <- n - 1
t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
margin_error <- t_scr * st_er
out5 <- paste(round(ms,digits = 2)," (",round(margin_error,digits =4),")",sep="")

# frac cells with impact below 0.2
fu <-  ifelse(TA1dat_shallow[,state_year] >0.8,1,0)
fu[is.na(fu)] <- 1
out6 <- round(sum(fu * TA1dat_shallow$area_sqkm)/sum(TA1dat_shallow$area_sqkm),digits=2)

tab200 <- data.frame(out0="Total",out1,out2,out3,out4,out5,out6)

# ------------------------
# do the same for the 8 most dominant habitat type (and "others")
msfd_csq_new <- cbind(msfd_csq, TA1dat[match(msfd_csq$csquares,TA1dat$csquares), c("Depth")])
colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "Depth"
msfd_shallow <- subset(msfd_csq_new,msfd_csq_new$Depth > -200)
id_msfd <- aggregate(msfd_shallow$area_km2,by=list(msfd_shallow$MSFD),FUN=sum,na.rm=T)
id_msfd <- id_msfd[order(-id_msfd[,2]),]

for (hab in 1:8){
  habdat <- subset(msfd_shallow,msfd_shallow$MSFD == id_msfd$Group.1[hab])
  habdat <- cbind(habdat, TA1dat[match(habdat$csquares,TA1dat$csquares), c(SSAR_year,state_year)])
  
  # area
  td <- sum(habdat$area_km2,na.rm=T) /1000 #(1000 km2)
  fd <- sum(habdat$area_km2,na.rm=T)/ sum(TA1dat$area_sqkm,na.rm=T)
  out1 <- paste(round(td,digits = 0)," (",round(fd,digits = 2),")",sep="")
  
  # frac untrawled within grid cells (irrespective of swept area)
  fu <-  ifelse(habdat[,SSAR_year] == 0,1,0)
  out2 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  # get mean SAR and CI
  ms    <- mean(habdat[,SSAR_year]) # mean sar
  n     <- length(habdat[,SSAR_year]) # Compute the size
  stdev <- sd(habdat[,SSAR_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out3 <- paste(round(ms,digits = 1)," (",round(margin_error,digits =2),")",sep="")
  
  # frac cells SAR > 0.5
  fu <-  ifelse(habdat[,SSAR_year] >0.5,1,0)
  out4 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  # get mean impact and CI
  ms    <- mean(1-habdat[,state_year]) # mean state
  n     <- length(habdat[,state_year]) # Compute the size
  stdev <- sd(1-habdat[,state_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out5 <- paste(round(ms,digits = 2)," (",round(margin_error,digits =4),")",sep="")
  
  # frac cells with impact below 0.2
  fu <-  ifelse(habdat[,state_year] >0.8,1,0)
  fu[is.na(fu)] <- 1
  out6 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  out <- data.frame(out0 = id_msfd$Group.1[hab], out1,out2,out3,out4,out5,out6)
  tab200 <- rbind(tab200,out)
}

# now get for all other habitat types
  habdat <- subset(msfd_shallow,!(msfd_shallow$MSFD %in% c(id_msfd$Group.1[1:8])))
  habdat <- cbind(habdat, TA1dat[match(habdat$csquares,TA1dat$csquares), c(SSAR_year,state_year)])
  
  # area
  td <- sum(habdat$area_km2,na.rm=T) /1000 #(1000 km2)
  fd <- sum(habdat$area_km2,na.rm=T)/ sum(TA1dat$area_sqkm,na.rm=T)
  out1 <- paste(round(td,digits = 0)," (",round(fd,digits = 2),")",sep="")
  
  # frac untrawled within grid cells (irrespective of swept area)
  fu <-  ifelse(habdat[,SSAR_year] == 0,1,0)
  out2 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  # get mean SAR and CI
  ms    <- mean(habdat[,SSAR_year]) # mean sar
  n     <- length(habdat[,SSAR_year]) # Compute the size
  stdev <- sd(habdat[,SSAR_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out3 <- paste(round(ms,digits = 1)," (",round(margin_error,digits =2),")",sep="")
  
  # frac cells SAR > 0.5
  fu <-  ifelse(habdat[,SSAR_year] >0.5,1,0)
  out4 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  # get mean impact and CI
  ms    <- mean(1-habdat[,state_year]) # mean state
  n     <- length(habdat[,state_year]) # Compute the size
  stdev <- sd(1-habdat[,state_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out5 <- paste(round(ms,digits = 2)," (",round(margin_error,digits =4),")",sep="")
  
  # frac cells with impact below 0.2
  fu <-  ifelse(habdat[,state_year] >0.8,1,0)
  fu[is.na(fu)] <- 1
  out6 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  out <- data.frame(out0 = "Other",out1,out2,out3,out4,out5,out6)
  tab200 <- rbind(tab200,out)

  #-------------------------------------------------------------------------------
  # Overview per habitat type in latest year 200- 800m
  # ------------------------------------------------------------------------------
  # get SAR
  TA1dat <- cbind(Region@data, Fisheries[match(Region@data$csquares,Fisheries$csquares), c(SSAR_year)])
  colnames(TA1dat)[ncol(TA1dat)] <- SSAR_year
  TA1dat[,c(SSAR_year)][is.na(TA1dat[,c(SSAR_year)])] <- 0
  
  # get state
  TA1dat <- cbind(TA1dat, State_reg[match(TA1dat$csquares,State_reg$Fisheries.csquares), c(state_year)])
  colnames(TA1dat)[ncol(TA1dat)] <- state_year
  TA1dat <- subset(TA1dat,!(is.na(TA1dat[,state_year])))
  
  # get deep
  TA1dat_deep    <- subset(TA1dat,TA1dat$Depth < -200 & TA1dat$Depth > -800)
  
  # area
  td <- sum(TA1dat_deep$area_sqkm,na.rm=T) /1000 #(1000 km2)
  fd <- sum(TA1dat_deep$area_sqkm,na.rm=T)/ sum(TA1dat$area_sqkm,na.rm=T)
  out1 <- paste(round(td,digits = 0)," (",round(fd,digits = 2),")",sep="")
  
  # frac untrawled within grid cells (irrespective of swept area)
  fu <-  ifelse(TA1dat_deep[,SSAR_year] == 0,1,0)
  out2 <- round(sum(fu * TA1dat_deep$area_sqkm)/sum(TA1dat_deep$area_sqkm),digits=2)
  
  # get mean SAR and CI
  ms    <- mean(TA1dat_deep[,SSAR_year]) # mean sar
  n     <- length(TA1dat_deep[,SSAR_year]) # Compute the size
  stdev <- sd(TA1dat_deep[,SSAR_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out3 <- paste(round(ms,digits = 1)," (",round(margin_error,digits =2),")",sep="")
  
  # frac cells SAR > 0.5
  fu <-  ifelse(TA1dat_deep[,SSAR_year] >0.5,1,0)
  out4 <- round(sum(fu * TA1dat_deep$area_sqkm)/sum(TA1dat_deep$area_sqkm),digits=2)
  
  # get mean impact and CI
  ms    <- mean(1-TA1dat_deep[,state_year],na.rm=T) # mean state
  n     <- length(TA1dat_deep[,state_year]) # Compute the size
  stdev <- sd(1-TA1dat_deep[,state_year],na.rm=T) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out5 <- paste(round(ms,digits = 2)," (",round(margin_error,digits =4),")",sep="")
  
  # frac cells with impact below 0.2
  fu <-  ifelse(TA1dat_deep[,state_year] >0.8,1,0)
  fu[is.na(fu)] <- 1
  out6 <- round(sum(fu * TA1dat_deep$area_sqkm)/sum(TA1dat_deep$area_sqkm),digits=2)
  
  tab800 <- data.frame(out0="Total",out1,out2,out3,out4,out5,out6)
  
  # ------------------------
  # do the same for the 3 most dominant habitat type (and "others")
  msfd_csq_new <- cbind(msfd_csq, TA1dat[match(msfd_csq$csquares,TA1dat$csquares), c("Depth")])
  colnames(msfd_csq_new)[ncol(msfd_csq_new)] <- "Depth"
  msfd_shallow <- subset(msfd_csq_new,msfd_csq_new$Depth < -200 & msfd_csq_new$Depth > -800)
  id_msfd <- aggregate(msfd_shallow$area_km2,by=list(msfd_shallow$MSFD),FUN=sum,na.rm=T)
  id_msfd <- id_msfd[order(-id_msfd[,2]),]
  
  for (hab in 1:3){
    habdat <- subset(msfd_shallow,msfd_shallow$MSFD == id_msfd$Group.1[hab])
    habdat <- cbind(habdat, TA1dat[match(habdat$csquares,TA1dat$csquares), c(SSAR_year,state_year)])
    
    td <- sum(habdat$area_km2,na.rm=T) /1000 #(1000 km2)
    fd <- sum(habdat$area_km2,na.rm=T)/ sum(TA1dat$area_sqkm,na.rm=T)
    out1 <- paste(round(td,digits = 0)," (",round(fd,digits = 2),")",sep="")
    
    # frac untrawled within grid cells (irrespective of swept area)
    fu <-  ifelse(habdat[,SSAR_year] == 0,1,0)
    out2 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
    
    # get mean SAR and CI
    ms    <- mean(habdat[,SSAR_year]) # mean sar
    n     <- length(habdat[,SSAR_year]) # Compute the size
    stdev <- sd(habdat[,SSAR_year]) # Find the standard deviation
    st_er <- stdev / sqrt(n) # Find the standard error
    alpha <- 0.05
    degfr <- n - 1
    t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
    margin_error <- t_scr * st_er
    out3 <- paste(round(ms,digits = 1)," (",round(margin_error,digits =2),")",sep="")
    
    # frac cells SAR > 0.5
    fu <-  ifelse(habdat[,SSAR_year] >0.5,1,0)
    out4 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
    
    # get mean impact and CI
    ms    <- mean(1-habdat[,state_year]) # mean state
    n     <- length(habdat[,state_year]) # Compute the size
    stdev <- sd(1-habdat[,state_year]) # Find the standard deviation
    st_er <- stdev / sqrt(n) # Find the standard error
    alpha <- 0.05
    degfr <- n - 1
    t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
    margin_error <- t_scr * st_er
    out5 <- paste(round(ms,digits = 2)," (",round(margin_error,digits =4),")",sep="")
    
    # frac cells with impact below 0.2
    fu <-  ifelse(habdat[,state_year] >0.8,1,0)
    fu[is.na(fu)] <- 1
    out6 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
    
    out <- data.frame(out0 = id_msfd$Group.1[hab], out1,out2,out3,out4,out5,out6)
    tab800 <- rbind(tab800,out)
  }
  
  # now get for all other habitat types
  habdat <- subset(msfd_shallow,!(msfd_shallow$MSFD %in% c(id_msfd$Group.1[1:3])))
  habdat <- cbind(habdat, TA1dat[match(habdat$csquares,TA1dat$csquares), c(SSAR_year,state_year)])
  
  td <- sum(habdat$area_km2,na.rm=T) /1000 #(1000 km2)
  fd <- sum(habdat$area_km2,na.rm=T)/ sum(TA1dat$area_sqkm,na.rm=T)
  out1 <- paste(round(td,digits = 0)," (",round(fd,digits = 2),")",sep="")
  
  # frac untrawled within grid cells (irrespective of swept area)
  fu <-  ifelse(habdat[,SSAR_year] == 0,1,0)
  out2 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  # get mean SAR and CI
  ms    <- mean(habdat[,SSAR_year]) # mean sar
  n     <- length(habdat[,SSAR_year]) # Compute the size
  stdev <- sd(habdat[,SSAR_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out3 <- paste(round(ms,digits = 1)," (",round(margin_error,digits =2),")",sep="")
  
  # frac cells SAR > 0.5
  fu <-  ifelse(habdat[,SSAR_year] >0.5,1,0)
  out4 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  # get mean impact and CI
  ms    <- mean(1-habdat[,state_year]) # mean state
  n     <- length(habdat[,state_year]) # Compute the size
  stdev <- sd(1-habdat[,state_year]) # Find the standard deviation
  st_er <- stdev / sqrt(n) # Find the standard error
  alpha <- 0.05
  degfr <- n - 1
  t_scr <- qt(p=alpha/2, df=degfr,lower.tail=F)
  margin_error <- t_scr * st_er
  out5 <- paste(round(ms,digits = 2)," (",round(margin_error,digits =4),")",sep="")
  
  # frac cells with impact below 0.2
  fu <-  ifelse(habdat[,state_year] >0.8,1,0)
  fu[is.na(fu)] <- 1
  out6 <- round(sum(fu * habdat$area_km2)/sum(habdat$area_km2),digits=2)
  
  out <- data.frame(out0 = "Other",out1,out2,out3,out4,out5,out6)
  tab800 <- rbind(tab800,out)

  save(tab200,file="EO_table1_200.Rdata")
  save(tab800,file="EO_table1_800.Rdata")

#------------------------------
# time series
#------------------------------
  A4dat <-  Region@data
  
  # get SAR for the whole period
  SSARNames <- paste("surface_sar",Period,sep="_") 
  A4dat <- cbind(A4dat, Fisheries[match(A4dat$csquares,Fisheries$csquares), c(SSARNames)])
  A4dat[,c(SSARNames)][is.na(A4dat[,c(SSARNames)])] <- 0
  
  stateNames <- paste("state",Period,sep="_")
  A4dat <- cbind(A4dat, State_reg[match(A4dat$csquares,State_reg$Fisheries.csquares), c(stateNames)])
  A4dat[,c(stateNames)][is.na(A4dat[,c(stateNames)])] <- 1
  
  # get data for left, middle and right panel
  A4left <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A4middle <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  A4right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))
  
  # calculate average SAR all years all data
  indexcol <- which(names(A4dat) %in% SSARNames)
  A4dat[,indexcol] <- A4dat[,indexcol] * A4dat$area_sqkm
  All = apply(A4dat[, indexcol], 2,  FUN=function(x){sum(x)})
  A4left[,1] <- All/sum(A4dat$area_sqkm) 
  A4dat[,indexcol] <- A4dat[,indexcol] / A4dat$area_sqkm
  
  # calculate average state all years all data
  indexcol <- which(names(A4dat) %in% stateNames)
  A4dat[,indexcol] <- A4dat[,indexcol] * A4dat$area_sqkm
  All = apply(A4dat[, indexcol], 2,  FUN=function(x){sum(x)})
  A4middle[,1] <- All/sum(A4dat$area_sqkm) 
  A4dat[,indexcol] <- A4dat[,indexcol] / A4dat$area_sqkm
  
  # calculate prop area >0.8 state
  for (j in 1: length(Period)){
    idx <- stateNames[j]
    looparea <-  ifelse(A4dat[,idx] >= 0.8,1,0)
    A4right[j,1] <- sum(looparea * A4dat$area_sqkm)/sum(A4dat$area_sqkm)
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
  
  id_msfd <- aggregate(msfd_csq$area_km2,by=list(msfd_csq$MSFD),FUN=sum,na.rm=T)
  id_msfd <- id_msfd[order(-id_msfd[,2]),]
  mostcommonMSFD <- id_msfd[1:4,1]
  
  for (imsfd in 1:4){
    hab <- subset(A4msfd,A4msfd$MSFD == mostcommonMSFD[imsfd])
    
    # calculate indicator 1 average SAR all years all data
    indexcol <- which(names(hab) %in% SSARNames)
    hab[,indexcol] <- hab[,indexcol] * hab$area_km2
    All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
    A4left[,(imsfd+1)] <- All/sum(hab$area_km2) 
    
    # calculate average state
    indexcol <- which(names(hab) %in% stateNames)
    hab[,indexcol] <- hab[,indexcol] * hab$area_km2
    All = apply(hab[, indexcol], 2,  FUN=function(x){sum(x)})
    A4middle[,(imsfd+1)] <- All/sum(hab$area_km2) 
    hab[,indexcol] <- hab[,indexcol] / hab$area_km2
    
    # calculate prop area >0.8 state
    for (j in 1: length(Period)){
      idx <- stateNames[j]
      looparea <-  ifelse(hab[,idx] >= 0.8,1,0)
      A4right[j,(imsfd+1)] <- sum(looparea * hab$area_km2)/sum(hab$area_km2)
    }
  }
  
  A4middle <- 1-A4middle # get impact from state
  
  # get names
  A4left <- cbind(A4left,Period)  
  colnames(A4left) <-c("All",mostcommonMSFD,"Year")
  A4middle <- cbind(A4middle,Period); colnames(A4middle) <- colnames(A4left)  
  A4right <- cbind(A4right,Period); colnames(A4right) <- colnames(A4left)  
  
  figEO2 <- list(A4left,A4middle,A4right)
  save(figEO2, file="EO_Figure2.RData")  
  