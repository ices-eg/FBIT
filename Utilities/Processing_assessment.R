
###### DATA PROCESSING

###### figures and tables from ICES Demonstration advice (ICES Special Request Advice -> sr.2017.13) 
###### Physical disturbance pressures from bottom-contacting fishing gears and their impacts on seabed habitats/seafloor integrity
SSAR_year <- paste("SurfaceSAR",AssYear,sep="_")
state_year <- paste("state",AssYear,sep="_")
weight_year <- paste("totweight",AssYear,sep="_")
value_year <- paste("totvalue",AssYear,sep="_")

#  possibly assuming a random distribution of trawling tracks
#  within grid cells (see 2017 report of WGSFD and Ellis et al., 2014, Can. J. Fish. Aquat. 71:733-746). 
# total area fished assuming a random distribution of trawling tracks within the grid cell:
Region@data$RandomF_SSAR <-  apply(Region@data[, c(SSAR_year, "area_sqkm")], 1, function (x) {   
  if(!is.na(x[SSAR_year])) sum( (1-pnbinom(q=0, size=100, mu=x[SSAR_year]*x["area_sqkm"])), na.rm=T) /x["area_sqkm"]
} )

setwd(paste(pathdir,"4df - Producing figures and tables",sep="/"))  
dir.create(paste(Assregion))
setwd(paste(pathdir,"4df - Producing figures and tables",Assregion,sep="/"))  
dir.create(paste(AssYear))
setwd(paste(pathdir,"4df - Producing figures and tables",Assregion,AssYear,sep="/"))

Region <- Region[!(is.na(Region$medlong)), ]

#####
# Figure A.1
################
save(Region, file="FigureA1.RData")


#####
# Table A.1
################
TA1dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)

# indicator 1 intensity
TA1dat$sweptarea <- TA1dat[,SSAR_year]*TA1dat$area_sqkm
ind1 <- sum(TA1dat$sweptarea,na.rm=T)/sum(TA1dat$area_sqkm)

# indicator 2 proportion of grid cells fished (fished irrespective of swept area > 0.001)
ind2 <- length(which(TA1dat[,SSAR_year]>0.001))/nrow(TA1dat)

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
SSARNames<-c()
for (i in 1:length(AssPeriod)){
  SSARNames <- c(SSARNames,paste("SurfaceSAR",AssPeriod[i],sep="_"))
}
TA1dat[,c(SSARNames)][is.na(TA1dat[,c(SSARNames)])] <- 0

TA1dat_sub <- subset(TA1dat,TA1dat[,SSARNames[1]] <= 0.001 & TA1dat[,SSARNames[2]] <= 0.001 &
                       TA1dat[,SSARNames[3]] <= 0.001 & TA1dat[,SSARNames[4]] <= 0.001 &
                       TA1dat[,SSARNames[5]] <= 0.001 & TA1dat[,SSARNames[6]] <= 0.001)
ind5 <- nrow(TA1dat_sub)/nrow(TA1dat) # unfished area is a grid cell with a swept area <= 0.001

# indicator 6 average impact
TA1dat[,state_year][is.na(TA1dat[,state_year])] <- 1
ind6 <- 1- mean(TA1dat[,state_year])

# indicator 7 proportion of area with impact < 0.2 
ind7 <- length(which(TA1dat[,state_year] >= 0.8))/nrow(TA1dat)

A1table <- c(ind1,ind2,ind3,ind4,ind5,ind6,ind7)
save(A1table, file="TableA1.RData")

#####
# Figure A.2
################
Habitat <- coordinates(Region)
colnames(Habitat) <- c("longitude","latitude")
Habitat<-as.data.frame(Habitat)
Habitat$EUNIS <- Region@data$EUNIS
Habitat$depth <- Region@data$Depth
Habitat <- subset(Habitat, Habitat$depth < 0 )
Habitat$EUNIS <- as.character(Habitat$EUNIS)
Habitat$EUNIS[Habitat$EUNIS=="Na"]=NA
save(Habitat, file="FigureA2.RData")

#####
# Table A.3
################
TA3dat <- subset(Region@data,Region@data$Depth >= -200  & Region@data$Depth < 0)

TA3dat$grid<-1
TA3dat$EUNIS <- as.character(TA3dat$EUNIS)
TA3dat$EUNIS[TA3dat$EUNIS=="Na"]= "Unknown"
TA3dat$EUNIS[TA3dat$EUNIS=="<Na>"]= "Unknown"
TA3dat$EUNIS[is.na(TA3dat$EUNIS)]= "Unknown"

TA3dat[,SSAR_year][is.na(TA3dat[,SSAR_year])] <- 0
TA3dat[,state_year][is.na(TA3dat[,state_year])] <- 0
TA3dat$sweptarea <- TA3dat[,SSAR_year]*TA3dat$area_sqkm
TA3dat$propgridfished <- ifelse(TA3dat[,SSAR_year] > 0.001,1,0)
TA3dat$propswept <- TA3dat$sweptarea
TA3dat$propswept <- ifelse(TA3dat$sweptarea > TA3dat$area_sqkm,TA3dat$area_sqkm,TA3dat$sweptarea)

nam <- c("area_sqkm","grid","sweptarea", paste(weight_year), paste(value_year),"propgridfished","propswept")
indexcol <- which(names(TA3dat) %in% nam)

#A3table<-TA3dat %>% 
#  group_by(EUNIS) %>%
#  aggregate(funs(sum(., na.rm = TRUE)), .vars= )
A3table = aggregate( TA3dat[, indexcol], by= list(TA3dat$EUNIS), FUN=function(x){sum(x, na.rm=T)})
names(A3table)[1] = 'EUNIS'

A3table <- as.data.frame(A3table)
A3table <- A3table[order(A3table$area_sqkm,decreasing = T),]
A3table$EUNIS <- as.character(A3table$EUNIS)

A3table$area_sqkm <- A3table$area_sqkm/1000 
A3table$sweptarea <- A3table$sweptarea/1000
A3table[,weight_year] <- A3table[,weight_year]/1000000
A3table[,value_year] <- A3table[,value_year]/1000000
A3table$propgridfished <- A3table$propgridfished / A3table$grid
A3table$propswept <- (A3table$propswept/1000) / A3table$area_sqkm

nam <- c(paste(SSAR_year), paste(state_year))
indexcol <- which(names(TA3dat) %in% nam)

#A3tableb<-TA3dat %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(mean(., na.rm = TRUE)),indexcol)
A3tableb = aggregate( TA3dat[, indexcol], by= list(TA3dat$EUNIS), FUN=function(x){mean(x, na.rm=T)})
names(A3tableb)[1] = 'EUNIS'

A3tableb <- as.data.frame(A3tableb)
A3tableb$impact <- 1-A3tableb[,state_year]
A3table <- cbind(A3table,A3tableb[match(A3table$EUNIS,A3tableb$EUNIS),c(SSAR_year,"impact")])

A3table$eff_fish <- NA
llenght <- max(which(A3table$grid>20))
for (i in 1:llenght){
  hab <- subset(TA3dat,TA3dat$EUNIS == A3table[i,1])
  hab <- hab[order(hab[,SSAR_year],decreasing = T),]
  hab$cumSSAR <- cumsum(hab[,SSAR_year])
  hab$cumSSAR <- hab$cumSSAR / sum(hab[,SSAR_year])
  A3table$eff_fish[i] <- min(which (hab$cumSSAR > .9))/nrow(hab)
}

save(A3table, file="TableA3.RData")

#####
# Figure A.4
################
A4dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
SSARNames<-c()
for (i in 1:length(Period)){
  SSARNames <- c(SSARNames,paste("SurfaceSAR",Period[i],sep="_"))
}
A4dat[,c(SSARNames)][is.na(A4dat[,c(SSARNames)])] <- 0

# left panel
# calculate for all data
indexcol <- which(names(A4dat) %in% SSARNames)
#All<-A4dat %>% 
#  summarise_each (funs(mean(., na.rm = TRUE)),indexcol)
All = apply(A4dat[, indexcol], 2,  FUN=function(x){mean(x, na.rm=T)})

# and calculate for most common habitat types
nam <- c("EUNIS",SSARNames)
indexcol <- which(names(A4dat) %in% nam)
table_eunis <- sort(table(A4dat$EUNIS),decreasing = T)
mostcommonEUNIS <- names(table_eunis)[1:4]

AvgEUNIS<- A4dat %>% 
  select(indexcol) %>%
  filter(EUNIS %in% c(mostcommonEUNIS))

indexcol <- which(names(AvgEUNIS) %in% SSARNames)
#AvgEUNIS2<-AvgEUNIS %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(mean(., na.rm = TRUE)), indexcol)
AvgEUNIS2 = aggregate( AvgEUNIS[, indexcol], by= list(AvgEUNIS$EUNIS), FUN=function(x){mean(x, na.rm=T)})
names(AvgEUNIS2)[1]= 'EUNIS'

AvgEUNIS2<-as.data.frame(AvgEUNIS2)

A4left <-cbind((All),t(AvgEUNIS2[1:4,2:(length(Period)+1)]),Period)
colnames(A4left) <-c("All",as.character(AvgEUNIS2[1:4,1]),"Year")

# middle panel
A4middle <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

rown<-c()
for (i in 1:length(Period)){
  nyear <-  paste("SurfaceSAR",Period[i],sep="_")
  
  A4middle[i,1] <- length(which(A4dat[,nyear]>0.1))/length(A4dat[,nyear])
  A4middle[i,2] <- length(which(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[1]]>0.1))/length(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[1]])
  A4middle[i,3] <- length(which(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[2]]>0.1))/length(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[2]])
  A4middle[i,4] <- length(which(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[3]]>0.1))/length(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[3]])
  A4middle[i,5] <- length(which(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[4]]>0.1))/length(A4dat[,nyear][A4dat$EUNIS == mostcommonEUNIS[4]])
  rown<-c(rown,paste("Prop_fished",Period[i],sep="_"))
}
A4middle<-cbind(A4middle,Period)
colnames(A4middle) <-colnames(A4left)
rownames(A4middle) <-rown

# right panel
A4right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

rown<-c()
for (i in 1:length(Period)){
  nyear <-  paste("SurfaceSAR",Period[i],sep="_")
  grd<-min(which(cumsum(sort(A4dat[,nyear],dec=T))/sum(A4dat[,nyear])>0.9))
  A4right[i,1]<-grd/nrow(A4dat)
  
  hab1<-subset(A4dat,A4dat$EUNIS == mostcommonEUNIS[1])
  grd<-min(which(cumsum(sort(hab1[,nyear],dec=T))/sum(hab1[,nyear])>0.9))
  A4right[i,2]<-grd/nrow(hab1)
  
  hab2<-subset(A4dat,A4dat$EUNIS == mostcommonEUNIS[2])
  grd<-min(which(cumsum(sort(hab2[,nyear],dec=T))/sum(hab2[,nyear])>0.9))
  A4right[i,3]<-grd/nrow(hab2)
  
  hab3<-subset(A4dat,A4dat$EUNIS == mostcommonEUNIS[3])
  grd<-min(which(cumsum(sort(hab3[,nyear],dec=T))/sum(hab3[,nyear])>0.9))
  A4right[i,4]<-grd/nrow(hab3)
  
  hab4<-subset(A4dat,A4dat$EUNIS == mostcommonEUNIS[4])
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
A5dat[ ,c(SSAR_year,state_year,weight_year,value_year)][is.na(A5dat[ ,c(SSAR_year,state_year,weight_year,value_year)]) ] = 0 

A5dat<-A5dat[order(-A5dat[,SSAR_year]),]
A5dat$sweptcumu <-cumsum(A5dat[,SSAR_year])/sum(A5dat[,SSAR_year])
A5dat$landcumu  <-cumsum(A5dat[,weight_year])/sum(A5dat[,weight_year])
A5dat$valuecumu <-cumsum(A5dat[,value_year])/sum(A5dat[,value_year])
A5dat$indixcumu <-(1:nrow(A5dat))/nrow(A5dat)

save(A5dat, file="FigureA5.RData")

#####
# Figure A.7
################
A7dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)

stateNames<-c()
for (i in 1:length(Period)){
  stateNames <- c(stateNames,paste("state",Period[i],sep="_"))
}
A7dat[,c(stateNames)][is.na(A7dat[,c(stateNames)])] <- 1

# left panel
indexcol <- which(names(A7dat) %in% stateNames)
#All<-A7dat %>% 
#  summarise_each (funs(mean(., na.rm = TRUE)),indexcol)
All = apply( A7dat[, indexcol], 2, FUN=function(x){mean(x, na.rm=T)})

# and calculate for most common habitat types
nam <- c("EUNIS",stateNames)
indexcol <- which(names(A7dat) %in% nam)

AvgEUNIS<- A7dat %>% 
  select(indexcol) %>%
  filter(EUNIS %in% c(mostcommonEUNIS))

indexcol <- which(names(AvgEUNIS) %in% stateNames)
#AvgEUNIS2<-AvgEUNIS %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(mean(., na.rm = TRUE)), indexcol)
AvgEUNIS2 = aggregate( AvgEUNIS[, indexcol], by= list(AvgEUNIS$EUNIS), FUN=function(x){mean(x, na.rm=T)})
names(AvgEUNIS2)= 'EUNIS'

AvgEUNIS2<-as.data.frame(AvgEUNIS2)

A7left <-cbind(All,t(AvgEUNIS2[1:4,2:(length(Period)+1)]),Period)
colnames(A7left) <-c("All",as.character(AvgEUNIS2[1:4,1]),"Year")

# right panel
A7right <-as.data.frame(matrix(data=NA,ncol=5,nrow=length(Period)))

rown<-c()
for (i in 1:length(Period)){
  nyear <-  paste("state",Period[i],sep="_")
  
  A7right[i,1] <- length(which(A7dat[,nyear]>0.8))/length(A7dat[,nyear])
  A7right[i,2] <- length(which(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[1]]>0.8))/length(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[1]])
  A7right[i,3] <- length(which(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[2]]>0.8))/length(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[2]])
  A7right[i,4] <- length(which(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[3]]>0.8))/length(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[3]])
  A7right[i,5] <- length(which(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[4]]>0.8))/length(A7dat[,nyear][A7dat$EUNIS == mostcommonEUNIS[4]])
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
setwd(paste(pathdir,"3 - Processed data",sep="/"))
load("NoCeSeagrid_state_permetiergroup.Rdata") # for now only North Sea and Celtic Sea
Region_metier <-subset(NoCe_state_metier,NoCe_state_metier@data[,paste(Assunit)] == Assregion)
Region_metier <- Region_metier[!(is.na(Region_metier@data$medlong)), ]
A8dat <- Region_metier@data

# OT_CRU
indexcol <- which(names(A8dat) %in%  paste(rep("State_OTCRU",length(Period)),Period,sep="_"))
#A8dat[,indexcol][is.na(A8dat[,indexcol])] <- 1
#AvOTCRU<-A8dat %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(mean(., na.rm = TRUE)),indexcol)
AvOTCRU = aggregate( A8dat[, indexcol], by= list(A8dat$EUNIS), FUN=function(x){mean(x, na.rm=T)})
names(AvOTCRU)[1] = 'EUNIS'

AvOTCRU<-as.data.frame(AvOTCRU)

# OT_REST
indexcol <- which(names(A8dat) %in%  paste(rep("State_OTREST",length(Period)),Period,sep="_"))
#A8dat[,indexcol][is.na(A8dat[,indexcol])] <- 1
#AvOTREST<-A8dat %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(mean(., na.rm = TRUE)), indexcol)

AvOTREST = aggregate( A8dat[, indexcol], by= list(A8dat$EUNIS), FUN=function(x){mean(x, na.rm=T)})
names(AvOTREST)[1] = 'EUNIS'
AvOTREST<-as.data.frame(AvOTREST)

# TBB all
indexcol <- which(names(A8dat) %in%  paste(rep("State_TBB",length(Period)),Period,sep="_"))
#A8dat[,indexcol][is.na(A8dat[,indexcol])] <- 1
#AvTBBALL<-A8dat %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(mean(., na.rm = TRUE)), indexcol)

AvTBBALL = aggregate( A8dat[, indexcol], by= list(A8dat$EUNIS), FUN=function(x){mean(x, na.rm=T)})
names(AvTBBALL)[1] = 'EUNIS'
AvTBBALL<-as.data.frame(AvTBBALL)

AvEUNIS_metier<-cbind(AvOTCRU,AvOTREST[,2:(length(Period)+1)],AvTBBALL[,2:(length(Period)+1)])
A8_A9fig <- subset(AvEUNIS_metier, AvEUNIS_metier$EUNIS %in%  c(mostcommonEUNIS))

setwd(paste(pathdir,"4df - Producing figures and tables",Assregion,AssYear,sep="/"))
save(A8_A9fig, file="FigureA8_A9.RData")

#####
# Table A.4
################
datT4 <- subset(Region_metier@data,Region_metier@data$Depth >= -200  & Region_metier@data$Depth < 0)

## get surface sar for OTREST
gearnames <- c(paste("OTDMF_SurfaceSAR",AssYear,sep="_"),paste("OTMIX_SurfaceSAR",AssYear,sep="_"),paste("OTMIXDMFBEN_SurfaceSAR",AssYear,sep="_"),
               paste("OTMIXCRUDMF_SurfaceSAR",AssYear,sep="_"), paste("OTSPF_SurfaceSAR",AssYear,sep="_"))
indexgear <- which(names(datT4) %in% gearnames)
OTREST_SurfaceSAR <- rowSums(datT4[,indexgear],na.rm=T)

## get totweight for OTREST
gearnames <- c(paste("OTDMF_totweight",AssYear,sep="_"),paste("OTMIX_totweight",AssYear,sep="_"),paste("OTMIXDMFBEN_totweight",AssYear,sep="_"),
               paste("OTMIXCRUDMF_totweight",AssYear,sep="_"), paste("OTSPF_totweight",AssYear,sep="_"))
indexgear <- which(names(datT4) %in% gearnames)
OTREST_totweight <- rowSums(datT4[,indexgear],na.rm=T)

## get totvalue for OTREST
gearnames <- c(paste("OTDMF_totvalue",AssYear,sep="_"),paste("OTMIX_totvalue",AssYear,sep="_"),paste("OTMIXDMFBEN_totvalue",AssYear,sep="_"),
               paste("OTMIXCRUDMF_totvalue",AssYear,sep="_"), paste("OTSPF_totvalue",AssYear,sep="_"))
indexgear <- which(names(datT4) %in% gearnames)
OTREST_totvalue  <- rowSums(datT4[,indexgear],na.rm=T)

OTREST <- data.frame(OTREST_SurfaceSAR,OTREST_totweight,OTREST_totvalue)
colnames(OTREST) <- c(paste("OTREST","SurfaceSAR",AssYear,sep="_"),paste("OTREST","totweight",AssYear,sep="_"),paste("OTREST","totvalue",AssYear,sep="_"))

datT4 <- cbind(datT4,OTREST)

# get the names for the assessment year
SSAR_TBB_year <- paste("TBB","SurfaceSAR",AssYear,sep="_"); state_TBB_year <- paste("State","TBB",AssYear,sep="_")
weight_TBB_year <- paste("TBB","totweight",AssYear,sep="_"); value_TBB_year <- paste("TBB","totvalue",AssYear,sep="_")
SSAR_OTCRU_year <- paste("OTCRU","SurfaceSAR",AssYear,sep="_"); state_OTCRU_year <- paste("State","OTCRU",AssYear,sep="_")
weight_OTCRU_year <- paste("OTCRU","totweight",AssYear,sep="_"); value_OTCRU_year <- paste("OTCRU","totvalue",AssYear,sep="_")
SSAR_OTREST_year <- paste("OTREST","SurfaceSAR",AssYear,sep="_"); state_OTREST_year <- paste("State","OTREST",AssYear,sep="_")
weight_OTREST_year <- paste("OTREST","totweight",AssYear,sep="_"); value_OTREST_year <- paste("OTREST","totvalue",AssYear,sep="_")

datT4[,SSAR_TBB_year][is.na(datT4[,SSAR_TBB_year])] <- 0
datT4[,SSAR_OTCRU_year][is.na(datT4[,SSAR_OTCRU_year])] <- 0
datT4[,SSAR_OTREST_year][is.na(datT4[,SSAR_OTREST_year])] <- 0

# Area fished
TBBsweptarea <- datT4[,SSAR_TBB_year]*datT4$area_sqkm
OTCRUsweptarea <- datT4[,SSAR_OTCRU_year]*datT4$area_sqkm
OTRESTsweptarea <- datT4[,SSAR_OTREST_year]*datT4$area_sqkm
A4table <- c(sum( OTCRUsweptarea),sum(OTRESTsweptarea),sum(TBBsweptarea))/1000

# Intensity of area fished > 0.001
TBB_T4 <- subset(datT4,datT4[,c(SSAR_TBB_year)] > 0.001) 
OTCRU_T4 <- subset(datT4,datT4[,c(SSAR_OTCRU_year)] > 0.001) 
OTREST_T4 <- subset(datT4,datT4[,c(SSAR_OTREST_year)] > 0.001) 
A4table <- rbind(A4table, c(mean(OTCRU_T4[,SSAR_OTCRU_year]),mean(OTREST_T4[,SSAR_OTREST_year] ),mean(TBB_T4[,SSAR_TBB_year])))

# Aggregation of fishing pressure (90% of fishing effort)
TBB_T4 <- TBB_T4[order(TBB_T4[,SSAR_TBB_year],decreasing = T),]
TBB_T4$cumSSAR <- cumsum(TBB_T4[,SSAR_TBB_year])
TBB_T4$cumSSAR <- TBB_T4$cumSSAR / sum(TBB_T4[,SSAR_TBB_year])
tb09 <- min(which (TBB_T4$cumSSAR > .9))/nrow(TBB_T4)

OTCRU_T4 <- OTCRU_T4[order(OTCRU_T4[,SSAR_OTCRU_year],decreasing = T),]
OTCRU_T4$cumSSAR <- cumsum(OTCRU_T4[,SSAR_OTCRU_year])
OTCRU_T4$cumSSAR <- OTCRU_T4$cumSSAR / sum(OTCRU_T4[,SSAR_OTCRU_year])
oc09 <- min(which (OTCRU_T4$cumSSAR > .9))/nrow(OTCRU_T4)

OTREST_T4 <- OTREST_T4[order(OTREST_T4[,SSAR_OTREST_year],decreasing = T),]
OTREST_T4$cumSSAR <- cumsum(OTREST_T4[,SSAR_OTREST_year])
OTREST_T4$cumSSAR <- OTREST_T4$cumSSAR / sum(OTREST_T4[,SSAR_OTREST_year])
ot09 <- min(which (OTREST_T4$cumSSAR > .9))/nrow(OTREST_T4)

A4table <- rbind(A4table, c(oc09,ot09,tb09))

# average impact
A4table <- rbind(A4table, c(1-mean(OTCRU_T4[,state_OTCRU_year]),1-mean(OTREST_T4[,state_OTREST_year]),1-mean(TBB_T4[,state_TBB_year])))

# landings
A4table <- rbind(A4table, c(sum(OTCRU_T4[,weight_OTCRU_year],na.rm=T),sum(OTREST_T4[,weight_OTREST_year],na.rm=T),sum(TBB_T4[,weight_TBB_year],na.rm=T))/1000000)

# value
A4table <- rbind(A4table, c(sum(OTCRU_T4[,value_OTCRU_year],na.rm=T),sum(OTREST_T4[,value_OTREST_year],na.rm=T),sum(TBB_T4[,value_TBB_year],na.rm=T))/1000000)

# average impact/landings ratio (10^-2)
A4table <- rbind(A4table, A4table[4,]/A4table[5,]*100)

# average impact/value (10^-2)
A4table <- rbind(A4table, A4table[4,]/A4table[6,]*100)

save(A4table, file="TableA4.RData")

#####
# Figure A.12
################
# impact is not calculated from the continuous longevity (takes a lot of time to do it for all the different scenarios)
A12dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)

year <- AssYear
# workingdir<-"H:/Werk/Benthic assessments BENTHIS and ICES/TAF - ICES work/ICES.2017.OSPAR.Technical-Service-VMS-fishing-pressure"
workingdir = '/Users/dfiorent/Desktop/FBIT/1 - Input env and fishing data/ICES.2017.OSPAR.Technical-Service-VMS-fishing-pressure'

TBB   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Beam_",year,sep=""))
OT    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Otter_",year,sep=""))
TD    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Dredge_",year,sep=""))
Seine <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Seine_",year,sep=""))

A12dat <- cbind(A12dat,TBB@data[match(A12dat$squares,TBB@data$c_square),c("SurfaceSAR")])
colnames(A12dat)[ncol(A12dat)]<-"TBB_SSAR"
A12dat <- cbind(A12dat,OT@data[match(A12dat$squares,OT@data$c_square),c("SurfaceSAR")])
colnames(A12dat)[ncol(A12dat)]<-"OT_SSAR"
A12dat <- cbind(A12dat,TD@data[match(A12dat$squares,TD@data$c_square),c("SurfaceSAR")])
colnames(A12dat)[ncol(A12dat)]<-"TD_SSAR"
A12dat <- cbind(A12dat,Seine@data[match(A12dat$squares,Seine@data$c_square),c("SurfaceSAR")])
colnames(A12dat)[ncol(A12dat)]<-"Seine_SSAR"

state_ampl <- c()
ccname <- c()
amplifier <- c(4, 2, 1.25, 1, 0.75, 0.50, 0.25)

# recovery following Hiddink et al. J Applied Ecol. 2018
rzerone <- 5.31/1
ronethree <- 5.31/2
rthreeten <- 5.31/6.5
rten <- 5.31/10
Recov <- c(rzerone,ronethree,rthreeten,rten)

for (j in 1: length(amplifier)){
  
  ### calculate fractions of biomass per longevity class 
  zerone<-A12dat$Lone                      #### fraction of biomass with longevity 0-1
  onethree<-A12dat$Lthree-zerone           #### fraction of biomass with longevity 1-3
  threeten<-A12dat$Lten-(onethree+zerone)  #### fraction of biomass with longevity 3-10
  tenmore<- 1-(threeten+onethree+zerone)   #### fraction of biomass with longevity > 10
  
  ### calculate depletion per gear group following Hiddink et al. and include the amplifier
  Depl_TBB  <- 0.14 * A12dat$TBB_SSAR * amplifier[j]    ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_OT   <- 0.06 * A12dat$OT_SSAR * amplifier[j]     ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_TD   <- 0.20 * A12dat$TD_SSAR * amplifier[j]     ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_sein <- 0.06 * A12dat$Seine_SSAR * amplifier[j]  ### unknown (now similar to otter trawling) 
  
  Depl <- cbind(Depl_TBB,Depl_OT,Depl_TD,Depl_sein)
  Depl_tot<-rowSums(Depl,na.rm=T)
  
  # calculate state for the whole community
  frac_bio <- cbind(zerone,onethree,threeten,tenmore)
  
  dat <-as.data.frame(matrix(data=NA,nrow=nrow(A12dat)))
  for(i in 1:4){
    dat[,i]<-(frac_bio[,i]*(1-Depl_tot/Recov[i]))
  }  
  dat[dat<0] <- 0
  state <- rowSums(dat)
  
  state_ampl <- cbind(state_ampl,state)
  ccname <- c(ccname,paste("state",amplifier[j],sep="_"))
}

state_ampl[,1:7][is.na(state_ampl[,1:7]) & A12dat$Depth >= -200 & A12dat$Depth < 0] <- 1
test <- apply(state_ampl,2,sort,decreasing=F)
state_ampl<-as.data.frame(test)
colnames(state_ampl) <- ccname 

state_ampl$footprop <- 1-(1:nrow(state_ampl))/nrow(state_ampl)

A12fig <- state_ampl

save(A12fig, file="FigureA12.RData")

#####
# Figure A.13
################
A13fig <- Region
A13fig <- as.data.frame(A13fig)
A13fig2<-data.frame(longitude = coordinates(Region)[,1], latitude = coordinates(Region)[,2])
A13fig<-cbind(A13fig2,A13fig)

valueNames<-c()
for (i in 1:length(AssPeriod)){
  valueNames <- c(valueNames,paste("totvalue",AssPeriod[i],sep="_"))
}
A13fig[,c(valueNames)][is.na(A13fig[,c(valueNames)])] <- 0

SSARNames<-c()
for (i in 1:length(AssPeriod)){
  SSARNames <- c(SSARNames,paste("SurfaceSAR",AssPeriod[i],sep="_"))
}
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
TA5dat$EUNIS <- as.character(TA5dat$EUNIS)
TA5dat$EUNIS[TA5dat$EUNIS=="Na"]= "Unknown"
TA5dat$EUNIS[TA5dat$EUNIS=="<Na>"]= "Unknown"
TA5dat$EUNIS[is.na(TA5dat$EUNIS)]= "Unknown"

valueNames<-c()
for (i in 1:length(AssPeriod)){
  valueNames <- c(valueNames,paste("totvalue",AssPeriod[i],sep="_"))
}
Nam <- c("area_sqkm",valueNames)
indexcol <- which(names(TA5dat) %in% Nam)

#A5table<-TA5dat %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(sum(., na.rm = TRUE)),indexcol)
A5table = aggregate( TA5dat[, indexcol], by= list(TA5dat$EUNIS), FUN=function(x){sum(x, na.rm=T)})
names(A5table)[1] = 'EUNIS'

A5table<-as.data.frame(A5table)
A5table$value_AVG<-rowMeans(A5table[,3:(length(AssPeriod)+2)])
A5table <- A5table [,-c(3:(length(AssPeriod)+2))]

sub5 <- subset(TA5dat,TA5dat$inout5 == 0)
#sub5table<-sub5 %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(sum(., na.rm = TRUE)),indexcol)
sub5table = aggregate( sub5[, indexcol], by= list(sub5$EUNIS), FUN=function(x){sum(x, na.rm=T)})
names(sub5table)[1] = 'EUNIS'

sub5table<-as.data.frame(sub5table)
sub5table$value_AVG<-rowMeans(sub5table[,3:(length(AssPeriod)+2)])
sub5table <- sub5table [,-c(3:(length(AssPeriod)+2))]

A5table <- cbind(A5table,sub5table[match(A5table$EUNIS,sub5table$EUNIS),c(2:3)])
colnames(A5table)[4:5] <-c("area_sqkm5","value_AVG_5")
A5table$area_sqkm5perc <- A5table$area_sqkm5 / A5table$area_sqkm
A5table$value_AVG_5perc <- A5table$value_AVG_5 / A5table$value_AVG

sub10 <- subset(TA5dat,TA5dat$inout10 == 0)
#sub10table<-sub10 %>% 
#  group_by(EUNIS) %>%
#  summarise_each (funs(sum(., na.rm = TRUE)),indexcol)
sub10table = aggregate( sub10[, indexcol], by= list(sub10$EUNIS), FUN=function(x){sum(x, na.rm=T)})
names(sub10table)[1] = 'EUNIS'

sub10table<-as.data.frame(sub10table)
sub10table$value_AVG<-rowMeans(sub10table[,3:(length(AssPeriod)+2)])
sub10table <- sub10table [,-c(3:(length(AssPeriod)+2))]

A5table <- cbind(A5table,sub10table[match(A5table$EUNIS,sub10table$EUNIS),c(2:3)])
colnames(A5table)[8:9] <-c("area_sqkm10","value_AVG_10")

A5table$area_sqkm10perc <- A5table$area_sqkm10 / A5table$area_sqkm
A5table$value_AVG_10perc <- A5table$value_AVG_10 / A5table$value_AVG

A5table <- A5table[order(-A5table$area_sqkm),]
save(A5table, file="TableA5.RData")

rm(list= ls()[!(ls() %in% c('pathdir','Assregion','AssYear','Period','AssPeriod'))])

