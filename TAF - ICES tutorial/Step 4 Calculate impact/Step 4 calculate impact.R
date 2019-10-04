
  library(rgdal)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  
  # set folder directory
  pathdir <- "H:/Werk/Benthic assessments BENTHIS and ICES/TAF - ICES tutorial/"

  # load step 3 output
  setwd(paste(pathdir,"Step 3 Predict sensitivity",sep="/"))
  load("region_grid_sensitivity.RData")
  
  # load available fishing data: The shapefile datasets for the OSPAR region are available at: https://doi.org/10.17895/ices.pub.2861 ####
  workingdir<-paste(pathdir,"Step 4 Calculate impact/ICES.2017.OSPAR.Technical-Service-VMS-fishing-pressure",sep="/")
  
  # select a year and load swept area data
  Period <- 2016
  TBB   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Beam_",Period[1],sep=""))
  OT    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Otter_",Period[1],sep=""))
  TD    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Dredge_",Period[1],sep=""))
  Seine <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Seine_",Period[1],sep=""))
    
  # link to grid via c-squares
  bargrid <- cbind(bargrid, TBB@data[match(bargrid@data$csquares,TBB@data$c_square), c(5)])
  bargrid <- cbind(bargrid, OT@data[match(bargrid@data$csquares,OT@data$c_square), c(5)])
  bargrid <- cbind(bargrid, TD@data[match(bargrid@data$csquares,TD@data$c_square), c(5)])
  bargrid <- cbind(bargrid, Seine@data[match(bargrid@data$csquares,Seine@data$c_square), c(5)])
  
  # add colnames 
  nb <- ncol(bargrid@data)
  colnames(bargrid@data)[(nb-3):nb] <- c("TBB_SurfSAR","OT_SurfSAR","TD_SurfSAR","Seine_SurfSAR")
  
  Depl_TBB  <- 0.14 * bargrid@data$TBB_SurfSAR   ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_OT   <- 0.06 * bargrid@data$OT_SurfSAR    ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_TD   <- 0.20 * bargrid@data$TD_SurfSAR    ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_sein <- 0.06 * bargrid@data$Seine_SurfSAR ### unknown (now similar to otter trawling) 
  Depl <- cbind(Depl_TBB,Depl_OT,Depl_TD,Depl_sein)
  Depl_tot<-rowSums(Depl,na.rm=T)
  bargrid@data$Depl_tot <- Depl_tot
  
  # calculate state from RBS function per grid cell
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("RBS.R")
  
  state <- c()
  for(j in 1:nrow(bargrid)){
   state[j] <- RBS(a=bargrid@data$slope[j],b=bargrid@data$intercept[j],Fd=bargrid@data$Depl_tot[j])
    }
  
  bargrid@data$state <- state
  
  # plot state as a function of total depletion
  plot(bargrid@data$state~bargrid@data$Depl_tot,ylab="State (PD model)",xlab="Depletion (SAR*d)",las=1)
  
  # make a map of benthic state
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R") # warnings are okay
  map_plot(bargrid,"state",purples)  
  
  # estimate state per MSFD habitat
  aggregate(state ~ MSFDhab, data=bargrid@data, mean)
  
  # make a map of swept areas, weight and value
  Total  <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_total_",Period[1],sep=""))
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(5)])
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(6)])
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(7)])
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(8)])
  
  # add colnames 
  nb <- ncol(bargrid@data)
  colnames(bargrid@data)[(nb-3):nb] <- c("SurfaceSAR","Subsurface","totweight","totvalue")

  map_plot(bargrid,"SurfaceSAR",bluegreen)  
  map_plot(bargrid,"Subsurface",bluegreen)  
  map_plot(bargrid,"totweight",yellowred)  
  map_plot(bargrid,"totvalue",yellowred)  
  