
  library(rgdal)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  
  # set folder directory
  pathdir <- "C:/Users/pdvd/Online for git/FBIT/TAF - ICES tutorial/"

  # load step 3 output
  setwd(paste(pathdir,"Step 3 Predict sensitivity",sep="/"))
  load("region_grid_sensitivity.RData")
  
  # load available fishing data: The shapefile datasets for the OSPAR region are available at:
  # https://www.ices.dk/sites/pub/Publication%20Reports/Advice/2018/Special_requests/ospar.2018.14.pdf
  # The example is using the data for the region of interest for 2016
  
  setwd(paste(pathdir,"Step 4 Calculate impact",sep="/"))
  load("MBCG_2016_area.RData") # fishing data subset 

  # link to grid via c-squares
  bargrid <- cbind(bargrid, fishing@data[match(bargrid@data$csquares,fishing@data$csquares), c(2)]) #TBB
  bargrid <- cbind(bargrid, fishing@data[match(bargrid@data$csquares,fishing@data$csquares), c(3)]) #OT
  bargrid <- cbind(bargrid, fishing@data[match(bargrid@data$csquares,fishing@data$csquares), c(4)]) #TD
  bargrid <- cbind(bargrid, fishing@data[match(bargrid@data$csquares,fishing@data$csquares), c(5)]) #Seine
  colnames(bargrid@data)[(ncol(bargrid)-3):ncol(bargrid)] <- c("TBB_SurfSAR","OT_SurfSAR","TD_SurfSAR","Seine_SurfSAR")

  # now calculate the depletion rate per c-sq 
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
  