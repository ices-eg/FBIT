rm(list = ls())

# libraries needed
  library(rje)
  library(rgdal)

#####################################################################################
### load file with environmental conditions to calculate longevity and impact for all regions data is available
###
  
  pathdir <- "..../"

# now only available for North Sea and Celtic Sea
  setwd(paste(pathdir,"1 - Input env and fishing data",sep="/"))
  load("NorthS_CelticS_Envcond.Rdata") # for now only North Sea and Celtic Sea
  Region <- subset(NoCegrid_env,NoCegrid_env@data$Ecoregion %in% c("Greater North Sea",  "Celtic Seas"))
  
# calculate longevity for North Sea and Celtic Sea
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("NS_CS_longevity.R")
  
# calculate state and impact for North Sea and Celtic Sea
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  
  Period <- c(2009:2016) # years to calculate state and impact 
  source("NS_CS_habitatstatefishing.R") ### takes a while ~30 minutes 
  
  NoCe_state <- Region
  setwd(paste(pathdir,"3 - Processed data",sep="/"))
  save(NoCe_state,file="NoCeSeagrid_state.RData")
  
# do the same for three different metier groups TBBall, OTCRU and OTREST
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  
  Period <- c(2009:2016) # years to calculate state and impact 
  source("NS_CS_habitatstatefishing_permetier.R") ### takes a while ~60 minutes 
  
  NoCe_state_metier <- Region
  setwd(paste(pathdir,"3 - Processed data",sep="/"))
  save(NoCe_state_metier,file="NoCeSeagrid_state_permetiergroup.RData")
  
  