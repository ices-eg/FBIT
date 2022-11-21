
### load sensitivity layer and fishing to calculate impact for an ecoregion

# load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"region_grid_sensitivity.RData",sep="_"),sep="/")) 

# load the Fisheries for the region
  load(paste(pathdir_nogit,EcoReg,datacall-1,paste(EcoReg,"fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,EcoReg,datacall-1,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 

# calculate state and impact for all gears 
  # and 10 different metier groups
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  source("Habitatstatefishing.R") ## takes a few minutes
  
  pathec <- paste(pathdir_nogit,"/",EcoReg,"/",datacall-1,sep="")
  save(State_reg,file=paste(pathec,paste(EcoReg,"state.RData",sep="_"),sep="/"))
  
  source("Habitatstatefishing_resampled.R") ## takes a few minutes
  State_reg_boot <- State_reg
  save(State_reg_boot,file=paste(pathec,paste(EcoReg,"state_resampled.RData",sep="_"),sep="/"))  
  