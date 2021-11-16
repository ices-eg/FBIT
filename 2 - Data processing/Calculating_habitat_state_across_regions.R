
### load sensitivity layer and fishing to calculate impact for an ecoregion

# load the grid for the region
  load(paste(pathdir,"1 - Input env",paste(EcoReg,"region_grid_sensitivity.RData",sep="_"),sep="/")) 

# load the Fisheries for the region
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 

# calculate state and impact for all gears 
  # and 10 different metier groups
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Impact_continuous_longevity.R")
  source("Habitatstatefishing.R") ## takes a few minutes
  
  setwd(pathdir_nogit)
  save(State_reg,file=paste(EcoReg,"state.RData",sep="_"))
  