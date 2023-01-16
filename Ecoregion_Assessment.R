rm(list = ls())

### github folder
  pathdir <- getwd()
  
### folder for restricted data
  pathdir_nogit <- paste(getwd(),"Fisheries restricted",sep=" - ")

### get all libraries to run the work
  source(paste(pathdir,"Utilities/Libraries_FBIT.R",sep="/"))
  
### select the assessment region and years
  EcoReg  <- "Barents Sea"
  Period <- 2009:2021 # period with fishing data to calculate impact
  Assunit <- "Ecoregion" # "Ecoregion" or "EEZ" or "OSPARreg (for now only ecoregion possible)"
  Assregion <- EcoReg #  check which to select -> unique(Region@data[,paste(Assunit)])
  AssYear <- 2021 # year to be assessed
  AssPeriod <- 2016:2021 # assessment period /typically  6 years
  
### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,EcoReg,AssYear,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,EcoReg,AssYear,paste(EcoReg,"state_resampled.RData",sep="_"),sep="/"))
  load(paste(pathdir_nogit,EcoReg,AssYear,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,EcoReg,AssYear,paste(EcoReg,"fisheries_per_metier_comb.RData",sep="_"),sep="/")) 
  
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 
  load(paste(EcoReg,"MSFD_per_csquare.RData",sep="_"))
  
### run script to process all figures and tables
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("EO_processing_assessment.R")
  source("Processing_assessment.R")
  
### run script to make final output  
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R")
  source("map_plot_Baltic.R")
  source("EO_Output_assessment.R")
  source("Output_assessment.R")
  
