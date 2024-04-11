rm(list = ls())

## Adjusted by Karin van der Reijden for the ADGEOB (April 2024)
## adjustments:
# - do not load library 'rgdal' as this is outdated.
# - change Period from 2009-2022 to 2012-2022 (uncertainty of VMS coverage in 2009-2012)
# - run code for all EcoReg areas, add the options as a comment
# - fixed typo in preprocessing script
# - changed legend titles in map plot script
# - create single-panel plots of the fishing intensity, and time trends in fishing intensity and %impact < 0.2
# - create two-panel plot of seabed sensitivity and PD-impact

### github folder
  #pathdir <- getwd()
  pathdir <- "D:/FBIT"
  
### folder for restricted data
  pathdir_nogit <- paste(pathdir,"Fisheries restricted",sep=" - ")

### get all libraries to run the work
  source(paste(pathdir,"Utilities/Libraries_FBIT.R",sep="/"))
  
### select the assessment region and years
  EcoReg  <-  "Celtic Seas" #"Baltic Sea" "Greater North Sea"  "Bay of Biscay and the Iberian Coast"
  Period <- 2012:2022 # period with fishing data to calculate impact # changed from 2009-2022 to 2012-2022
  Assunit <- "Ecoregion" # "Ecoregion" or "EEZ" or "OSPARreg (for now only ecoregion possible)"
  Assregion <- EcoReg #  check which to select -> unique(Region@data[,paste(Assunit)])
  AssYear <- 2022 # year to be assessed
  AssPeriod <- 2017:2022 # assessment period /typically  6 years
  
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
  
  if(EcoReg == "Baltic Sea"){
    source("map_plot_Baltic.R")} else {
    source("map_plot.R")}
  source("EO_Output_assessment.R") ## Newly created wrt ADGEOB april 2024: FigSAR, FigIMP, Figure2left, Figure2right
  source("Output_assessment.R")
  
