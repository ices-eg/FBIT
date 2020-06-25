rm(list = ls())

### libraries needed
  library(rgdal)
  library(dplyr)
  library(rje)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  library(gridExtra)

### github folder
  pathdir <- "C:/Users/pdvd/Online for git/FBIT"
  
### folder for restricted data
  pathdir_nogit <- "C:/Users/pdvd/Online for git/FBIT - Fisheries restricted"

### select the assessment region and years
  EcoReg  <- "Baltic Sea"
  Period <- 2009:2018 # period with fishing data to calculate impact
  Assunit <- "Ecoregion" # "Ecoregion" or "EEZ" or "OSPARreg (for now only ecoregion possible)"
  Assregion <- EcoReg #  check which to select -> unique(Region@data[,paste(Assunit)])
  AssYear <- 2018 # year to be assessed
  AssPeriod <- 2013:2018 # assessment period /typically  6 years
  
### load processed file, with longevity and state/impact 
  load(paste(pathdir_nogit,paste(EcoReg,"state.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"Fisheries.RData",sep="_"),sep="/")) 
  load(paste(pathdir_nogit,paste(EcoReg,"fisheries_per_metier.RData",sep="_"),sep="/")) 
  
  setwd(paste(pathdir,"1 - Input env",sep="/"))
  load(paste(EcoReg,"region_grid_sensitivity.RData",sep="_")) 

### run script to process all figures and tables
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Processing_assessment.R")
  
### run script to make final output  
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R")

  source("Output_assessment.R")
  
