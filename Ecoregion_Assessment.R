rm(list = ls())

### libraries needed
  library(rgdal)
  library(dplyr)

### github folder
  pathdir <- "..../"

#####################################################################################
### load processed file, with longevity and state/impact 
###
  setwd(paste(pathdir,"3 - Processed data",sep="/"))
  load("NoCeSeagrid_state.Rdata") # for now only North Sea and Celtic Sea
  
### select the assessment region and years
  Assregion <- "Greater North Sea" ##  "Greater North Sea" or "Celtic Seas"
  AssYear <- 2015 # year to be assessed
  Period <- 2009:2015 # period with data for time series plot
  AssPeriod <- 2010:2015 # assessment period /typically  6 years
  
### select the region 
  Region<-subset(NoCe_state,NoCe_state@data$Ecoregion == Assregion)
  
### run script to process all figures and tables
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("Processing_assessment.R")
  
### run script to make final output  
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R")

  source("Output_assessment.R")
  

  
  
  
