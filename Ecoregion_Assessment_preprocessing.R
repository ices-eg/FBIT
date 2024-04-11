rm(list = ls())

### libraries needed
#library(rgdal)

### github folder
pathdir <- "D:/FBIT"

### create folder for restricted data
dir.create(paste(pathdir," - Fisheries restricted",sep=""))
pathdir_nogit <- paste(pathdir," - Fisheries restricted",sep="")

### get fishing data
library(icesVMS);library(icesConnect)
#icesConnect::ices_token("vandenderen")
icesConnect::set_username("vandenderen") ## use your sharepoint name
datacall <- 2023
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 

### create fishing data per ecoregion
EcoReg  <- "Celtic Seas"
Period <- 2012:(datacall-1) # period with fishing data to calculate impact # changed to 2012-..
source(paste(pathdir,"Utilities/Merge_fishing_data_per_region.R",sep="/")) 

### estimate state for specified ecoregion per metier per year # takes 5 minutes
library(logitnorm)
source(paste(pathdir,"2 - Data processing/Calculating_habitat_state_across_regions.R",sep="/")) 
