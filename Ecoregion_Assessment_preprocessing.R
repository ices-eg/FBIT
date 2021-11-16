rm(list = ls())

### libraries needed
library(rgdal)

### github folder
pathdir <- getwd()

### create folder for restricted data
dir.create(paste(pathdir," - Fisheries restricted",sep=""))
pathdir_nogit <- paste(pathdir," - Fisheries restricted",sep="")

EcoReg  <- "Baltic Sea"
Period <- 2009:2020 # period with fishing data to calculate impact

### get fishing data
library(icesVMS)
icesVMS::update_token("vandenderen") ## use your sharepoint name
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 

### estimate state for specified ecoregion per metier per year # takes 5 minutes
source(paste(pathdir,"2 - Data processing/Calculating_habitat_state_across_regions.R",sep="/")) 
