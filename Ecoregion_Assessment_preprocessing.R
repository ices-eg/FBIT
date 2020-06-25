rm(list = ls())

### libraries needed
library(rgdal)

### github folder
pathdir <- "C:/Users/pdvd/Online for git/FBIT - offline/"

### create folder for restricted data
pathdir_nogit <- "C:/Users/pdvd/Online for git"
# setwd(pathdir_nogit)
#dir.create(paste("FBIT - Fisheries restricted"))
pathdir_nogit <- paste(pathdir_nogit,"FBIT - Fisheries restricted",sep="/")

EcoReg  <- "Baltic Sea"
Period <- 2009:2019 # period with fishing data to calculate impact

### get fishing data
library(icesVMS)
icesVMS::update_token("vandenderen") ## use your sharepoint name
source(paste(pathdir,"Utilities/Get_fishing_data.R",sep="/")) 

### estimate state for specified ecoregion per metier per year # takes 5 minutes
source(paste(pathdir,"2 - Data processing/Calculating_habitat_state_across_regions.R",sep="/")) 
