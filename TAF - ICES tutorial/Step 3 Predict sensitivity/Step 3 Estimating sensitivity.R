
  library(rje)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)

  # set folder directory
  pathdir <- "C:/Users/pdvd/Online for git/FBIT/TAF - ICES tutorial/"
  
  # load output step 1 
  setwd(paste(pathdir,"Step 1 Assign region of interest/Data layers",sep="/"))
  load("region_grid.RData")
  
  # only select region for which we had sampling data in step 2
  bargrid <- subset(bargrid,bargrid@data$MSFDhab %in% c("Circalittoral sand","Offshore circalittoral mud","Offshore circalittoral sand"))
  
  # load output step 2
  setwd(paste(pathdir,"Step 2 Estimate relationships benthic data",sep="/"))
  load("Coefficients_Bdata.RData")  

  coef_int <- modcoeff[1]
  coef_ll  <- modcoeff[2]
  coef_OCM <- modcoeff[3]
  coef_OCS <- modcoeff[4]
  
  # for each grid cell, longevity can now be predicted at a certain cumulative biomass
  # for example we can show median longevity (medLong, see below) (the value where 50% of biomass is longer-living)
  # to estimate longevity at a certain cumulative biomass:
  # 1) the statistical model -- Cumb ~ ll + MSFD + (1 | ID) -- needs to be reshuffled
  # 2) longevity is log transformed in the model so we backtransform --> exp(log(x)) = x
  # 3) cumulative biomass 0.5 needs a logit as the statistical model is binomial; 
  #     logit(p) == log(p/(1-p)) ; p = exp(A)/(1+exp(A))
  
  # prepare grid specific information to predict longevity at a certain location
  OCM <- ifelse(bargrid@data$MSFDhab == "Offshore circalittoral mud",1,0)
  OCS <- ifelse(bargrid@data$MSFDhab == "Offshore circalittoral sand",1,0)
  medLong <- exp((logit(0.5)- coef_int - coef_OCM*OCM - coef_OCS*OCS) / coef_ll)
  bargrid@data$medLong <- medLong
  
  # plot the median longevity to see which areas are more sensitive than others
  # load map code 
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R") # warnings are okay
  map_plot(bargrid,"medLong",bluegreen)  

  # to predict impact we will not use median longevity but the longevity distribution
  # hence estimate the slope and intercept for each gridcell
  slope <- rep(coef_ll,nrow(bargrid@data))  # slope of binomial model
  intercept <- coef_int + coef_OCM*OCM + coef_OCS*OCS  # intercept of binomial model
  
  bargrid@data$intercept <- intercept
  bargrid@data$slope     <- slope
  
  setwd(paste(pathdir,"Step 3 Predict sensitivity",sep="/"))  
  save(bargrid,file = "region_grid_sensitivity.RData")  
  