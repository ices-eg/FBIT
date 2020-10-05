
library(lme4)

# set folder directory
  pathdir <- "C:/Users/pdvd/Online for git/FBIT/TAF - ICES tutorial/Step 2 Estimate relationships benthic data/"

# open benthic data  
  setwd(paste(pathdir,"Benthic data",sep="/"))
  datBen <- read.csv(file="Benthic_Data_tutorial.csv",header=T,sep=";")  
  datBen[,6:9] <- datBen$Biomass* datBen[,6:9] # multiply biomasss with fuzzy coded trait data

# summarize benthic data per sample ID and calculate the fraction
  namesCol <- c("Biomass","L1","L1_3","L3_10","L10")
  statdat <- aggregate(datBen[, namesCol], by= list(datBen$sample_ID), FUN=function(x){sum(x, na.rm=T)})
  statdat[,c(3:6)] <- statdat[,c(3:6)]/statdat$Biomass
    
# now link to environmental conditions
  statEnv <- read.csv(file="Env_Data_tutorial.csv",header=T,sep=";")  
  statEnv <- cbind(statEnv,statdat[match(statEnv$ID,statdat$Group.1), c(3:6)])
  
# select only MSFD habitats with multiple observations
  table(statEnv$MSFDhab)
  statEnv <- subset(statEnv,statEnv$MSFDhab %in% 
                      c("Circalittoral sand","Offshore circalittoral mud","Offshore circalittoral sand"))
  
# now prepare for statistical analysis
  
  # get longevity categories seperate for each station 
  ID        <-rep(statEnv$ID,3)
  MSFD      <-rep(statEnv$MSFDhab,3)
  Cumb      <-c(statEnv$L1,(statEnv$L1+statEnv$L1_3),(statEnv$L1+statEnv$L1_3+statEnv$L3_10))
  Longevity <-c(rep(1,nrow(statEnv)),rep(3,nrow(statEnv)),rep(10,nrow(statEnv)))  

  fulldat   <-data.frame(ID,MSFD,Cumb,Longevity) 
  fulldat$ll <-log(fulldat$Longevity)
  
  # add a small number to values very close to 0 and 1 
  for (i in 1:(nrow(fulldat))){
    if (fulldat$Cumb[i] < 1e-3){ fulldat$Cumb[i] <- 1e-3}
    if (fulldat$Cumb[i] > 0.999){fulldat$Cumb[i] <- 0.999}
  }   
  
  # fit a linear mixed model with sampling station as random factor and MSFD habitats as exploratory variable
  mod1   <-  glmer(Cumb ~ ll + MSFD*ll + (1 | ID), data=fulldat, family=binomial)
  mod2   <-  glmer(Cumb ~ ll + MSFD + (1 | ID), data=fulldat, family=binomial)
  mod3   <-  glmer(Cumb ~ ll + (1 | ID), data=fulldat, family=binomial)
  AIC(mod1,mod2,mod3)
  # models give a singular fit --> the random effect is very small (but you can argue that it, in principle, has to be included)
 
  modcoeff  <-  fixef(mod2)

  setwd(paste(pathdir))
  save(modcoeff,file="Coefficients_Bdata.RData")  
