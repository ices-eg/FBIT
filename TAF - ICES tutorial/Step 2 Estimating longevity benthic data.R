
# Load necessary libraries and custom functions
source("Libraries_needed.R")
source("Functions.R")


# open benthic data  
  datBen <- read.csv(file="data/Benthic data/Benthic_Data_tutorial.csv",header=T,sep=";")  
  head(datBen)
  
  #Longevity is here assigned in 4 categories (using BENTHIS trait data table)
  #< 1 year
  #1-3 years
  #3-10 years
  #> 10 years
  
  
  datBen[,6:9] <- datBen$Biomass* datBen[,6:9] # multiply biomasss with fuzzy coded trait data

# summarize benthic data per sample ID and calculate the fraction
  namesCol <- c("Biomass","L1","L1_3","L3_10","L10")
  statdat <- aggregate(datBen[, namesCol], by= list(datBen$sample_ID), FUN=function(x){sum(x, na.rm=T)})
  statdat[,c(3:6)] <- statdat[,c(3:6)]/statdat$Biomass
    
# now link to environmental conditions
  statEnv <- read.csv(file="data/Benthic data/Env_Data_tutorial.csv",header=T,sep=",")  
  statEnv <- cbind(statEnv,statdat[match(statEnv$ID,statdat$Group.1), c(3:6)])
  head(statEnv)
  
# For each sampling station, we can describe the distribution of biomass across the longevity classes
  
# let's look at three
  par(mfrow = c(1, 3))
  barplot(as.numeric(statEnv[1,10:13]),names.arg = colnames(statEnv)[10:13],ylim=c(0,1),las=1)
  barplot(as.numeric(statEnv[10,10:13]),names.arg = colnames(statEnv)[10:13],ylim=c(0,1),las=1)
  barplot(as.numeric(statEnv[25,10:13]),names.arg = colnames(statEnv)[10:13],ylim=c(0,1),las=1)
  
  
# From categorical to continuous distribution
# we can rewrite these examples as a cumulative biomass distribution
  
  # Indices of interest
  idx <- c(1, 10, 25)  
  
 example <-  data.frame(Cumb=c(statEnv[idx,10],
                    statEnv[idx,10]+statEnv[idx,11],
                    statEnv[idx,10] + statEnv[idx,11]+statEnv[idx,12]),
             Longevity = c(1,1,1,
                           3,3,3,
                           10,10,10),
             ID = c(rep(statEnv$ID[idx],3)))
 
 # Example dataset you built
 example <- data.frame(
   Cumb = c(statEnv[c(1,10,25),10],
            statEnv[c(1,10,25),10] + statEnv[c(1,10,25),11],
            statEnv[c(1,10,25),10] + statEnv[c(1,10,25),11] + statEnv[c(1,10,25),12]),
   Longevity = c(1,1,1,3,3,3,10,10,10),
   ID = rep(statEnv$ID[c(1,10,25)], 3)
 )
 
 # First plot: linear x-axis
ggplot(example, aes(x = Longevity, y = Cumb, color = ID)) +
   geom_point(size = 4) +
   geom_smooth(se = FALSE, method = "loess", span = 1) +  
   labs(x = "Longevity", y = "Cumulative biomass") +
   theme_minimal(base_size = 14) +
   theme(legend.position = "bottom")
   
#Fit a statistical model to predict variation in longevity as a function of the environment
#We assume that the biomass proportion at each station is a sigmoidal (logistic) function 
# of longevity, which starts at 0 and approaches 1 when longevity becomes large

#Model specifications: 
#  Longevity is log transformed
# Mixed model with binomial distribution
# Mixed model with sampling station as random variable (normal distribution)

# Purpose is to predict (extrapolate) longevity for each grid cell

  
  
  
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
