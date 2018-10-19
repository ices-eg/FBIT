
#### script to calculate longevity composition of benthic communities in North Sea and Celtic Sea
#### following relationships by Rijnsdorp et al (Ecol App 2018)

  #### calculate median longevity and fraction of biomass in <3 3-10 and more than 10 years
  b0 <- -5.682973    ### intercept
  b1 <-  3.502197    ### ln(longevity)
  b2 <- -0.082575    ## ln(trawling)
  b3 <-  0.021062    ### mud
  b4 <-  0.018634    ### gravel
  b5 <-  0.042260    ## ln(shear stress)
  b6 <- -0.118155    ## ln(trawling):ln(shear stress)
  b7 <- -0.019554    ## ln(longevity):Gravel

  Mud <- Region@data$Mud
  Gravel <- Region@data$Gravel
  Stress <- log(Region@data$Shearstress+0.01)
  Trawling <- log(rep(0.01,nrow(Region@data)))

  medlong<-exp((logit(0.5)-b0-b3*Mud-b4*Gravel-b5*Stress-b6*Trawling*Stress-b2*Trawling)/(b1+b7*Gravel))
  medlong[medlong<1]  <- 1
  Region@data$medlong<-medlong

  ll<-log(1)
  Region@data$Lone<-expit(b0+b7*Gravel*ll+b3*Mud+b4*Gravel+b5*Stress+b6*Trawling*Stress+b2*Trawling+b1*ll)
  ll<-log(3)
  Region@data$Lthree<-expit(b0+b7*Gravel*ll+b3*Mud+b4*Gravel+b5*Stress+b6*Trawling*Stress+b2*Trawling+b1*ll)
  ll<-log(10)
  Region@data$Lten<-expit(b0+b7*Gravel*ll+b3*Mud+b4*Gravel+b5*Stress+b6*Trawling*Stress+b2*Trawling+b1*ll)

  rm(list = c('b0','b1','b2','b3','b4','b5','b6','b7','Mud','Gravel','Stress','Trawling','ll','medlong'))
  
  