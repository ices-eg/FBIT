##################################################################
# calculate state for each year

# loop each year and calculate state 
  state_year <- c()
  ccname <- c()

  FisheriesMet<-cbind(FisheriesMet, Region@data[match(FisheriesMet$csquares,Region@data$csquares), c("intercept","slope")])

# depletion rates is based on  Hiddink et al. PNAS 2017 / Rijnsdorp et al. ICES 2020
# percentiles are taken from Hiddink table S4. Relative difference in d percentiles is fixed per gear type
  nb <- 100

  quant <- (c(0.13, 0.20, 0.30))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_DRB_MOL <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])
  
  quant <- (c(0.100/3, 0.100, 0.100*2.67))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_CRU <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])
  
  quant <- (c(0.026/3, 0.026, 0.026*2.67))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_DMF <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])
  
  quant <- (c(0.074/3, 0.074, 0.074*2.67))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_MIX <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])
  
  quant <- (c(0.009/3, 0.009, 0.009*2.67))*10 # does not convergence small numbers
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_SPF <- (rlogitnorm(nb, mu=theta[1], sigma=theta[2]))/10

  quant <- (c(0.009/3, 0.009, 0.009*2.67))*10 # does not convergence small numbers
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_SDN_DMF <- (rlogitnorm(nb, mu=theta[1], sigma=theta[2]))/10
  
  quant <- (c(0.016/3, 0.016, 0.016*2.67))*10 # does not convergence small numbers
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_SSC_DMF <- (rlogitnorm(nb, mu=theta[1], sigma=theta[2]))/10
  
  quant <- (c(0.060/2, 0.060, 0.060*1.79))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB_CRU <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])

  quant <- (c(0.140/2, 0.140, 0.140*1.79))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB_DMF <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])
  
  quant <- (c(0.060/2, 0.060, 0.060*1.79))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB_MOL <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])
  
  # get H
  med <- 5.31 # see Hiddink et al. 2019 J Applied Ecology
  lower <- 2.43 # 2.5 
  upper <- 11.44 # 97.5
  
  x <- c(lower,med,upper)
  x <- log(x) # log normal distribution
  sdev <- (x[2]-x[1])/2 # calculate sd (95% confidence is 2x standard deviation)
  H_boot <- exp(rnorm(nb, mean = log(5.31), sd = sdev)) 
  
  for (boot in 1:nb){
    for (i in 1: length(Period)){
  loopdata <- FisheriesMet
  Depl_DRB_MOL    <- d_DRB_MOL[boot] * loopdata[,paste("DRB_MOL_surface_sar",Period[i],sep="_")]
  Depl_OT_CRU     <- d_OT_CRU[boot] * loopdata[,paste("OT_CRU_surface_sar",Period[i],sep="_")]
  Depl_OT_DMF     <- d_OT_DMF[boot] * loopdata[,paste("OT_DMF_surface_sar",Period[i],sep="_")]
  Depl_OT_MIX     <- d_OT_MIX[boot] * loopdata[,paste("OT_MIX_surface_sar",Period[i],sep="_")]
  Depl_OT_SPF     <- d_OT_SPF[boot] * loopdata[,paste("OT_SPF_surface_sar",Period[i],sep="_")]
  Depl_SDN_DMF    <- d_SDN_DMF[boot] * loopdata[,paste("SDN_DMF_surface_sar",Period[i],sep="_")]
  Depl_SSC_DMF    <- d_SSC_DMF[boot] * loopdata[,paste("SSC_DMF_surface_sar",Period[i],sep="_")]
  Depl_TBB_CRU    <- d_TBB_CRU[boot] * loopdata[,paste("TBB_CRU_surface_sar",Period[i],sep="_")]
  Depl_TBB_DMF    <- d_TBB_DMF[boot] * loopdata[,paste("TBB_DMF_surface_sar",Period[i],sep="_")]
  Depl_TBB_MOL    <- d_TBB_MOL[boot] * loopdata[,paste("TBB_MOL_surface_sar",Period[i],sep="_")]
  
  H  <- H_boot[boot] # recovery  = H/ longevity
  
  ### calculate state for all gears
  Depl <- cbind(Depl_DRB_MOL,Depl_OT_CRU,Depl_OT_DMF,Depl_OT_MIX,
                Depl_OT_SPF,Depl_SDN_DMF,Depl_SSC_DMF,Depl_TBB_CRU,Depl_TBB_DMF,Depl_TBB_MOL)
  Depl_tot<-rowSums(Depl,na.rm=T)
  loopdata$Depl_tot <- Depl_tot
  
  # calculate slope and intercept and use RBS function
  state <- rep(1,nrow(loopdata))
  
  for(j in 1:nrow(loopdata)){
    if (loopdata$Depl_tot[j] > 0 ) {
      state[j] <- RBS(Fd=loopdata$Depl_tot[j],a=loopdata$slope[j],b=loopdata$intercept[j],H=H)
    }}
  
  state_year <- cbind(state_year,state)
  ccname <- c(ccname,paste("state",Period[i],boot,sep="_"))
    }
  }
  colnames(state_year)<-ccname
  State_reg <- data.frame(Fisheries$csquares,state_year)
  