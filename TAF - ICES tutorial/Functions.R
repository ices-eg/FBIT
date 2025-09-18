
# functions for FBIT fisheries-benthic impact risk assessment framework


# ------------------------------------------------------------------------------
# relative benthic state for 10% most long-lived fauna - RBS_sens

RBS_sens <- function(Fd,a,b,H){        
  #  a  = slope of binomial model, 
  #  b  = intercept of binomial model, 
  #  Fd = fishing SAR x depletion rate (gear specific)
  #  H  = used to translate longevity to recovery (r = H/longevity)
  
  # 3 equations
  step.size=.5
  longevity=seq(1,200,by=step.size)
  
  r = H/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  idx <- c(which(abs(cumsum(K)/sum(K) - 0.9) == min(abs(cumsum(K)/sum(K) - 0.9))):length(K))
  K_sen <- K[idx]/sum(K[idx]*step.size)
  B = K_sen*(1 - Fd / r[idx]); B[B<0]=0
  
  RBS_sens=sum(B)*step.size
  RBS_sens
}


# ------------------------------------------------------------------------------
# Relative benthic state for total biomass
RBS <- function(Fd,a,b,H){      
  #  a  = slope of binomial model, 
  #  b  = intercept of binomial model, 
  #  Fd = fishing SAR x depletion rate (gear specific)
  #  H  = used to translate longevity to recovery (r = H/longevity)
  # 3 equations
  step.size=.5
  longevity=seq(1,200,by=step.size)
  
  r = H/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  K[1] <- K[1] + ((1/step.size) - sum(K))
  B = K*(1 - Fd / r); B[B<0]=0
  
  RBS=sum(B)*step.size
  RBS
  
}

# ------------------------------------------------------------------------------
# get depletion, recovery values
get_depletion_recovery_param <- function(nsim = 1){
  
  #  nsim  = nb of resamples of each parameter
  #  nsim = 1 gives median value
  
  # generates two type of depletion values
  
  # 1)
  # gear names typically used within ICES 
  # "DRB_MOL", "OT_CRU", "OT_DMF", "OT_MIX", "OT_MIX_CRU_DMF", "OT_MIX_DMF_BEN",
  # "OT_MIX_DMF_PEL", "OT_SPF", "SDN_DMF", "SSC_DMF", "TBB_CRU", "TBB_DMF", "TBB_MOL",       
  
  # 2)
  # more generic gear names
  # "OTB", "TBB", "TD", "HD"
  
  # depletion rates is based on  Hiddink et al. PNAS 2017 / Rijnsdorp et al. ICES 2020
  # percentiles are taken from Hiddink table S4. Relative difference in d percentiles is fixed per gear type
  
  quant     <- c(0.13, 0.200, 0.30)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_DRB_MOL <- c(0.200, rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.100/3, 0.100, 0.100*2.67)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_CRU  <- c(0.100, rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.026/3, 0.026, 0.026*2.67)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_DMF  <- c(0.026, rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.074/3, 0.074, 0.074*2.67)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_MIX  <- c(0.074,rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  # note OT_MIX combines OT_MIX, "OT_MIX_CRU_DMF", "OT_MIX_DMF_BEN" & "OT_MIX_DMF_PEL"
  
  quant     <- c(0.009/3, 0.009, 0.009*2.67)*10 # does not convergence small numbers
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OT_SPF  <- c(0.009,(rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))/10)
  
  quant     <- c(0.009/3, 0.009, 0.009*2.67)*10 # does not convergence small numbers
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_SDN_DMF <- c(0.009,(rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))/10)
  
  quant     <- c(0.016/3, 0.016, 0.016*2.67)*10 # does not convergence small numbers
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_SSC_DMF <- c(0.016,(rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))/10)
  
  quant     <- c(0.060/2, 0.060, 0.060*1.79)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB_CRU <- c(0.060, rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.140/2, 0.140, 0.140*1.79)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB_DMF <- c(0.14,rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.060/2, 0.060, 0.060*1.79)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB_MOL <- c(0.060,rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.02, 0.06, 0.16)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_OTB     <- c(0.06, rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.07, 0.14, 0.25)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TBB     <- c(0.14,rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.13, 0.200, 0.30)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_TD      <- c(0.200, rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  quant     <- c(0.25, 0.400, 0.55)
  theta     <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  d_HD      <- c(0.400,rlogitnorm(nsim, mu=theta[1], sigma=theta[2]))
  
  # get H
  med   <- 5.31 # see Hiddink et al. 2019 J Applied Ecology
  lower <- 2.43 # 2.5 
  upper <- 11.44 # 97.5
  
  x <- c(lower,med,upper)
  x <- log(x) # log normal distribution
  sdev <- (x[2]-x[1])/2 # calculate sd (95% confidence is 2x standard deviation)
  H_par <- c(5.31, exp(rnorm(nsim, mean = log(5.31), sd = sdev)))
  
  out_H_d <- data.frame(d_DRB_MOL, d_OT_CRU, d_OT_DMF,  d_OT_MIX,
                        d_OT_SPF, d_SDN_DMF, d_SSC_DMF, d_TBB_CRU,
                        d_TBB_DMF,d_TBB_MOL, 
                        d_OTB, d_TBB, d_TD, d_HD,
                        H_par)
  
  out_H_d <- out_H_d[-2,]
  

  return(out_H_d)  
}

# ------------------------------------------------------------------------------
# get gear names
get_gear_names <- function(){
out <- c("DRB_MOL","OT_CRU","OT_DMF","OT_MIX","OT_MIX_CRU_DMF",
  "OT_MIX_DMF_BEN","OT_SPF","SDN_DMF",
  "SSC_DMF","TBB_CRU","TBB_DMF","TBB_MOL",
  "OTB","TBB","HD","TD")
return(out)  
}


# ------------------------------------------------------------------------------
# estimate state per grid cell

get_state <- function(dat,H_d_values){  
  
  # dat     = slope, intercept of binomial model, and SAR (gear specific) by c-square
  # H_d_values = dataframe with depletion and recovery parameters (re-sampled)
  
  gear_names <- get_gear_names()
  
  missing_gears <- setdiff(gear_names, colnames(dat))
  
  # Add missing columns with zeros
  for (gear in missing_gears) {
    dat[[gear]] <- 0
  }
  
  PD      <- c()
  PD_sens <- c()
  
  for(i_samp in 1:nrow(H_d_values)){
    Depl_tot <- H_d_values$d_DRB_MOL[i_samp] * dat[,"DRB_MOL"] +
      H_d_values$d_OT_CRU[i_samp]  * dat[,"OT_CRU"] + 
      H_d_values$d_OT_DMF[i_samp]  * dat[,"OT_DMF"] +
      H_d_values$d_OT_MIX[i_samp]  * dat[,"OT_MIX"] +
      H_d_values$d_OT_MIX[i_samp]  * dat[,"OT_MIX_CRU_DMF"] +
      H_d_values$d_OT_MIX[i_samp]  * dat[,"OT_MIX_DMF_BEN"] +
      H_d_values$d_OT_SPF[i_samp]  * dat[,"OT_SPF"] +
      H_d_values$d_SDN_DMF[i_samp] * dat[,"SDN_DMF"] +
      H_d_values$d_SSC_DMF[i_samp] * dat[,"SSC_DMF"] +
      H_d_values$d_TBB_CRU[i_samp] * dat[,"TBB_CRU"] +
      H_d_values$d_TBB_DMF[i_samp] * dat[,"TBB_DMF"] +
      H_d_values$d_TBB_MOL[i_samp] * dat[,"TBB_MOL"] +
      H_d_values$d_OTB[i_samp] * dat[,"OTB"] +
      H_d_values$d_TBB[i_samp] * dat[,"TBB"] +
      H_d_values$d_TD[i_samp] * dat[,"TD"] +
      H_d_values$d_HD[i_samp] * dat[,"HD"]
    
    H  <- H_d_values$H_par[i_samp] # recovery  = H/ longevity
    PD <- c(PD, RBS(Fd= Depl_tot,a=dat$slope,b=dat$intercept,H=H))
    PD_sens <- c(PD_sens, RBS_sens(Fd= Depl_tot,a=dat$slope,b=dat$intercept,H=H))
  }
  return(list(RBS = PD,RBS_sens = PD_sens))
}


# ------------------------------------------------------------------------------
#  c-squares to Lon - Lat coordinates
# Code obtained from VMStools
CSquare2LonLat <- function(csqr,degrees){
  
  ra          <- 1e-6 #Artificial number to add for rounding of 5'ves (round(0.5) = 0, but in Excel (where conversion comes from, it is 1)
  chars       <- as.numeric(nchar(csqr))
  tensqd      <- as.numeric(substr(csqr,1,1))+ra;      gqlat     <- (round(abs(tensqd-4)*2/10)*10/5)-1
  tenslatd    <- as.numeric(substr(csqr,2,2))+ra;      gqlon     <- (2*round(tensqd/10)-1)*-1
  tenslond    <- as.numeric(substr(csqr,3,4))+ra
  unitsqd     <- as.numeric(substr(csqr,6,6))+ra;      iqulat    <- round(unitsqd*2/10)
  unitslatd   <- as.numeric(substr(csqr,7,7))+ra;      iqulon    <- (round((unitsqd-1)/2,1) - floor((unitsqd-1)/2))*2
  unitslond   <- as.numeric(substr(csqr,8,8))+ra
  tenthsqd    <- as.numeric(substr(csqr,10,10))+ra;    iqtlat    <- round(tenthsqd*2/10)
  tenthslatd  <- as.numeric(substr(csqr,11,11))+ra;    iqtlon    <- (round((tenthsqd-1)/2,1) - floor((tenthsqd-1)/2))*2
  tenthslond  <- as.numeric(substr(csqr,12,12))+ra
  hundqd      <- as.numeric(substr(csqr,14,14))+ra;    iqhlat    <- round(hundqd*2/10)
  hundlatd    <- as.numeric(substr(csqr,15,15))+ra;    iqhlon    <- (round((hundqd-1)/2,1) - floor((hundqd-1)/2))*2
  hundlond    <- as.numeric(substr(csqr,16,16))+ra
  reso        <- 10^(1-floor((chars-4)/4))-((round((chars-4)/4,1)-floor((chars-4)/4))*10^(1-floor((chars-4)/4)))
  
  if(degrees < reso[1]) stop("Returning degrees is smaller than format of C-square")
  if(degrees == 10){   
    lat <- ((tenslatd*10)+5)*gqlat-ra                              
    lon <- ((tenslond*10)+5)*gqlon-ra              
  }
  if(degrees == 5){    
    lat <- ((tenslatd*10)+(iqulat*5)+2.5)*gqlat-ra
    lon <- ((tenslond*10)+(iqulon*5)+2.5)*gqlon-ra 
  }
  if(degrees == 1){    
    lat <- ((tenslatd*10)+ unitslatd+0.5)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond+0.5)*gqlon-ra 
  }
  if(degrees == 0.5){  
    lat <- ((tenslatd*10)+ unitslatd + (iqtlat*0.5)+0.25)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (iqtlon*0.5)+0.25)*gqlon-ra
  }         
  if(degrees == 0.1){  
    lat <- ((tenslatd*10)+ unitslatd + (tenthslatd*0.1)+0.05)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (tenthslond*0.1)+0.05)*gqlon-ra
  }
  if(degrees == 0.05){ 
    lat <- ((tenslatd*10)+ unitslatd + (tenthslatd*0.1)+(iqhlat*0.05)+0.025)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (tenthslond*0.1)+(iqhlon*0.05)+0.025)*gqlon-ra
  }
  if(degrees == 0.01){ 
    lat <- ((tenslatd*10)+ unitslatd + (tenthslatd*0.1)+(hundlatd*0.01)+0.005)*gqlat-ra
    lon <- ((tenslond*10)+ unitslond + (tenthslond*0.1)+(hundlond*0.01)+0.005)*gqlon-ra
  }
  return(data.frame(SI_LATI=lat,SI_LONG=lon))}

# ------------------------------------------------------------------------------
# Lon - Lat coordinates to c-squares
# Code obtained from VMStools
CSquare <- function(lon,lat,degrees){
  
  if(length(lon) != length(lat)) stop("length of longitude not equal to length of latitude")
  if(!degrees %in% c(10,5,1,0.5,0.1,0.05,0.01)) stop("degrees specified not in range: c(10,5,1,0.5,0.1,0.05,0.01)")
  
  dims <- length(lon)
  
  quadrants <- array(NA,dim=c(4,6,dims),dimnames=list(c("globalQuadrant","intmQuadrant1","intmQuadrant2","intmQuadrant3"),c("quadrantDigit","latDigit","lonDigit","latRemain","lonRemain","code"),seq(1,dims,1)))
  
  quadrants["globalQuadrant","quadrantDigit",] <- 4-(((2*floor(1+(lon/200)))-1)*((2*floor(1+(lat/200)))+1))
  quadrants["globalQuadrant","latDigit",]      <- floor(abs(lat)/10)
  quadrants["globalQuadrant","lonDigit",]      <- floor(abs(lon)/10)
  quadrants["globalQuadrant","latRemain",]     <- round(abs(lat)-(quadrants["globalQuadrant","latDigit",]*10),7)
  quadrants["globalQuadrant","lonRemain",]     <- round(abs(lon)-(quadrants["globalQuadrant","lonDigit",]*10),7)
  quadrants["globalQuadrant","code",]          <- quadrants["globalQuadrant","quadrantDigit",]*1000+quadrants["globalQuadrant","latDigit",]*100+quadrants["globalQuadrant","lonDigit",]
  
  quadrants["intmQuadrant1","quadrantDigit",]  <- (2*floor(quadrants["globalQuadrant","latRemain",]*0.2))+floor(quadrants["globalQuadrant","lonRemain",]*0.2)+1
  quadrants["intmQuadrant1","latDigit",]       <- floor(quadrants["globalQuadrant","latRemain",])
  quadrants["intmQuadrant1","lonDigit",]       <- floor(quadrants["globalQuadrant","lonRemain",])
  quadrants["intmQuadrant1","latRemain",]      <- round((quadrants["globalQuadrant","latRemain",]-quadrants["intmQuadrant1","latDigit",])*10,7)
  quadrants["intmQuadrant1","lonRemain",]      <- round((quadrants["globalQuadrant","lonRemain",]-quadrants["intmQuadrant1","lonDigit",])*10,7)
  quadrants["intmQuadrant1","code",]           <- quadrants["intmQuadrant1","quadrantDigit",]*100+quadrants["intmQuadrant1","latDigit",]*10+quadrants["intmQuadrant1","lonDigit",]
  
  quadrants["intmQuadrant2","quadrantDigit",]  <- (2*floor(quadrants["intmQuadrant1","latRemain",]*0.2))+floor(quadrants["intmQuadrant1","lonRemain",]*0.2)+1
  quadrants["intmQuadrant2","latDigit",]       <- floor(quadrants["intmQuadrant1","latRemain",])
  quadrants["intmQuadrant2","lonDigit",]       <- floor(quadrants["intmQuadrant1","lonRemain",])
  quadrants["intmQuadrant2","latRemain",]      <- round((quadrants["intmQuadrant1","latRemain",]-quadrants["intmQuadrant2","latDigit",])*10,7)
  quadrants["intmQuadrant2","lonRemain",]      <- round((quadrants["intmQuadrant1","lonRemain",]-quadrants["intmQuadrant2","lonDigit",])*10,7)
  quadrants["intmQuadrant2","code",]           <- quadrants["intmQuadrant2","quadrantDigit",]*100+quadrants["intmQuadrant2","latDigit",]*10+quadrants["intmQuadrant2","lonDigit",]
  
  quadrants["intmQuadrant3","quadrantDigit",]  <- (2*floor(quadrants["intmQuadrant2","latRemain",]*0.2))+floor(quadrants["intmQuadrant2","lonRemain",]*0.2)+1
  quadrants["intmQuadrant3","latDigit",]       <- floor(quadrants["intmQuadrant2","latRemain",])
  quadrants["intmQuadrant3","lonDigit",]       <- floor(quadrants["intmQuadrant2","lonRemain",])
  quadrants["intmQuadrant3","latRemain",]      <- round((quadrants["intmQuadrant2","latRemain",]-quadrants["intmQuadrant3","latDigit",])*10,7)
  quadrants["intmQuadrant3","lonRemain",]      <- round((quadrants["intmQuadrant2","lonRemain",]-quadrants["intmQuadrant3","lonDigit",])*10,7)
  quadrants["intmQuadrant3","code",]           <- quadrants["intmQuadrant3","quadrantDigit",]*100+quadrants["intmQuadrant3","latDigit",]*10+quadrants["intmQuadrant3","lonDigit",]
  
  if(degrees == 10)   CSquareCodes  <- quadrants["globalQuadrant","code",]
  if(degrees == 5)    CSquareCodes  <- paste(quadrants["globalQuadrant","code",],":",quadrants["intmQuadrant1","quadrantDigit",],sep="")
  if(degrees == 1)    CSquareCodes  <- paste(quadrants["globalQuadrant","code",],":",quadrants["intmQuadrant1","code",],sep="")
  if(degrees == 0.5)  CSquareCodes  <- paste(quadrants["globalQuadrant","code",],":",quadrants["intmQuadrant1","code",],":",quadrants["intmQuadrant2","quadrantDigit",],sep="")
  if(degrees == 0.1)  CSquareCodes  <- paste(quadrants["globalQuadrant","code",],":",quadrants["intmQuadrant1","code",],":",quadrants["intmQuadrant2","code",],sep="")
  if(degrees == 0.05) CSquareCodes  <- paste(quadrants["globalQuadrant","code",],":",quadrants["intmQuadrant1","code",],":",quadrants["intmQuadrant2","code",],":",quadrants["intmQuadrant3","quadrantDigit",],sep="")
  if(degrees == 0.01) CSquareCodes  <- paste(quadrants["globalQuadrant","code",],":",quadrants["intmQuadrant1","code",],":",quadrants["intmQuadrant2","code",],":",quadrants["intmQuadrant3","code",],sep="")
  
  return(CSquareCodes)}