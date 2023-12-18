## calculate impact from continuous longevity


RBS <- function(Fd,a,b,H){        #  a  = slope of binomial model, 
                                #  b  = intercept of binomial model, 
                                #  Fd = fishing SAR x depletion rate (gear specific)
                                #  H  = used to translate longevity to recovery (r = H/longevity)
  # 3 equations
  step.size=.5
  longevity=seq(.5,200,by=step.size)
  
  r = H/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  B = K*(1 - Fd / r); B[B<0]=0
  
  RBS=sum(B)*step.size
  RBS
  
}

RBS_sens <- function(Fd,a,b,H){        #  a  = slope of binomial model, 
  #  b  = intercept of binomial model, 
  #  Fd = fishing SAR x depletion rate (gear specific)
  #  H  = used to translate longevity to recovery (r = H/longevity)
  # 3 equations
  step.size=.5
  longevity=seq(.5,200,by=step.size)
  
  r = H/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  idx <- c(which(abs(cumsum(K)/sum(K) - 0.9) == min(abs(cumsum(K)/sum(K) - 0.9))):length(K))
  K_sen <- K[idx]/sum(K[idx]*step.size)
  B = K_sen*(1 - Fd / r[idx]); B[B<0]=0
  
  RBS_sens=sum(B)*step.size
  RBS_sens
  
}



