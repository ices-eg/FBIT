## calculate impact from continuous longevity
RBS <- function(Fd,a,b){        #  a = slope of binomial model, b = intercept of binomial model, Fd = fishing SAR x depletion rate (gear specific)
  
  # 3 equations
  step.size=0.5
  longevity=seq(1,200,by=step.size)
  H <- 5.31
  
  r = H/longevity
  K = (a*exp(a * log(longevity)+b))/(longevity * (exp(a*log(longevity) + b) + 1)^2)     #1st derivative of logistic
  B = K*(1 - Fd / r); B[B<0]=0
  
  RBS=sum(B)*step.size
  RBS
  
}