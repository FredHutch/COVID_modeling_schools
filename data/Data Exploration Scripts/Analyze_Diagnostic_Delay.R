####Analyze Diagnostic Delay####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Analyze delay between symptoms and diagnosis

#Input:
# delay.by.date.Rdata

#Output:
# optimized.delay.Rdata

Analyze_Diagnostic_Delay = function(working_folder = "../WA Data Exploration/",
                                 MONTHS_TO_ANALYZE = seq(3, 14)
){
  
  load(paste0(working_folder, "delay.by.age.Rdata"))

#Assume fixed value for gamma for now


#Convert table output to dataframe
convert.to.data.frame = function(x){
  d = as.numeric(names(x))
  
  d.all = seq(min(d), max(d))
  
  n.all = numeric(length(d.all))
  
  n.all[which(d.all%in%d)] = x
  
  data.frame(DELAY = d.all,
             NUMBER = n.all)
}

delay.data = lapply(delay.by.age$DELAY, convert.to.data.frame)
delay.data.hosp = lapply(delay.by.age.hosp$DELAY, convert.to.data.frame)
delay.data.death = lapply(delay.by.age.death$DELAY, convert.to.data.frame)


#Define the bi-exponential curve
delay.function = function(k, p){
  with(as.list(p),{
    
    #a1 = GAMMA + DELTAA
    #a2 = DELTA + RHO
    #f1 = DELTAA/(GAMMA + DELTAA)
    #f2 = (1 - f1) * DELTA/a2
    
    #prop1 = f1/(f1 + f2)
    
    prop1 = RATIO * a2/(RATIO * a2 + GAMMA)
    prop2 = 1 - prop1
    
    pre = prop1 * (exp(a1 * k) * (exp(a1) - 2 + exp(-a1)) / a1)
    post = prop2 * (exp(-a2 * k) * (exp(a2) - 2 + exp(-a2)) / a2)
    
    simul = prop1 * (1 - 1/a1 + exp(-a1)/a1) + prop2 * (1 - 1/a2 + exp(-a2)/a2)
    
    ifelse(k>0, 
           TOTAL * post,
           ifelse(k<0, 
                  TOTAL * pre,
                  TOTAL * simul))
  })
}

#Guess at parameters (optimization appears to be robust to these choices :-))
p.guess = c(a1 = 1/2, a2 = 1/7, RATIO = 0.33)


#Wrapper function for optim (computes residuals + objective function)
delay.residual = function(p, n.p, delay.data,...){
  names(p) = n.p
  output = delay.values(p, delay.data,...)
  sum(-dpois(delay.data$NUMBER, lambda = output, log = T))
}


#Another helper function for optim (computes necessary model values)
delay.values = function(p, delay.data, onesided = F){
  if(onesided){
    p['a2'] = p
    p['a1'] = 1
    p['RATIO'] = 0
  }
  p['TOTAL'] = sum(delay.data$NUMBER)
  delay.function(delay.data$DELAY, p)
}

#Wrapper function to run optim
optimize.delay.by.agegroup = function(delay.data.to.use, 
                                      days.to.use = seq(-7, 21),
                                      onesided = F){
  
  #Subset to allow -1week to +3 weeks (because after that we'll have to worry about zeros)
  delay.data.to.use = subset(delay.data.to.use, DELAY%in%days.to.use)
  out.optim = NULL
  if(onesided){
    
    p.guess = c(a2 = 1/7)
    out.optim = optim(p.guess,
                      lower = c(a2 = 0),
                      upper = c(a2 = 1),
                      delay.residual, 
                      n.p = names(p.guess), 
                      delay.data = subset(delay.data.to.use, DELAY%in%days.to.use),
                      onesided = T,
                      method = "Brent",
                      hessian = T)
  } else{
      out.optim = optim(p.guess, 
                        delay.residual, 
                        n.p = names(p.guess), 
                        delay.data = subset(delay.data.to.use, DELAY%in%days.to.use),
                        hessian = T)
  }


  
  #Record model output at best fit parameters
  delay.data.to.use$fitted.value = delay.values(out.optim$par, delay.data.to.use, onesided = onesided)
  
  #Record standard errors (from Fisher Information Matrix)
  standard.errors = sqrt(diag(solve(out.optim$hessian)))
  
  #Return results
  list(optimal.parameter = out.optim$par,
       standard.errors = standard.errors,
       data = delay.data.to.use)
}

#Run optim for each age-group separately
optimized.delay = lapply(delay.data, optimize.delay.by.agegroup)
optimized.delay.hosp = lapply(delay.data.hosp, optimize.delay.by.agegroup, days.to.use = seq(-7, 30))
optimized.delay.death = lapply(delay.data.death, optimize.delay.by.agegroup, days.to.use = seq(0, 30), onesided = T)
save(optimized.delay, optimized.delay.hosp, optimized.delay.death, file = paste0(working_folder, "optimized.delay.Rdata"))

}
