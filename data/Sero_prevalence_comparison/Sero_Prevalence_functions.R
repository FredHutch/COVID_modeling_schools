####Sero-Prevalence Functions####

#Author: Mia Moore
#Date: 4/11/12021
#Purpose: Define functions for projecting sero-prevalence

#Input:
# None

#Output:
# None:

###################################################################################
#Projection functions (in order called)

#Project.sero (top-level)
#parameters
#1: Decay rate (1/week) of sero-positivity rate following symptomatic infection
#2: Decay rate (1/week) of sero-positivity rate following asymptomatic infection
#3-6: Number of symptomatic infections per hospitalization (or non-hospital death) by age
#7-10: Number of asymptomatic infections per symptomatic infections by age
Project.sero = function(params){
  
  #Construct parameters by age
  p.0 = c(params[1], params[3], params[2], (1 + params[3]) * params[7])
  p.1 = c(params[1], params[4], params[2], (1 + params[4]) * params[8])
  p.2 = c(params[1], params[5], params[2], (1 + params[5]) * params[9])
  p.3 = c(params[1], params[6], params[2], (1 + params[6]) * params[10])
  
  #Call underlying function by age
  out.0 = get.sero.prevalence.by.month(p.0, subset(sero.data.WA, AGEGR==0))
  out.1 = get.sero.prevalence.by.month(p.1, subset(sero.data.WA, AGEGR==1))
  out.2 = get.sero.prevalence.by.month(p.2, subset(sero.data.WA, AGEGR==2))
  out.3 = get.sero.prevalence.by.month(p.3, subset(sero.data.WA, AGEGR==3))
  
  rbind(out.0, out.1, out.2, out.3)
}

#get.sero.prevalence.by.month

#input
#p: decay model parametets
#1: Decay rate (1/week) of sero-positivity rate following symptomatic infection
#2: Decay rate (1/week) of sero-positivity rate following asymptomatic infection
#3: Number of symptomatic infections per hospitalization (or non-hospital death) 
#4: Number of asymptomatic infections per symptomatic infections

#x: The sero-prevalence data to compare to
get.sero.prevalence.by.month = function(p, x){
  ddply(x,
        .var = c("AGEGR", "week.start"),
        .fun = get.sero.prevalence.single,
        p = p,
        y = DATA_AGGR_WEEKLY)
}

#get.sero.prevalence.single
get.sero.prevalence.single = function(x, p, y){
  y = subset(y, AGEGR == x$AGEGR[1])
  
  seropositive = sapply(y$WEEK, decay.f.1, x$week.start[1], x$week.end[1], p)
  
  #replace decay rate with zero to track the "true cumulative incidence"
  in.past = sapply(y$WEEK, decay.f.1, x$week.start[1], x$week.end[1], c(0, p[2],0, p[4]))
  
  
  still.positive = 100 * seropositive * (y$HOSPITALIZATIONS + y$NON_HOSPITAL_DEATHS)/y$TOTALPOP
  cumulative.incidence = 100 * in.past * (y$HOSPITALIZATIONS + y$NON_HOSPITAL_DEATHS)/y$TOTALPOP
  data.frame(SEROPOSITIVE = sum(still.positive),
             CUMINC = sum(cumulative.incidence),
             SERO.LOWER = x$sero.lower,
             SERO.UPPER = x$sero.upper)
}

decay.f.1 = function(w, start, end, p){
  diff = seq(start - w, end - w)
  mean(ifelse(diff>=0, 
              exp(-p[1] * diff) * (1 + p[2]) + exp(-p[3] * diff) * p[4],
              0))
}

####################################################################################################
#Likelihood calculation functions

LL.sero = function(lparams){
  
  #Exponentiate to get to positive domain
  params = exp(lparams)
  
  #Compute likelihood of decay rates from prior
  loglik1 = sum(get.sero.prior(params[1], params[2]))
  
  #Project seroprevalence rates and compute the residual
  out = Project.sero(params)
  loglik2 = -0.5 * get.residual(out) #Note that the likelihood is one half of SSE (by definition)
  
  #Compute likelihood of symptomatic rates from prior
  loglik3 = get.severe.lik(params[seq(3, 6)])
  
  #Compute likelihood of symptomatic rates from prior
  loglik4 = get.symptomatic.lik(params[seq(7, 10)])
  
  loglik1 + loglik2 + loglik3 + loglik4
}

get.residual = function(sero.df){
  se = (sero.df$SERO.UPPER - sero.df$SERO.LOWER)/(2 * qnorm(.975))
  mean = (sero.df$SERO.UPPER + sero.df$SERO.LOWER)/2
  
  sum((sero.df$SEROPOSITIVE - mean)^2/se^2, na.rm = T)
}

######################################################################################################

#Sampler
SERO.samp = function(i){
  
  params = numeric(10)
  
  #Sample Sero Prior (i.e. decay rates)
  params[1:2] = sample.sero.prior()
  
  #Sample symptomatic rate
  params[3:6] = get.symptomatic.samp()                 
  
  #Fix hospitalization rate (for symptomatic)
  params[7:10] = get.severe.samp()
  
  Project.sero(params)
  
}

######################################################################################################
#Priors

#Serological parameters

# From Long, Nature Medicine


get.sero.prior = function(y1, y2){
  x = c(y1, y2)
  out = dbeta(exp(-x * 8), 
              shape1 = c(rep(27, length(y1)), rep(18, length(y2))), 
              shape2 = c(rep(4, length(y1)), rep(12, length(y2))), 
              log = T)
 matrix(out, ncol = 2)

}
sample.sero.prior = function(n = 1){
  out = -log(rbeta(2*n, 
             shape1 = rep(c(27, 18), each = n), 
             shape2 = rep(c(4, 12), each = n)
             ))/8
  
  matrix(out, ncol = 2)
}

get.sero.prior.transform = function(y1, y2){
  .tmp = get.sero.prior(exp(y1), exp(y2))
  x1 = exp(-8 * exp(y1))
  x2 = exp(-8 * exp(y2))
  .tmp[,1] = -exp(.tmp[,1]) * x1 * log(x1)
  .tmp[,2] = -exp(.tmp[,2]) * x2 * log(x2)
  .tmp
}

#Symptomatic rate parameters

#From Davies et.al Table

#Likelihood
#x = Odds (asymptomatic:symptomatic ratio)
get.symptomatic.lik = function(x){
  prior = get.symptomatic.prior()
  with(prior,{
    sum(dnorm(-log(x), mean.p, sd = se.p, log = T))
  })
}

#Hard coding of values from Davies
#Note that these values correspond to percent symptomatic
get.symptomatic.prior = function(){
  quant.50 = c(0.25, 0.33, 0.55, 0.70)
  quant.75 = c(0.29, 0.37, .60, .74)
  
  list(mean.p = logit(quant.50),
  se.p = (logit(quant.75) - logit(quant.50))/qnorm(.75))
}

#Sample from prior
get.symptomatic.samp = function(){
  prior = get.symptomatic.prior()
  with(prior,{
    exp(rnorm(length(mean.p), -mean.p, sd = se.p))
  })
}

#Severity priors
#Based on Data Exploration
#x: mild:severe ratio
get.severe.lik = function(x){
  prior = get.severe.prior()
  with(prior,{
    sum(dnorm(log(x), -mean.p, sd = se.p, log = T))
  })
}

#Priors from data exploration
#Note that these priors correspond to percent severe among symptomatic
get.severe.prior = function(){
  quant.50 = c(0.004, 0.012, 0.05, 0.2) #By age group
  quant.975 = c(0.01, 0.02, .08, .3) #By age group
  
  list(mean.p = logit(quant.50),
       se.p = (logit(quant.975) - logit(quant.50))/qnorm(.975))
}

get.severe.samp = function(){
  prior = get.severe.prior()
  with(prior,{
    exp(rnorm(length(mean.p), -mean.p, sd = se.p))
  })
}
expit = function(x){
  exp(x)/(1 + exp(x))
}

logit = function(x){
  log(x/(1 - x))
}


#SERO.samples = ldply(seq(100), SERO.samp, c(.004, .012, .05, .20))
