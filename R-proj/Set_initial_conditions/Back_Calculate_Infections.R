####Back Calculate Infections####

#Author: Mia Moore
#Date: 4/29/2021
#Purpose: Define functions for back calculating number of infections

#Input:
# None

#Output:
# None:

library(Matrix)
library(nnls)
load("../data/WA Data Exploration/optimized.delay.Rdata")
load("../data/Sero_prevalence_comparison/SERO_MCMC.Rdata")



GAMMA = 0.6
samples.to.use = 1000:5000

expit = function(x){exp(x)/(1 + exp(x))}
get.multipliers = function(p){
  if(is.null(p)){
  p = get.parameters.infection()
  return(get.HIR(p))
  }
  1/((1 - p$m) * p$p) 
}

get.parameters.infection = function(){
  i = sample(samples.to.use, 1)
  out.MCMC$samples[i, ]
}

convert.parameters.infection = function(){
  x = get.parameters.infection()
  list(m = expit(x[3:6]), p = expit(-x[7:10]), HIR = get.HIR(x))
}

get.HIR = function(p){

  return((1 + exp(p[7:10])) * (1 + exp(p[3:6])))
  
}

get.delay = function(optimized.delay, p){
  if(is.null(p)){
    h = rnorm(1, mean = optimized.delay$optimal.parameter['a2'], sd = optimized.delay$standard.errors['a2'])
    return((1/h + 1/GAMMA)/7)
  }
  
  1/p$h + 1/params_base$gamma_1 + 1/params_base$gamma_2
}

fillin.data.hosp = function(data){
  helper.df = expand.grid(WEEK = unique(data$WEEK), AGEGR = unique(data$AGEGR), COUNTY = unique(data$COUNTY), RACE = unique(data$RACE))
  data = merge(data, helper.df, all = T)
  data$STARTDT = as.Date("2019-12-31") + 7 * data$WEEK - 6
  data$ENDDT = as.Date("2019-12-31") + 7 * data$WEEK
  data$HOSPITALIZATIONS[is.na(data$HOSPITALIZATIONS)] = 0
  data$NON_HOSPITAL_DEATHS[is.na(data$NON_HOSPITAL_DEATHS)] = 0
  data
}
Compute.Infections = function(sim.number, weighting, data.to.use = DATA_AGGR_WEEKLY, Prep.Data = F, Resample = F, Normalization = NA, parameters = NULL){
  
  data.to.use = fillin.data.hosp(data.to.use)
  delay = sapply(optimized.delay.hosp, get.delay, parameters)
  HIR = get.multipliers(parameters)
  X = Reduce('rbind', lapply(seq(4), BackFill.Infections, HIR, delay, weighting, data.to.use, Resample, Normalization))
  
  if(Prep.Data){
    return(Prep.Reff.Data(X))
  }
  
  X$sim = sim.number
  X
}


Starting.Configurations = function(end.date, data.to.use = DATA_AGGR_WEEKLY, Prep.Data = F, Resample = F, Normalization = NA){
  
  
  delay = sapply(optimized.delay.hosp, get.delay)
  
  infection.parameters = convert.parameters.infection()
  HIR = infection.parameters$HIR
  X = Reduce('rbind', lapply(seq(4), BackFill.Infections, HIR, delay, weighting, data.to.use, Resample, Normalization))
  
  if(Prep.Data){
    return(Prep.Reff.Data(X))
  }
  infection.parameters$starting.configuration = get.end.date(X, end.date)
  infection.parameters
}

#Construct the matrix A given arbitrary parameters
BackFill.Infections = function(i,
                                       HIR, delay, weighting, data, Resample, Normalization
){
  cols.to.keep = c("WEEK", "AGEGR", "HOSPITALIZATIONS", "NON_HOSPITAL_DEATHS", "STARTDT", "ENDDT")
  x = subset(data, AGEGR == i - 1)[, cols.to.keep]
  y = (x$HOSPITALIZATIONS + x$NON_HOSPITAL_DEATHS)
  N.weeks = length(y)
  

  if(Resample){
    y = rpois(N.weeks, y)
  }
  
  if(!is.na(Normalization)[1]){
    y = y/Normalization[i]
  }
  A = t(sapply(seq(N.weeks), get.time.of.infection, N.weeks, 1/delay[i]))/HIR[i]
  
  gamma = get.diff.matrix(N.weeks) * weighting/HIR[i]
  B = (t(A)%*%A + t(gamma)%*%gamma)
  out.nls = nnls(B, t(A)%*%y)
  
  
  x$INFECTIONS = out.nls$x
  x$CUM.INFECTIONS = cumsum(x$INFECTIONS)
  x$Y.CONSTRUCT = A%*%out.nls$x
  x$Y = y
  x
}

get.end.date = function(x, end.date){
  subset(x, as.numeric(ENDDT - end.date)%in%seq(-6,0))[, c("AGEGR","INFECTIONS", "CUM.INFECTIONS", "ENDDT")]
}
get.diff.matrix = function(N.times, start.date = 1){
  
  subdiagonal = c(rep(1, N.times-2), 0)
  central = c(rep(0, start.date), c(rep(-1, N.times - start.date - 1), 0))
  
  as.matrix(bandSparse(N.times, N.times, #dimensions
                       -1:0, #band, diagonal is number 0
                       list(subdiagonal, 
                            central)))
}

#Provides individual columns of the matrix A
get.time.of.infection = function(start.time, N.times, rate){
  prob.infection.vector = numeric(N.times)
  times = seq(N.times)
  Y0 = (1 - exp(-rate))/rate
  ifelse(start.time<times,
         0,
         ifelse(start.time>times,
         Y0 * (exp(-rate * (start.time - times - 1)) - exp(-rate * (start.time - times))),
         1 - Y0))
}

Prepare.Reff.Data = function(X){
  X = plyr::ddply(X, .variables = "AGEGR", .fun = Prev.Infections.Data)
  
  X0 = subset(X, AGEGR==0)[,c("WEEK", "INFECTIONS.PREV")]
  X1 = subset(X, AGEGR==1)[,c("WEEK", "INFECTIONS.PREV")]
  X2 = subset(X, AGEGR==2)[,c("WEEK", "INFECTIONS.PREV")]
  X3 = subset(X, AGEGR==3)[,c("WEEK", "INFECTIONS.PREV")]
  
  X = X[, c("WEEK", "AGEGR", "INFECTIONS", "Y.CONSTRUCT", "Y")]
  
  names(X0) = c("WEEK", "INFECTIONS.PREV0")
  names(X1) = c("WEEK", "INFECTIONS.PREV1")
  names(X2) = c("WEEK", "INFECTIONS.PREV2")
  names(X3) = c("WEEK", "INFECTIONS.PREV3")
  
  Reduce(
    function(x, y, ...) merge(x, y, all = TRUE, ...),
    list(X, X0, X1, X2, X3)
  )
}

Prev.Infections.Data = function(X){
  X$INFECTIONS.PREV = c(NA, X$INFECTIONS[-nrow(X)])
  X
}



######## Cross validation ######

#In order to set the parameter w = omega = weighting (regularization parameter), we use cross validation to select the most
#predictive model.

#Subdivide data, stratified by country, into K-folds
cross.validation.setup = function(fold = 5, dat.covid = DATA_AGGR_WEEKLY){
  cols.to.keep = c("WEEK", "AGEGR", "HOSPITALIZATIONS", "NON_HOSPITAL_DEATHS", "STARTDT", "ENDDT", "fold")
  AGEGR.list = unique(dat.covid$AGEGR)
  N.AGEGR = length(AGEGR.list)
  
  
  #By default put data in fold 0 (not used) to account for countries with an uninformative amount of data
  dat.covid$fold = 0
  for(i in seq(N.AGEGR)){
    
    #Find all data entries that correspond to a given country
    index = which(dat.covid$AGEGR == AGEGR.list[i])
    #COunt the number of observations
    observations = length(index)
    
    #If there are sufficient entries to have at least one element in each fold, randomly assign each day to a fold
    if(observations>=fold){
      fold.assignments = sample(seq(observations)%%fold + 1) #Ensures that each fold is evenly split (as close as possible) in each country
      dat.covid$fold[index]=fold.assignments
    }
  }
  dat.covid[, cols.to.keep]
}

#For a given value of w, generate predictions for each y using the data from the other folds. Then calculate the sum of squares
cross.validation = function(weighting, dat.covid){
  
  dat.covid$target.value = dat.covid$HOSPITALIZATIONS + dat.covid$NON_HOSPITAL_DEATHS
  #Initialize residual
  residual = 0
  
  #Number of folds
  folds = max(dat.covid$fold)
  
  #Cycle over fold
  for(i in seq(folds)){
    
    #Create temporary data frame
    .dat = dat.covid
    
    #At test spots replace data with NA, so it will have to be predicted
    target.index = .dat$fold==i
    .dat$target.value[target.index] = NA
    
    #Perform all calculation on partial data set
    X = Compute.Infections(data.to.use = .dat, weighting = weighting)
    
    X = merge(X, dat.covid[,c("WEEK","AGEGR","target.value", "fold")])
    #For each age group compare true number of hospitalizations with reconstructed number of hospitalizations
    get.residual = function(x){
      reconstructed.target.j = x$Y.CONSTRUCT
      true.target.j = x$target.value
      residual.j = (true.target.j - reconstructed.target.j)/sd(true.target.j) #Compare true and reconstructed values
      
      compare.index = x$fold==i #Find index of values to be compared
      
      sum(residual.j[compare.index]^2, na.rm = T) #Add residual sum of squares to running total
    }
    
    residual = residual + sum(daply(X, .variables = "AGEGR", .fun = get.residual))
  }
  residual
}

