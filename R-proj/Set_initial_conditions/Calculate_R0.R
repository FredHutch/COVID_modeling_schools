#########Calculate R0####################


#Author: Mia Moore
#Date: 5/4/2021
#Purpose: From a set of parameters, calculate R0

#Input:
# parameters

#Output:
# R0

#setwd("../R-proj")
library(HutchCOVID)

cwd = getwd()
containing_dir = strsplit(cwd, "COVID_modeling", fixed = T)[[1]][1]
setwd(paste0(containing_dir, "COVID_modeling/R-proj"))
source("covid-model.R")
source("kc_read_data.R")
source("covid-model-plotting.R")


#Shift back to where you started

p.fit = read.csv("Set_initial_conditions/parameter_sets_temp/params_out_firstperiod_rej.csv")
p.fit_newbounds = read.csv("Set_initial_conditions/parameter_sets_temp/params_out_firstperiod_newbounds_rej.csv")
p.fit_hosponly = read.csv("Set_initial_conditions/parameter_sets_temp/params_out_firstperiod_hosponly_rej.csv")
p.fit_noschools = read.csv("Set_initial_conditions/parameter_sets_temp/params_out_firstperiod_noschools_rej.csv")

setwd(cwd)



get_R0 = function(p, sd.input = c(0,0,0,0)){
  p.full = params_base
  p.full$bstar = p['bstar']
  p.full$beta_d = p['beta_d']
  p.full$first_inf_day = p['first_inf_day']
  p.full$sd = sd.input
  p.full$delta_IM = c(0,0,0,0)
  p.full$delta_IS = c(0,0,0,0)
  p.full$delta_H = c(0,0,0,0)
  R_effective(p.full, state)
}


R0.vals = apply(p.fit, 1, get_R0)
R0.vals_newbounds = apply(p.fit_newbounds, 1, get_R0)
R0.vals_hosponly = apply(p.fit_hosponly, 1, get_R0)
R0.vals_noschools = apply(p.fit_noschools, 1, get_R0)

png("R0vals.png")
par(mfrow = c(2, 2))
hist(R0.vals)
hist(R0.vals_newbounds)
hist(R0.vals_hosponly)
hist(R0.vals_noschools)
dev.off()