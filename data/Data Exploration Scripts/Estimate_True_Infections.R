####Estimate True Infection Numbers####

#Author: Mia Moore
#Date: 4/11/12021
#Purpose: Estimate the true number of infections by week

#Input:
# WA_Data_ALT1_WEEKLY.Rdata

#Output:
# monthyl_rates.Rdata

library(data.table)

working_folder = "../WA Data Exploration/"
load(paste0(working_folder, "DATA_AGGR_WEEKLY.Rdata"))
load(paste0(working_folder, "diagnosis.data.Rdata"))
load(paste0(working_folder, "optimized.delay.Rdata"))
load(paste0(working_folder, "Fraction_Severe.Rdata"))
load(paste0(working_folder, "HFR_estimates.Rdata"))

RHO = 1/5
PROPORTION.SYMPTOMATIC = c(.25,.3,.5,.7)


WA_DATA_AUG_WEEKLY$MONTH = month(WA_DATA_AUG_WEEKLY$STARTDT) + 12 * (year(WA_DATA_AUG_WEEKLY$STARTDT) - 2020)


aggregate.cases.by.month = aggregate(list("CASES" = WA_DATA_AUG_WEEKLY$CASES),
                                    list("AGEGR" = WA_DATA_AUG_WEEKLY$AGEGR,
                                         "MONTH" = WA_DATA_AUG_WEEKLY$MONTH),
                                    sum)

aggregate.tests.by.month = aggregate(list("NEGATIVE_TESTS" = WA_DATA_AUG_WEEKLY$NEGATIVE_TESTS),
                                     list("AGEGR" = WA_DATA_AUG_WEEKLY$AGEGR,
                                          "MONTH" = WA_DATA_AUG_WEEKLY$MONTH),
                                     sum)
get.asymptomatic.rate = function(i){
  .monthly.rates = subset(diagnostic.rates, AGEGR==i-1 & SEVERITY=="MILD")[,c("MONTH", "diagnostic.rate", "AGEGR")]
  .monthly.rates$diagnostic.asymptomatic = .monthly.rates$diagnostic.rate * optimized.delay[[i]]$optimal.parameter[3]
  .monthly.rates
}

monthly.rates = data.table(Reduce('rbind', lapply(seq(4), get.asymptomatic.rate)))

monthly.rates$symptomatic.proportion = PROPORTION.SYMPTOMATIC[monthly.rates$AGEGR + 1]
monthly.rates[,frac.diagnosed.presymptomatic := diagnostic.asymptomatic/(diagnostic.asymptomatic + GAMMA)]
monthly.rates[,frac.diagnosed.asymptomatic := frac.diagnosed.presymptomatic + (1 - frac.diagnosed.presymptomatic) * diagnostic.asymptomatic/(diagnostic.asymptomatic + RHO)]
monthly.rates[,frac.diagnosed.symptomatic := diagnostic.rate/(diagnostic.rate + RHO)]
monthly.rates[,frac.diagnosed.mild := frac.diagnosed.presymptomatic + (1 - frac.diagnosed.presymptomatic) * frac.diagnosed.symptomatic]
monthly.rates[,frac.diagnosed.non.severe := frac.diagnosed.mild * symptomatic.proportion + frac.diagnosed.asymptomatic * (1 - symptomatic.proportion)]

monthly.rates = merge(monthly.rates, Fraction_Severe)

monthly.rates[,Severe.True := Severe * (symptomatic.proportion * frac.diagnosed.mild + (1 - symptomatic.proportion) * frac.diagnosed.asymptomatic)/((1 - Severe) + Severe * frac.diagnosed.mild)]

monthly.rates[,frac.diagnosed := symptomatic.proportion * (Severe.True + (1 - Severe.True) * frac.diagnosed.mild) + frac.diagnosed.asymptomatic * (1 - symptomatic.proportion)]

monthly.rates = merge(monthly.rates, aggregate.cases.by.month)

monthly.rates[,true.cases := CASES/frac.diagnosed]

monthly.rates = merge(monthly.rates, aggregate.tests.by.month)

monthly.rates  = merge(monthly.rates, HFR.estimated)

# WA_pop = 8000000
# kc_pop = 2190000 #2118119                         # King County population
# kc_age_prop = c(0.2293,	0.4552,	0.2350,	0.0805)   # King County age distribution
# 
# 
# estimate.true.infections.data = WA_DATA_AUG_WEEKLY
# 
# 
# monthly.rates[,TOTALPOP := WA_pop * kc_age_prop[monthly.rates$AGEGR + 1]]
# 
# 
# MONTHS_TO_ANALYZE = seq(3, 13)
# 
# estimate.true.infections.data$MONTH = month(estimate.true.infections.data$STARTDT) + 12 * (year(estimate.true.infections.data$STARTDT) - 2020)
# 
# estimate.true.infections.data = subset(estimate.true.infections.data, MONTH %in% MONTHS_TO_ANALYZE)

save("monthly.rates", file = "monthly_rates.Rdata")