####Estimate True Infection Numbers####

#Author: Mia Moore
#Date: 4/1/2021
#Purpose: Estimate the true number of infections by week

#Input:
# KINGCOV_Data_ALT1_WEEKLY.Rdata

#Output:
# HFR_estimates.Rdata

library(data.table)
load("../Aggregated KC Data/KINGCOV_DATA_AUG_WEEKLY.Rdata")
load("diagnosis.data.Rdata")
load("optimized.delay.Rdata")
load("Fraction_Severe.Rdata")
load("HFR_estimates.Rdata")

RHO = 1/7
PROPORTION.SYMPTOMATIC = 2/3

KINGCOV_DATA_AUG_WEEKLY$MONTH = month(KINGCOV_DATA_AUG_WEEKLY$STARTDT) + 12 * (year(KINGCOV_DATA_AUG_WEEKLY$STARTDT) - 2020)


aggregate.cases.by.month = aggregate(list("CASES" = KINGCOV_DATA_AUG_WEEKLY$CASES),
                                    list("AGEGR" = KINGCOV_DATA_AUG_WEEKLY$AGEGR,
                                         "MONTH" = KINGCOV_DATA_AUG_WEEKLY$MONTH),
                                    sum)

aggregate.tests.by.month = aggregate(list("NEGATIVE_TESTS" = KINGCOV_DATA_AUG_WEEKLY$NEGATIVE_TESTS),
                                     list("AGEGR" = KINGCOV_DATA_AUG_WEEKLY$AGEGR,
                                          "MONTH" = KINGCOV_DATA_AUG_WEEKLY$MONTH),
                                     sum)
get.asymptomatic.rate = function(i){
  .monthly.rates = subset(diagnostic.rates, AGEGR==i-1 & SEVERITY=="MILD")[,c("MONTH", "diagnostic.rate", "AGEGR")]
  .monthly.rates$diagnostic.asymptomatic = .monthly.rates$diagnostic.rate * optimized.delay[[i]]$optimal.parameter[3]
  .monthly.rates
}

monthly.rates = data.table(Reduce('rbind', lapply(seq(4), get.asymptomatic.rate)))

monthly.rates[,frac.diagnosed.presymptomatic := diagnostic.asymptomatic/(diagnostic.asymptomatic + GAMMA)]
monthly.rates[,frac.diagnosed.asymptomatic := frac.diagnosed.presymptomatic + (1 - frac.diagnosed.presymptomatic) * diagnostic.asymptomatic/(diagnostic.asymptomatic + RHO)]
monthly.rates[,frac.diagnosed.symptomatic := diagnostic.rate/(diagnostic.rate + RHO)]
monthly.rates[,frac.diagnosed.mild := frac.diagnosed.presymptomatic + (1 - frac.diagnosed.presymptomatic) * frac.diagnosed.symptomatic]
monthly.rates[,frac.diagnosed.non.severe := frac.diagnosed.mild * PROPORTION.SYMPTOMATIC + frac.diagnosed.asymptomatic * (1 - PROPORTION.SYMPTOMATIC)]

monthly.rates = merge(monthly.rates, Fraction_Severe)

monthly.rates[,Severe.True := Severe * (PROPORTION.SYMPTOMATIC * frac.diagnosed.mild + (1 - PROPORTION.SYMPTOMATIC) * frac.diagnosed.asymptomatic)/((1 - Severe) + Severe * frac.diagnosed.mild)]

monthly.rates[,frac.diagnosed := PROPORTION.SYMPTOMATIC * (Severe.True + (1 - Severe.True) * frac.diagnosed.mild) + frac.diagnosed.asymptomatic * (1 - PROPORTION.SYMPTOMATIC)]

monthly.rates = merge(monthly.rates, aggregate.cases.by.month)

monthly.rates[,true.cases := CASES/frac.diagnosed]

monthly.rates = merge(monthly.rates, aggregate.tests.by.month)

monthly.rates  = merge(monthly.rates, HFR.estimated)
# kc_pop = 2190000 #2118119                         # King County population
# kc_age_prop = c(0.2293,	0.4552,	0.2350,	0.0805)   # King County age distribution
# 
# 
# estimate.true.infections.data = KINGCOV_DATA_AUG_WEEKLY
# 
# 
monthly.rates[,TOTALPOP := kc_pop * kc_age_prop[monthly.rates$AGEGR + 1]]
# 
# 
# MONTHS_TO_ANALYZE = seq(3, 13)
# 
# estimate.true.infections.data$MONTH = month(estimate.true.infections.data$STARTDT) + 12 * (year(estimate.true.infections.data$STARTDT) - 2020)
# 
# estimate.true.infections.data = subset(estimate.true.infections.data, MONTH %in% MONTHS_TO_ANALYZE)

save("monthly.rates", file = "monthly_rates.Rdata")