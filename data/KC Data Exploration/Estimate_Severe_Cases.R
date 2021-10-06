####Estimate Severe Fraction####

#Author: Mia Moore
#Date: 3/25/2021
#Purpose: Extract severe fraction from KC data

#Input:
# KINGCOV_Data_AUG_WEEKLY.Rdata

#Output:
# HFR_estimates.Rdata

load("../Aggregated KC Data/KINGCOV_DATA_AUG_WEEKLY.Rdata")


MONTHS_TO_ANALYZE = seq(3, 13)

severity.data = KINGCOV_DATA_AUG_WEEKLY

severity.data$MONTH = month(severity.data$STARTDT) + 12 * (year(severity.data$STARTDT) - 2020)
severity.data = subset(severity.data, MONTH %in% MONTHS_TO_ANALYZE)


glm.severe = glmer(cbind(CASES - CASES_NO_DEATH_NO_HOSPITALIZATION, CASES_NO_DEATH_NO_HOSPITALIZATION)~as.factor(AGEGR) + (1|MONTH), 
                   severity.data,
                    family = "binomial")

prediction.frame = data.frame(
  MONTH = rep(seq(3, 13),each = 4),
  AGEGR = rep(seq(0, 3), 11))

predictions = predict(glm.severe, newdata = prediction.frame)

Severe = exp(predictions)/(1 + exp(predictions))

Fraction_Severe = cbind(prediction.frame, Severe)

save(Fraction_Severe, file = "Fraction_Severe.Rdata")