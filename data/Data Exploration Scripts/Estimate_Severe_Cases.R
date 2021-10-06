####Estimate Severe Fraction####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Extract severe fraction from KC data

#Input:
# REQD_Data_AUG_WEEKLY.Rdata

#Output:
# HFR_estimates.Rdata

library(lme4)

Estimate_Severe_Cases = function(working_folder = "../WA Data Exploration/",
                                    MONTHS_TO_ANALYZE = seq(3, 14)
){

  load(paste0(working_folder, "DATA_AGGR_WEEKLY.Rdata"))
  
severity.data = DATA_AGGR_WEEKLY

severity.data$MONTH = month(severity.data$STARTDT) + 12 * (year(severity.data$STARTDT) - 2020)
severity.data = subset(severity.data, MONTH %in% MONTHS_TO_ANALYZE)

glm.severe = glmer(cbind(CASES - CASES_NO_DEATH_NO_HOSPITALIZATION, CASES_NO_DEATH_NO_HOSPITALIZATION)~as.factor(AGEGR) + (1|MONTH), 
                   severity.data,
                    family = "binomial")

prediction.frame = data.frame(
  MONTH = rep(MONTHS_TO_ANALYZE,each = 4),
  AGEGR = rep(seq(0, 3), length(MONTHS_TO_ANALYZE)))

predictions = predict(glm.severe, newdata = prediction.frame)

Severe = exp(predictions)/(1 + exp(predictions))

Fraction_Severe = cbind(prediction.frame, Severe)
save(Fraction_Severe, file = paste0(working_folder, "Fraction_Severe.Rdata"))

list(glm.severe, Fraction_Severe)
}

#