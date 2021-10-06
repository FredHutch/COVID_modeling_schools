####Estimate Hospital Fatality Rate####

#Author: Mia Moore
#Date: 3/25/2021
#Purpose: Extract hospital death rates from KC data

#Input:
# KINGCOV_Data_AUG_WEEKLY.Rdata

#Output:
# HFR_estimates.Rdata

load("../Aggregated KC Data/KINGCOV_DATA_AUG_WEEKLY.Rdata")

MONTHS_TO_ANALYZE = seq(3, 13)

hospitalization.data = KINGCOV_DATA_AUG_WEEKLY
# hospitalization.data$DEATHS = c(KINGCOV_DATA_AUG_WEEKLY$HOSPITAL_DEATHS,
#                              KINGCOV_DATA_AUG_WEEKLY$NON_HOSPITAL_DEATHS)
# 
# hospitalization.data$WAITING = c(KINGCOV_DATA_AUG_WEEKLY$WAITING.TO.DIE.HOSPITAL,
#                            KINGCOV_DATA_AUG_WEEKLY$WAITING.TO.DIE.NOT.HOSPITAL)
# hospitalization.data$HOSP_STATUS = c(rep(c("IN", "OUT"), each = nrow(KINGCOV_DATA_AUG_WEEKLY)))
# 
hospitalization.data$MONTH = month(hospitalization.data$STARTDT) + 12 * (year(hospitalization.data$STARTDT) - 2020)
# 
hospitalization.data = subset(hospitalization.data, MONTH %in% MONTHS_TO_ANALYZE)

glm.waiting = glmer(cbind(PRE_DEATH_HOSPITALIZATION, NO_DEATH_HOSPITALIZATION)~as.factor(AGEGR) + (1|MONTH), 
                    hospitalization.data,
                    family = "binomial")

glm.waiting2 = glm(cbind(PRE_DEATH_HOSPITALIZATION, NO_DEATH_HOSPITALIZATION)~as.factor(AGEGR)*as.factor(MONTH), 
                    hospitalization.data,
                    family = "binomial")

prediction.frame = data.frame(
                              MONTH = rep(seq(3, 13),each = 4),
                              AGEGR = rep(seq(0, 3), 11))

predictions = predict(glm.waiting, newdata = prediction.frame)

HFR = exp(predictions)/(1 + exp(predictions))

HFR.estimated = cbind(prediction.frame, HFR)

#save(HFR.estimated, file = "HFR_estimates.Rdata")