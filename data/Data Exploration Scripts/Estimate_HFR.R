####Estimate Hospital Fatality Rate####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Extract hospital death rates from KC data

#Input:
# WA_DATA_AUG_WEEKLY.Rdata

#Output:
# HFR_estimates.Rdata


Estimate_HFR = function(working_folder = "../WA Data Exploration/",
                        MONTHS_TO_ANALYZE = seq(3, 14)
){
  load(paste0(working_folder, "DATA_AGGR_WEEKLY.Rdata"))
  
  MONTHS_TO_ANALYZE = seq(3, 14)
  
  hospitalization.data = DATA_AGGR_WEEKLY
  # hospitalization.data$DEATHS = c(WA_DATA_AUG_WEEKLY$HOSPITAL_DEATHS,
  #                              WA_DATA_AUG_WEEKLY$NON_HOSPITAL_DEATHS)
  # 
  # hospitalization.data$WAITING = c(WA_DATA_AUG_WEEKLY$WAITING.TO.DIE.HOSPITAL,
  #                            WA_DATA_AUG_WEEKLY$WAITING.TO.DIE.NOT.HOSPITAL)
  # hospitalization.data$HOSP_STATUS = c(rep(c("IN", "OUT"), each = nrow(WA_DATA_AUG_WEEKLY)))
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
    MONTH = rep(MONTHS_TO_ANALYZE,each = 4),
    AGEGR = rep(seq(0, 3), length(MONTHS_TO_ANALYZE)))
  
  predictions = predict(glm.waiting, newdata = prediction.frame)
  
  HFR = exp(predictions)/(1 + exp(predictions))
  
  HFR.estimated = cbind(prediction.frame, HFR)
  
  save(HFR.estimated, file = paste0(working_folder, "HFR_estimates.Rdata"))
  
  list(glm.waiting, glm.waiting2, HFR.estimated)
}