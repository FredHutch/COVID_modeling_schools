####Estimate Diagnosis Rate####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Extract diagnostic rates from KC data

#Input:
# DATA_AGGR_WEEKLY.Rdata

#Output:
# diagnosis.data.Rdata


library(lme4)
library(lubridate)

Estimate_Diagnostic_Rate = function(working_folder = "../WA Data Exploration/",
                        MONTHS_TO_ANALYZE = seq(3, 14)
){
load(paste0(working_folder, "DATA_AGGR_WEEKLY.Rdata"))
diagnosis.data = rbind(DATA_AGGR_WEEKLY,
                       DATA_AGGR_WEEKLY,
                       DATA_AGGR_WEEKLY)


diagnosis.data$DIAGNOSED = c(DATA_AGGR_WEEKLY$MILD_SYMPTOMATIC_DIAGNOSIS,
                             DATA_AGGR_WEEKLY$SEVERE_SYMPTOMATIC_DIAGNOSIS_EARLY,
                             DATA_AGGR_WEEKLY$SEVERE_SYMPTOMATIC_DIAGNOSIS_LATE)

diagnosis.data$WAITING = c(DATA_AGGR_WEEKLY$WAITING.TO.BE.DIAGNOSED.MILD,
                           DATA_AGGR_WEEKLY$WAITING.TO.BE.DIAGNOSED.EARLY,
                           DATA_AGGR_WEEKLY$WAITING.TO.BE.DIAGNOSED.LATE)

diagnosis.data$SEVERITY = c(rep(c("MILD", "SEVERE_EARLY", "SEVERE_LATE"), each = nrow(DATA_AGGR_WEEKLY)))

diagnosis.data$MONTH = month(diagnosis.data$STARTDT) + 12 * (year(diagnosis.data$STARTDT) - 2020)

diagnosis.data = subset(diagnosis.data, MONTH %in% MONTHS_TO_ANALYZE)
glm.waiting = glmer(cbind(DIAGNOSED, WAITING)~1 + (1|AGEGR) + (1|SEVERITY) + (1|MONTH), 
                    diagnosis.data,
                    family = "binomial")

diagnosis.data$SEVERITY2 = diagnosis.data$SEVERITY == "SEVERE_LATE"
glm.waiting2 = glmer(cbind(DIAGNOSED, WAITING)~1 + (1|AGEGR) + (1|SEVERITY2) + (1|MONTH), 
                     diagnosis.data,
                     family = "binomial")


diagnosis.data$SEVERITY3 = diagnosis.data$SEVERITY == "MILD"
glm.waiting3 = glmer(cbind(DIAGNOSED, WAITING)~1 + (1|AGEGR) + (1|SEVERITY3) + (1|MONTH), 
                     diagnosis.data,
                     family = "binomial")



glm.waiting4 = glmer(cbind(DIAGNOSED, WAITING)~1 + (SEVERITY|AGEGR) + (1|MONTH), 
                     diagnosis.data,
                     family = "binomial")

glm.waiting5 = glmer(cbind(DIAGNOSED, WAITING)~1 + (0 + SEVERITY|AGEGR) + (0 + SEVERITY|MONTH), 
                     diagnosis.data,
                     family = "binomial")






prediction.logit = predict(glm.waiting5, newdata = diagnosis.data)

diagnosis.data$DELTA = log(exp(prediction.logit) + 1)


prediction.frame = data.frame(SEVERITY = rep(c("MILD", "SEVERE_EARLY", "SEVERE_LATE"), length(MONTHS_TO_ANALYZE), each = 4),
                              MONTH = rep(MONTHS_TO_ANALYZE,each = 4 * 3),
                              AGEGR = rep(seq(0, 3), length(MONTHS_TO_ANALYZE)*3))
extract.parameters = function(model.result){
  log(exp(predict(model.result, newdata = prediction.frame)) + 1)
}


#diagnosis.boot = bootMer(glm.waiting5, extract.parameters, nsim = 10)

# diagnosis.rate = cbind(prediction.frame, 
#                        t(apply(diagnosis.boot$t, 
#                                2, 
#                                quantile, 
#                                probs = c(0.025, 0.5, 0.975)
#                                )
#                          )
#                        )


diagnostic.rate = extract.parameters(glm.waiting5)
diagnostic.rates = cbind(prediction.frame,
                        diagnostic.rate)
save(diagnosis.data, diagnostic.rates, file = paste0(working_folder, "diagnosis.data.Rdata"))

list(glm.waiting5, diagnosis.data, diagnostic.rates)
}



