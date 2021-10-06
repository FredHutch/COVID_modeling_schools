####Estimate Diagnosis Rate####

#Author: Mia Moore
#Date: 3/25/2021
#Purpose: Extract diagnostic rates from KC data

#Input:
# KINGCOV_Data_ALT1_WEEKLY.Rdata

#Output:
# diagnosis.data.Rdata


library(lme4)
library(lubridate)
MONTHS_TO_ANALYZE = seq(3, 13) #Only look March 2020 to Jan 2021
load("../Aggregated KC Data/KINGCOV_DATA_ALT1_WEEKLY.Rdata")
diagnosis.data = rbind(KINGCOV_DATA_ALT1_WEEKLY,
                       KINGCOV_DATA_ALT1_WEEKLY,
                       KINGCOV_DATA_ALT1_WEEKLY)


diagnosis.data$DIAGNOSED = c(KINGCOV_DATA_ALT1_WEEKLY$MILD_SYMPTOMATIC_DIAGNOSIS,
                             KINGCOV_DATA_ALT1_WEEKLY$SEVERE_SYMPTOMATIC_DIAGNOSIS_EARLY,
                             KINGCOV_DATA_ALT1_WEEKLY$SEVERE_SYMPTOMATIC_DIAGNOSIS_LATE)

diagnosis.data$WAITING = c(KINGCOV_DATA_ALT1_WEEKLY$WAITING.TO.BE.DIAGNOSED.MILD,
                           KINGCOV_DATA_ALT1_WEEKLY$WAITING.TO.BE.DIAGNOSED.EARLY,
                           KINGCOV_DATA_ALT1_WEEKLY$WAITING.TO.BE.DIAGNOSED.LATE)

diagnosis.data$SEVERITY = c(rep(c("MILD", "SEVERE_EARLY", "SEVERE_LATE"), each = nrow(KINGCOV_DATA_ALT1_WEEKLY)))

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


prediction.frame = data.frame(SEVERITY = rep(c("MILD", "SEVERE_EARLY", "SEVERE_LATE"), 11, each = 4),
                              MONTH = rep(seq(3, 13),each = 4 * 3),
                              AGEGR = rep(seq(0, 3), 11*3))
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
save(diagnosis.data, diagnostic.rates, file = "diagnosis.data.Rdata")



