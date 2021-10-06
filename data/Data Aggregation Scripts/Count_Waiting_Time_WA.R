####Count Waiting Time Washington####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Use aggregated data to count waiting time for alternative variables

#Input:
# REQD_DATA.Rdata

#Output:
# REQD_Data_ALT1.Rdata
# REQD_Data_ALT2.Rdata

library(plyr)
library(lubridate)
load("REQD_DATA_AGGR.Rdata")

REQD_DATA_ALT1.var = c("DATE",
                          "WEEK",
                          "AGEGR",
                          "DEATHS", 
                          "HOSPITALIZATIONS",
                          "NEGATIVE_TESTS",
                          "MILD_SYMPTOMATIC_DIAGNOSIS",
                          "SEVERE_SYMPTOMATIC_DIAGNOSIS",
                          "SEVERE_SYMPTOMATIC_DIAGNOSIS_EARLY",
                          "SEVERE_SYMPTOMATIC_DIAGNOSIS_LATE",
                          "WAITING.TO.BE.DIAGNOSED.MILD",
                          "WAITING.TO.BE.DIAGNOSED.EARLY",
                          "WAITING.TO.BE.DIAGNOSED.LATE",
                          "WAITING.TO.BE.DIAGNOSED.SEVERE")

REQD_DATA_ALT2.var = c("DATE",
                          "WEEK",
                          "AGEGR",
                          "NEGATIVE_TESTS",
                          "MILD_SYMPTOMATIC_DIAGNOSIS",
                          "WAITING.TO.BE.DIAGNOSED.MILD",
                          "WAITING.TO.DIE.HOSPITAL",
                          "WAITING.TO.DIE.NOT.HOSPITAL",
                          "HOSPITAL_DEATHS",
                          "NON_HOSPITAL_DEATHS",
                          "HOSPITALIZATIONS")

REQD_DATA_BASIC.var = c("DATE",
                           "WEEK",
                           "AGEGR",
                           "CASES",
                           "DEATHS",
                        "HOSPITAL_DEATHS",
                        "NON_HOSPITAL_DEATHS",
                           "HOSPITALIZATIONS")



get.waiting = function(X){
  data.frame(DATE = X$DATE,
             WAITING.TO.BE.DIAGNOSED.MILD = cumsum(X$MILD_SYMPTOMS_PRE_DIAGNOSIS) - cumsum(X$MILD_SYMPTOMATIC_DIAGNOSIS),
             WAITING.TO.BE.DIAGNOSED.SEVERE = cumsum(X$SEVERE_SYMPTOMS_PRE_DIAGNOSIS) - cumsum(X$SEVERE_SYMPTOMATIC_DIAGNOSIS),
             WAITING.TO.BE.DIAGNOSED.EARLY = cumsum(X$SEVERE_SYMPTOMS_PRE_DIAGNOSIS_EARLY) - cumsum(X$SEVERE_SYMPTOMATIC_DIAGNOSIS_EARLY),
             WAITING.TO.BE.DIAGNOSED.LATE = cumsum(X$SEVERE_SYMPTOMS_PRE_DIAGNOSIS_LATE) - cumsum(X$SEVERE_SYMPTOMATIC_DIAGNOSIS_LATE),
             WAITING.TO.DIE.HOSPITAL = cumsum(X$PRE_DEATH_HOSPITALIZATION) - cumsum(X$HOSPITAL_DEATHS),
             WAITING.TO.DIE.NOT.HOSPITAL = cumsum(X$NON_HOSPITAL_PREDEATH_DIAGNOSIS) - cumsum(X$NON_HOSPITAL_DEATHS),
             WAITING.FOR.HOSPITALIZATION = cumsum(X$CASES_NO_DEATH_NO_HOSPITALIZATION)
  )
}

REQD_DATA_AUG = merge(ddply(REQD_DATA_AGGR, "AGEGR", .fun = get.waiting), REQD_DATA_AGGR)

REQD_DATA_AUG$WEEK = pmin(week(REQD_DATA_AUG$DATE), 52) + 52 * (year(REQD_DATA_AUG$DATE) - 2020)


REQD_DATA_BASIC = REQD_DATA_AUG[, REQD_DATA_BASIC.var]
REQD_DATA_ALT1 = REQD_DATA_AUG[, REQD_DATA_ALT1.var]
REQD_DATA_ALT2 = REQD_DATA_AUG[, REQD_DATA_ALT2.var]

weekly.count = function(x){
  .n = setdiff(names(x), c("WEEK", "AGEGR", "DATE"))
  .x = as.data.frame(t(colSums(x[,.n])))
  .x$STARTDT = min(x$DATE)
  .x$ENDDT = max(x$DATE)
  .x
}


write.csv(REQD_DATA_ALT1, file = "REQD_DATA_ALT1.csv")
write.csv(REQD_DATA_ALT2, file = "REQD_DATA_ALT2.csv")
write.csv(REQD_DATA_BASIC, file = "REQD_DATA_BASIC.csv")

save(REQD_DATA_ALT1, file = "REQD_DATA_ALT1.Rdata")
save(REQD_DATA_ALT2, file = "REQD_DATA_ALT2.Rdata")
save(REQD_DATA_BASIC, file = "REQD_DATA_BASIC.Rdata")


REQD_DATA_BASIC_WEEKLY = ddply(REQD_DATA_BASIC, c("WEEK", "AGEGR"), .fun = weekly.count)
REQD_DATA_ALT1_WEEKLY = ddply(REQD_DATA_ALT1, c("WEEK", "AGEGR"), .fun = weekly.count)
REQD_DATA_ALT2_WEEKLY = ddply(REQD_DATA_ALT2, c("WEEK", "AGEGR"), .fun = weekly.count)

#write.csv(REQD_DATA_ALT1_WEEKLY, file = "REQD_DATA_ALT1_WEEKLY.csv")
#write.csv(REQD_DATA_ALT2_WEEKLY, file = "REQD_DATA_ALT2_WEEKLY.csv")
#write.csv(REQD_DATA_BASIC_WEEKLY, file = "REQD_DATA_BASIC_WEEKLY.csv")

#save(REQD_DATA_ALT1_WEEKLY, file = "REQD_DATA_ALT1_WEEKLY.Rdata")
#save(REQD_DATA_ALT2_WEEKLY, file = "REQD_DATA_ALT2_WEEKLY.Rdata")
#save(REQD_DATA_BASIC_WEEKLY, file = "REQD_DATA_BASIC_WEEKLY.Rdata")

WA_DATA_AUG_WEEKLY = ddply(REQD_DATA_AUG, c("WEEK", "AGEGR"), .fun = weekly.count)


write.csv(WA_DATA_AUG_WEEKLY, file = "WA_DATA_AUG_WEEKLY.csv")
save(WA_DATA_AUG_WEEKLY, file = "WA_DATA_AUG_WEEKLY.Rdata")