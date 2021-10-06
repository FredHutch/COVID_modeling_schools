####Aggregate WA Data####

#Author: Mia Moore
#Date: 4/11/2021
#Purpose: Aggregate Line List Data into a format usable for fitting

#Output:
# REQD_AGGR_Data.Rdata: Aggregated data for King County by date and age group
# REQD_AGGR_BASIC.Rdata: Aggregated data for King County containing only hospitalizations, deaths, cases


Aggregation_Function = function(variable_name, data_name, data){
  .x = aggregate(list(blank = data[,data_name]), 
            by = list(DATE = data[,data_name],
                      COUNTY = data$County,
                      AGEGR = data$AGEGR,
                      RACE = data$Race), 
            length)
  names(.x) = c("DATE", "COUNTY", "AGEGR", "RACE", variable_name)
  .x
}
Aggregate_Data = function(outputpath = "../WA Data Exploration/",
                          AGEGR.CUTOFFS = c(20, 50, 70),
                          columns.needed = "ALL",
                          AGG.RACE = T,
                          AGG.COUNTY = F,
                          return.delay = T,
                          weekly.only = T,
                          counties.included = "ALL"){
  
load("../../../DOHdata/REQD_Data.Rdata")

  group.by = c("RACE", "AGEGR", "COUNTY")
DATE_MIN = as.Date("2020-01-01")

date.list = seq(DATE_MIN, Sys.Date(), by = 1)

assign.to.agegr = function(age, cutoffs){
  agegr = 0 * age
  
  for(i in seq_along(cutoffs)){
    agegr = ifelse(age>cutoffs[i], i, agegr)
  }
  agegr
}

if(counties.included[1]!="ALL"){
  REQD_Data = subset(REQD_Data, County %in% counties.included)
}

if(AGG.RACE){
  REQD_Data$Race = "All"
}

if(AGG.COUNTY){
  REQD_Data$County = "All"
}
REQD_Data$AGEGR = assign.to.agegr(REQD_Data$AGE_YEARS, AGEGR.CUTOFFS)



if(return.delay){
  REQD_Data$DELAY = as.numeric(as.Date(REQD_Data$CASE_DATE) - as.Date(REQD_Data$SYMPTOM_ONSET_DATE))
  REQD_Data$DELAY_HOSP = as.numeric(pmin(as.Date(REQD_Data$admitdate), as.Date(REQD_Data$DEATH_DATE), na.rm = T) - as.Date(REQD_Data$SYMPTOM_ONSET_DATE))
  REQD_Data$DELAY_DEATH = as.numeric(as.Date(REQD_Data$DEATH_DATE) - as.Date(REQD_Data$admitdate))
  REQD_MILD = subset(REQD_Data, is.na(admitdate) & is.na(DEATH_DATE))
  delay.by.age = aggregate(list("DELAY" = REQD_MILD$DELAY),
                           by = list(AGEGR = REQD_MILD$AGEGR),
                           table)
  
  delay.by.age.hosp = aggregate(list("DELAY_HOSP" = REQD_Data$DELAY_HOSP),
                                by = list(AGEGR = REQD_Data$AGEGR),
                                table)
  
  delay.by.age.death = aggregate(list("DELAY_DEATH" = REQD_Data$DELAY_DEATH),
                                 by = list(AGEGR = REQD_Data$AGEGR),
                                 table)
  
  save(delay.by.age, delay.by.age.death, delay.by.age.hosp, file = paste0(outputpath, "delay.by.age.Rdata"))
}




#Aggregate Deaths
deaths.by.date = Aggregation_Function("DEATHS", "DEATH_DATE", REQD_Data)


#Aggregate Hospitalizations
hospitalizations.by.date = Aggregation_Function("HOSPITALIZATIONS", "admitdate", REQD_Data)

#Aggregate Negative Tests
negative_tests.by.date = Aggregation_Function("NEGATIVE_TESTS", "NEG_TEST_DATE", REQD_Data)

#Aggregate Cases
cases.by.date = Aggregation_Function("CASES", "CASE_DATE", REQD_Data)

#Aggregate pre-symptomatic
REQD_Data_PreSymptoms = subset(REQD_Data, CASE_DATE<SYMPTOM_ONSET_DATE)
pre_symptoms.by.date = Aggregation_Function("PRE_SYMPTOMATIC_DIAGNOSIS", "CASE_DATE", REQD_Data_PreSymptoms)

symptoms_post_diagnosis.by.date = Aggregation_Function("SYMPTOMS_POST_DIAGNOSIS", "SYMPTOM_ONSET_DATE", REQD_Data_PreSymptoms)

#Aggregate Mild Symptom Onset
REQD_Data_Symptom_Mild = subset(REQD_Data, CASE_DATE>=SYMPTOM_ONSET_DATE & is.na(admitdate) &is.na(DEATH_DATE) & SYMPTOM_ONSET_DATE>=DATE_MIN)
mild_symptoms.by.date = Aggregation_Function("MILD_SYMPTOMS_PRE_DIAGNOSIS", "SYMPTOM_ONSET_DATE", REQD_Data_Symptom_Mild)

#Aggregate Mild Symptomatic Diagnosis
mild_symptomatic_cases.by.date = Aggregation_Function("MILD_SYMPTOMATIC_DIAGNOSIS", "CASE_DATE", REQD_Data_Symptom_Mild)

#Aggregate Severe Symptom Onset
REQD_Data_Symptom_Severe = subset(REQD_Data, (!is.na(admitdate)|!is.na(DEATH_DATE)) & CASE_DATE>=SYMPTOM_ONSET_DATE & SYMPTOM_ONSET_DATE>=DATE_MIN)
REQD_Data_Symptom_Severe$SEVERE_DATE = pmin(REQD_Data_Symptom_Severe$admitdate, REQD_Data_Symptom_Severe$DEATH_DATE, na.rm = T)
REQD_Data_Symptom_Severe_Early = subset(REQD_Data_Symptom_Severe, SEVERE_DATE>CASE_DATE)
REQD_Data_Symptom_Severe_Late = subset(REQD_Data_Symptom_Severe, SEVERE_DATE<=CASE_DATE)
severe_symptoms.by.date = Aggregation_Function("SEVERE_SYMPTOMS_PRE_DIAGNOSIS", "SYMPTOM_ONSET_DATE", REQD_Data_Symptom_Severe)

#Aggregate Severe Symptomatic Diagnosis
severe_symptomatic_cases.by.date = Aggregation_Function("SEVERE_SYMPTOMATIC_DIAGNOSIS", "CASE_DATE", REQD_Data_Symptom_Severe)

#Aggregate Severe Early Symptomatic Diagnosis
severe_symptoms_early.by.date = Aggregation_Function("SEVERE_SYMPTOMS_PRE_DIAGNOSIS_EARLY", "SYMPTOM_ONSET_DATE", REQD_Data_Symptom_Severe_Early)


severe_symptomatic_cases_early.by.date =  Aggregation_Function("SEVERE_SYMPTOMATIC_DIAGNOSIS_EARLY", "CASE_DATE", REQD_Data_Symptom_Severe_Early)



#Aggregate Severe Late Symptomatic Diagnosis
severe_symptoms_late.by.date = Aggregation_Function("SEVERE_SYMPTOMS_PRE_DIAGNOSIS_LATE", "SEVERE_DATE", REQD_Data_Symptom_Severe_Late)



severe_symptomatic_cases_late.by.date = Aggregation_Function("SEVERE_SYMPTOMATIC_DIAGNOSIS_LATE", "CASE_DATE", REQD_Data_Symptom_Severe_Late)


#Aggregate Death in hospital
REQD_Data_DeathHospital = subset(REQD_Data, DEATH_DATE>admitdate & admitdate>=DATE_MIN)
hospital_death.by.date = Aggregation_Function("HOSPITAL_DEATHS", "DEATH_DATE", REQD_Data_DeathHospital)

#Aggregate Hospitalizations that occurred prior to death
hospital_pre_death.by.date = Aggregation_Function("PRE_DEATH_HOSPITALIZATION", "admitdate", REQD_Data_DeathHospital)

#Aggregate Death not in hospital
REQD_Data_DeathNonHospital = subset(REQD_Data, !is.na(DEATH_DATE) & is.na(admitdate) & DEATH_DATE>=DATE_MIN)
nonhospital_death.by.date = Aggregation_Function("NON_HOSPITAL_DEATHS", "DEATH_DATE", REQD_Data_DeathNonHospital)


#Aggregate Diagnosis prior to death not in hospital
nonhospital_predeath.by.date = Aggregation_Function("NON_HOSPITAL_PREDEATH_DIAGNOSIS", "CASE_DATE", REQD_Data_DeathNonHospital)


#Aggregate hospitalization with no death
REQD_Data_HospitalAlive = subset(REQD_Data, is.na(DEATH_DATE) & !is.na(admitdate) & admitdate>=DATE_MIN)
hospital_no_death.by.date =  Aggregation_Function("NO_DEATH_HOSPITALIZATION", "admitdate", REQD_Data_HospitalAlive)


#Aggregate diagnoses with no hospitalizations
REQD_Data_NoHospitalization = subset(REQD_Data, !is.na(CASE_DATE) & is.na(DEATH_DATE) & is.na(admitdate) & CASE_DATE>=DATE_MIN)
no_hospital_no_death.by.date = Aggregation_Function("CASES_NO_DEATH_NO_HOSPITALIZATION", "CASE_DATE", REQD_Data_NoHospitalization)

dataList = list(deaths.by.date,
                cases.by.date,
                hospitalizations.by.date,
                negative_tests.by.date,
                hospital_death.by.date,
                nonhospital_death.by.date,
                nonhospital_predeath.by.date,
                pre_symptoms.by.date,
                symptoms_post_diagnosis.by.date,
                mild_symptomatic_cases.by.date,
                mild_symptoms.by.date,
                severe_symptomatic_cases.by.date,
                severe_symptoms.by.date,
                severe_symptoms_early.by.date,
                severe_symptoms_late.by.date,
                severe_symptomatic_cases_early.by.date,
                severe_symptomatic_cases_late.by.date,
                hospital_pre_death.by.date,
                hospital_no_death.by.date,
                no_hospital_no_death.by.date)


# For Full (Outer) Join
REQD_DATA_AGGR <- Reduce(
  function(x, y, ...) merge(x, y, all = TRUE, ...),
  dataList
)

REQD_DATA_AGGR$DATE = as.Date(REQD_DATA_AGGR$DATE)

Data.template = merge(merge(data.frame(DATE=date.list), 
                      data.frame(AGEGR=seq(0, length(AGEGR.CUTOFFS))),
                      all =  T), data.frame(RACE = unique(REQD_DATA_AGGR$RACE)), all = T)

REQD_DATA_AGGR = merge(REQD_DATA_AGGR, Data.template, all.y = T, all.x = F)
REQD_DATA_AGGR[is.na(REQD_DATA_AGGR)]<-0


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

DATA_AGGR = merge(plyr::ddply(REQD_DATA_AGGR, group.by, .fun = get.waiting), REQD_DATA_AGGR)




DATA_AGGR$WEEK = ceiling(as.numeric(DATA_AGGR$DATE - as.Date("2019-12-31"))/7)#pmin(week(DATA_AGGR$DATE), 52) + 52 * (year(DATA_AGGR$DATE) - 2020)

if(columns.needed[1]!="ALL"){
  DATA_AGGR = DATA_AGGR[,union(columns.needed, c("WEEK", "DATE", group.by))]
}


weekly.count = function(x){
  .n = setdiff(names(x), c("WEEK", "DATE", group.by))
  .x = as.data.frame(t(colSums(x[,.n])))
  .x$STARTDT = min(x$DATE)
  .x$ENDDT = max(x$DATE)
  .x
}

DATA_AGGR_WEEKLY = plyr::ddply(DATA_AGGR, c("WEEK", group.by), .fun = weekly.count)

#write.csv(DATA_AGGR_WEEKLY, file = paste0(outputpath, "DATA_AGGR_WEEKLY.csv"))
save(DATA_AGGR_WEEKLY, file = paste0(outputpath, "DATA_AGGR_WEEKLY.Rdata"))

if(!weekly.only){
#write.csv(DATA_AGGR, file = paste0(outputpath, "DATA_AGGR.csv"))
save(DATA_AGGR, file = paste0(outputpath, "DATA_AGGR.Rdata"))
}



}