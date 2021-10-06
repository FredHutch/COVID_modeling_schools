####Shrink Data####

#Author: Mia Moore
#Date: 3/18/2021
#Purpose: Load the Washington Line List data and compress to only king county and needed fields

#Output:
# KINGCOV_Data.Rdata: Reduced Line list data for King County
# REQD_Data.Rdata: Reduced Line list data for Washington State

ALL_Data = read.csv("../../../DOHdata/IDM_linelist_EXPANDED_AsOf_2021-06-20_exported_2021-06-20/IDM_linelist_EXPANDED_AsOf_2021-06-20_exported_2021-06-20.csv", 
                    na.strings = "")



fields.reqd = c("County", "DEATH_DATE", "AGE_YEARS", "FIRST_NEGATIVE_SPECIMEN__COLLECTION__DTTM", "FIRST_POSITIVE_SPECIMEN__COLLECTION__DTTM", "SYMPTOM_ONSET_DATE", "admitdate", "LabTestResult", "Race")

REQD_Data = ALL_Data[,fields.reqd]
REQD_Data$CASE_DATE = REQD_Data$FIRST_POSITIVE_SPECIMEN__COLLECTION__DTTM
REQD_Data$NEG_TEST_DATE = ifelse(REQD_Data$LabTestResult=="Negative",
                             REQD_Data$FIRST_NEGATIVE_SPECIMEN__COLLECTION__DTTM,
                             as.Date(NA))

save(REQD_Data, file = "../../../DOHdata/REQD_Data.Rdata")

