####Refresh WA Data####

#Author: Mia Moore
#Date: 4/12/2021
#Purpose: Aggregate Line List Data into a format usable for fitting. Outputs multiple data sets

#Output:
# See Aggregate_WA_data.R

#source("Shrink_Data.R")
#source("Aggregate_WA_data.R")


#Model Calibration Data
Aggregate_Data(weekly.only = F, 
               return.delay = F,
               counties.included = "King County",
               AGG.RACE = T,
               outputpath = "../KC_Data/",
               columns.needed = c("DATE", 
                                  "WEEK",
                                  "AGEGR",
                                  "RACE",
                                  "DEATHS",
                                  "CASES",
                                  "HOSPITALIZATIONS")
               )


#Hospitalization Data
Aggregate_Data(weekly.only = F,
               return.delay = F,
               AGG.RACE = T,
               outputpath = "../Hosp_Data/",
               columns.needed = c("DATE", 
                                  "WEEK",
                                  "AGEGR",
                                  "HOSPITALIZATIONS",
                                  "NON_HOSPITAL_DEATHS")
)


#Washington Exploration
Aggregate_Data(AGG.COUNTY = T)



##KC Exploration
#Aggregate_Data(counties.included = "King County",
#               outputpath = "../KC Data Exploration/")


#Sero-prevalence comparison
Aggregate_Data(AGEGR.CUTOFFS = c(18, 50, 65), 
               weekly.only = T,
               outputpath = "../Sero_prevalence_comparison/",
               columns.needed = c("DATE", 
                                  "WEEK",
                                  "AGEGR",
                                  "HOSPITALIZATIONS",
                                  "NON_HOSPITAL_DEATHS"),
               return.delay = F)



