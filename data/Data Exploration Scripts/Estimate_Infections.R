target_dir = "../Hosp_Data/"
resamples = 1
weighting = 2.5
week = c(65, 74)
age.names = c("AGEGR1", "AGEGR2", "AGEGR3", "AGEGR4")
load(paste0(target_dir, "DATA_AGGR_WEEKLY.Rdata"))
load("../wa_counties_aggr.Rdata")
get.cumulative.incidence = function(county){

#source("Assess_Reff.R")
#col.list = c("indianred", "skyblue", "seagreen", "goldenrod")
data.infections.full = Reduce('rbind', 
                              lapply(rep(weighting, resamples), 
                                     Compute.Infections, 
                                     Resample = F, 
                                     Normalization = wa.counties.pop[[county]],
                                     data.to.use = subset(DATA_AGGR_WEEKLY, COUNTY == county)))

data.infections.full$AGEGR = age.names[data.infections.full$AGEGR + 1]

data.infections.full$PREVALENCE = data.infections.full$INFECTIONS

data.infections.full$CUM.INCIDENCE = data.infections.full$CUM.INFECTIONS

subset(data.infections.full, WEEK %in% week, select = c("WEEK", "ENDDT", "AGEGR","PREVALENCE", "CUM.INCIDENCE"))
}

wa.current = lapply(names(wa.counties.pop), get.cumulative.incidence)

names(wa.current) = names(wa.counties.pop)

save(wa.current, file = paste0(target_dir, "CUMULATIVE_INCIDENCE.Rdata"))