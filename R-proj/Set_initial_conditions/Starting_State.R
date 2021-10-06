####Calculate Starting State####

#Author: Mia Moore
#Date: 4/7/2021
#Purpose: Define a function to calculate the complete starting state

#Input:
# parameters
# prevalence
# cumulative incidence

#Output:
# Starting State


target_dir = "../data/Hosp_Data/"
age.names = c("AGEGR1", "AGEGR2", "AGEGR3", "AGEGR4")
load(paste0(target_dir, "DATA_AGGR_WEEKLY.Rdata"))
load("../data/wa_counties_aggr.Rdata")



assemble.starting.conditions = function(current.conditions, County, Start_Date){
  #Population pulled from census data
  total_population = wa.counties.pop[[County]]
  names(total_population) = paste0("a", 1:4)
  
  #Cumulative incidence crudely estimated from hospitalizations
  cumulative_incidence = current.conditions$CUM.INCIDENCE
  names(cumulative_incidence) = paste0("a", 1:4)
  
  #Number of vaccines
  number_vaccinated = subset(wa_county_vaccines[[County]], 
                             date == Start_Date, 
                             select = c("AGEGR1", "AGEGR2", "AGEGR3", "AGEGR4"))
  
  if(nrow(number_vaccinated) == 0){
    number_vaccinated = rep(0, 4)
  }
  
  proportion_vaccinated = as.numeric(number_vaccinated/total_population)
  names(proportion_vaccinated) = paste0("a", 1:4)
  
  #Estimate total prevalence here (this is very crude currently, will fix)
  total_prevalence = sum(current.conditions$PREVALENCE * total_population)/sum(total_population)
  
  list(total_prevalence = total_prevalence,
       total_population = total_population,
       proportion_vaccinated = proportion_vaccinated,
       cumulative_incidence = cumulative_incidence)
}
generate_starting_conditions_by_county_weeks = function(County,weeks){
  out.cum.inc = get.cumulative.incidence(County)
  names(weeks) = weeks
  .f = function(week){
    Start_Date = as.Date("2020-01-01") + 7 * week
    current.conditions = subset(out.cum.inc, WEEK == week)
    assemble.starting.conditions(current.conditions, County, Start_Date)
  }
  lapply(weeks, .f)
  
}


generate_starting_conditions_by_county_date = function(County,Start_Date, N = 1, Resample = F, parameters = NULL){
  out.cum.inc = get.cumulative.incidence(County, resamples = N, Resample = Resample, parameters = parameters)
  current.conditions = subset(out.cum.inc, as.numeric(ENDDT - as.Date(Start_Date)) %in% seq(-6, 0))
  
  if(N == 1){
    return(assemble.starting.conditions(current.conditions, County, Start_Date))
  }
  current.conditions %>%group_by(sim) %>% group_map(~assemble.starting.conditions(.x, "King County", as.Date("2021-06-01")))
}



get.cumulative.incidence = function(county, resamples = 1, Resample = F, parameters = NULL){
  
  weighting = 1
  
  data.infections.full = Reduce('rbind', 
                                lapply(seq(resamples),
                                       Compute.Infections,
                                       weighting,
                                       Resample = Resample, 
                                       Normalization = wa.counties.pop[[county]],
                                       data.to.use = subset(DATA_AGGR_WEEKLY, COUNTY == county),
                                       parameters = parameters))
  
  data.infections.full$AGEGR = age.names[data.infections.full$AGEGR + 1]
  data.infections.full$PREVALENCE = data.infections.full$INFECTIONS
  data.infections.full$CUM.INCIDENCE = data.infections.full$CUM.INFECTIONS
  subset(data.infections.full, select = c("WEEK", "ENDDT", "AGEGR","PREVALENCE", "CUM.INCIDENCE", "sim"))
}

##################################Deprecated#########################################
get_starting_conditions_by_county_date = function(County,Start_Date){
  current.conditions = subset(wa.current[[County]], as.numeric(ENDDT - as.Date(Start_Date)) %in% seq(-6, 0))
  assemble.starting.conditions(current.conditions, County, Start_Date)
}