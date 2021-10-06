##Run Projection
# Creation date: 6/23/2021
# Purpose: This function simulates version 2

#setwd("R-proj")
cwd = getwd()
containing_dir = strsplit(cwd, "COVID_modeling", fixed = T)[[1]][1]
setwd(paste0(containing_dir, "COVID_modeling/R-proj"))
library(HutchCOVID)
library(lubridate)
source("kc_globalparams.R")
source("Set_initial_conditions/Starting_State.R")
load("../data/wa_counties_aggr.Rdata")
load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")
load("../data/wa_counties_age1_proportions.Rdata")
load("../data/wa_county_vaccines.Rdata")
setwd(cwd)
### Vaccine and Strain Assumptions #####
# define the baseline parameters


Run_Projection = function(scenario.specs){
  
  with(as.list(scenario.specs),{
    
    starting_conditions = get_starting_conditions_by_county_date(
      County = County,
      Start_Date = "2021-06-01"
    )
    
    
    params_base$vac_rate = vaccination.parameters$rate.per.day
    params_base$vac_dist = vaccination.parameters$distribution.by.age
    params_base$vac_priority = 4:1
    params_base$vac_coverage = vaccination.parameters$max.coverage
    
    
    params_base$dynamic_sd = TRUE
    params_base$dynamic_sd_init_date = ymd("2021-06-11")
    params_base$dynamic_sd_func = dynamic.sd.parameters$dynamic.func
    params_base$dynamic_sd_lock_thresh = dynamic.sd.parameters$lock.threshold  * sum(starting_conditions$total_population)
    params_base$dynamic_sd_release_thresh = dynamic.sd.parameters$release.threshold  * sum(starting_conditions$total_population)
    params_base$dynamic_sd_period = dynamic.sd.parameters$period
    params_base$dynamic_sd_min = dynamic.sd.parameters$sd.min
    params_base$dynamic_sd_max = dynamic.sd.parameters$sd.max
    params_base$ramp_sd = FALSE # don't need to ramp since we're starting in 2021 with set sd value and just dynamic
    
    
    scenario.params = get_parameters_from_specs(scenario.specs, params_base, params_temporal_base)
    
    params_base = scenario.params$params
    scenario_temporal = scenario.params$params_temporal
    
    initial_conditions = get_starting_state_from_scenario(params_base, 
                                                          starting_conditions, 
                                                          scenario.specs)
    
    state_scenario_base = state_matrix_to_vector(initial_conditions)
    
    # this just runs against the uncalibrated parameters to check
    
    
    # now make parameter sets for the CSTE scenarios
    
    
    
    out_new = run_model_by_date(params_base, scenario_temporal, state_scenario_base, start_date = ymd("2021-06-01"), end_date = ymd("2022-04-01"), calc_r_eff = T)
    shape_data_long(out_new, as.Date("2020-01-01"))
    
  })
}