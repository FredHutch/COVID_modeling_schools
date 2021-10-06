cwd = getwd()
containing_dir = strsplit(cwd, "COVID_modeling", fixed = T)[[1]][1]
setwd(paste0(containing_dir, "COVID_modeling/R-proj"))
source("Set_initial_conditions/Starting_State.R")
#source("kc_read_data.R")
library(HutchCOVID)
library(lubridate)
source("kc_globalparams.R")
source("../App_Forecasts/functions.R")
load("../data/wa_counties_aggr.Rdata")
load("../data/wa_county_vaccines.Rdata")
load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")

#Shift back to where you started
setwd(cwd)

library(R.utils)
args = commandArgs(trailingOnly=TRUE, asValues = TRUE, 
                   defaults = c(input_path = "scenario_definition/",
				scenario_folder = "data/scenarios/",
                                output_path = "data/")
)



source(paste0(args$input_path, "Define_Scenario.R"))
source(paste0(args$input_path, "Define_Management.R"))

lapply(scenario.list, save.scenario, args$scenario_folder)


nmgmt = nrow(mgmt_params)
mgmt_params_internal$management = seq(nmgmt)
names(mgmt_params_internal) = names.management

save(best_guess_mgmt,
     best_guess_param_set,
     mgmt_params_internal,
     file = "mgmt.Rdata")


mgmt_params$management = seq(nmgmt)
mgmt_params$mgmt_fac = as.factor(seq(nmgmt))

settings.labels = c(
        "No waning, delta 20% more transmissable than alpha",
        "No waning, delta 60% more transmissable than alpha",
        "Two-year waning, delta 20% more transmissable than alpha",
        "Two-year waning, delta 60% more transmissable than alpha"
        )

save(mgmt_params,
     kc.vax,
     kc.cover,
     kc.cmax,
     kc.cmin,
     kc.sdmin,
     best_guess_mgmt,
     extremes_only,
     mgmt_extremes,
     kc.pop,
     kc_age_prop,
     calib.date,
     vax.date,
     settings.labels
     file = paste0(args$output_path, "app_data.Rdata"))