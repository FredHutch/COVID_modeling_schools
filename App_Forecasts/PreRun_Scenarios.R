cwd = getwd()
containing_dir = strsplit(cwd, "COVID_modeling", fixed = T)[[1]][1]
setwd(paste0(containing_dir, "COVID_modeling/R-proj"))
library(HutchCOVID)
library(lubridate)
library(dplyr)
source("Set_initial_conditions/Starting_State.R")
#source("covid-model-plotting.R")
#source("functions/R_effective.R")

source("../App_Forecasts/functions.R")
load("../data/wa_counties_aggr.Rdata")
load("../data/wa_county_vaccines.Rdata")
load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")

#Shift back to where you started
setwd(cwd)

library(R.utils)
library(purrr)
library(doParallel)
library(foreach)
args = commandArgs(trailingOnly=TRUE, asValues = TRUE, 
                   defaults = c(n_cores = 10,  
                                output_path = "data/scenarios/",
                                scenario_file = "data/scenarios/Wane_16.Rdata",
                                parameter_file = "../R-proj/out/parameter_sets/params_out_firstperiod_noschools_rej.csv"))


load(args$scenario_file)

load("mgmt.Rdata")

param_set = read.csv(args$parameter_file)





param_set$sim = seq(nrow(param_set))



param_set_full = rbind(
  merge(param_set, subset(mgmt_params_internal, management == best_guess_mgmt)),
  merge(subset(param_set, sim == best_guess_param_set), mgmt_params_internal)
)


scen_out = get_model_app_data_param_sets(param_set_full, names(param_set_full), p_base, p_temporal,
                                     state_scenario_init, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                                     n_cores = args$n_cores)

scen_complete = Reduce('rbind', scen_out)

saveRDS(scen_complete,
        file = paste0(args$output_path, scenario_name, ".rds"))

stopImplicitCluster()