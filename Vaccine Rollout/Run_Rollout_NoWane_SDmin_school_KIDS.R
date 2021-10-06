source("../Vaccine Rollout/Run_Rollout_NoWane_Setup.R")

# This runs scenarios B without waning or vaccinating kids 5-12 using the no schools in 2020 calibration
# It adds 3 scenarios for reopening schools september 1 at 50%, 75%, and 100%

# now pick which calibration to use
parameter_file = "out/parameter_sets/params_out_lastperiod_rej.csv"
param_set = read.csv(parameter_file, check.names = FALSE) # this preserves the dash in termporal parameter names
param_names = names(param_set)

# to just run a few parameter sets 
#param_set = param_set[1:3,]

# set the base scenarios to start with sd_school = 0
scenarios[["B"]]$school_sd = 1 # 0% open

# set up the temporal scenario for different reopening
school_0 = list(
  list( date = ymd("2021-09-01"),
        school_sd = 1))
school_50 = list(
  list( date = ymd("2021-09-01"),
        school_sd = 0.5))
school_75 = list(
  list( date = ymd("2021-09-01"),
        school_sd = 0.25))
school_100 = list(
  list( date = ymd("2021-09-01"),
        school_sd = 0))

school_scenarios = list(school_0, school_50, school_75, school_100)
names(school_scenarios) = c("school_0", "school_50", "school_75", "school_100")

sdmins = c(5,15)

print("starting scenarios")

foreach(i=1:length(sdmins)) %dopar%
{
      sdmin_scenario = scenarios[["B"]]
      sdmin_scenario$dynamic_sd_min = c(sdmins[i]/100,sdmins[i]/100,sdmins[i]/100,2*sdmins[i]/100)
      params_school_temp = append(scenarios_temp_kids[["B"]], school_0)
	scen_out = get_model_data_param_sets(param_set, param_names, sdmin_scenario, params_school_temp,
                             state_scenario_base, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                             calc_r_eff = TRUE, out_type = "long", parallel = TRUE) 
	wide_out = lapply(scen_out, function(x) shape_data_wide(x))
	save(scen_out, wide_out, file = paste0("big_out/scenario0_KIDS_NoWane_SD_", sdmins[i], "_30_data.rdata"))
	rm(list = c("scen_out", "wide_out"))
	print(paste("scenario0", sdmins[i]))
}

foreach(i=1:length(sdmins)) %dopar%
{
      sdmin_scenario = scenarios[["B"]]
      sdmin_scenario$dynamic_sd_min = c(sdmins[i]/100,sdmins[i]/100,sdmins[i]/100,2*sdmins[i]/100)
      params_school_temp = append(scenarios_temp_kids[["B"]], school_50)
	scen_out = get_model_data_param_sets(param_set, param_names, sdmin_scenario, params_school_temp,
                             state_scenario_base, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                             calc_r_eff = TRUE, out_type = "long", parallel = TRUE) 
	wide_out = lapply(scen_out, function(x) shape_data_wide(x))
	save(scen_out, wide_out, file = paste0("big_out/scenario50_KIDS_NoWane_SD_", sdmins[i], "_30_data.rdata"))
	rm(list = c("scen_out", "wide_out"))
	print(paste("scenario 50", sdmins[i]))
}

foreach(i=1:length(sdmins)) %dopar%
{
      sdmin_scenario = scenarios[["B"]]
      sdmin_scenario$dynamic_sd_min = c(sdmins[i]/100,sdmins[i]/100,sdmins[i]/100,2*sdmins[i]/100)
      params_school_temp = append(scenarios_temp_kids[["B"]], school_75)
	scen_out = get_model_data_param_sets(param_set, param_names, sdmin_scenario, params_school_temp,
                             state_scenario_base, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                             calc_r_eff = TRUE, out_type = "long", parallel = TRUE) 
	wide_out = lapply(scen_out, function(x) shape_data_wide(x))
	save(scen_out, wide_out, file = paste0("big_out/scenario75_KIDS_NoWane_SD_", sdmins[i], "_30_data.rdata"))
	rm(list = c("scen_out", "wide_out"))
	print(paste("scenario 75", sdmins[i]))
}

foreach(i=1:length(sdmins)) %dopar%
{
      sdmin_scenario = scenarios[["B"]]
      sdmin_scenario$dynamic_sd_min = c(sdmins[i]/100,sdmins[i]/100,sdmins[i]/100,2*sdmins[i]/100)
      params_school_temp = append(scenarios_temp_kids[["B"]], school_100)
	scen_out = get_model_data_param_sets(param_set, param_names, sdmin_scenario, params_school_temp,
                             state_scenario_base, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                             calc_r_eff = TRUE, out_type = "long", parallel = TRUE) 
	wide_out = lapply(scen_out, function(x) shape_data_wide(x))
	save(scen_out, wide_out, file = paste0("big_out/scenario100_KIDS_NoWane_SD_", sdmins[i], "_30_data.rdata"))
	rm(list = c("scen_out", "wide_out"))
	print(paste("scenario 100", sdmins[i]))
}

stopImplicitCluster()

