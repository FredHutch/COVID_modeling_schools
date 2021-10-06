source("../Vaccine Rollout/Run_Rollout_Setup.R")

# this runs the 4 basic scenarios A B C D for vaccinating kids or not

# now pick which calibration to use
parameter_file = "out/parameter_sets/params_out_firstperiod_rej.csv"
param_set = read.csv(parameter_file, check.names = FALSE) # this preserves the dash in termporal parameter names
param_names = names(param_set)

# to just run a few parameter sets including the median
#param_set = param_set[1:3,]
#param_set[5,] = apply(param_set, 2, median)


### Run the scenarios

print("starting scenarios")

foreach(i=1:length(scenarios)) %dopar%
{
	scen_out = get_model_data_param_sets(param_set, param_names, scenarios[[i]], scenarios_temp[[i]],
                             state_scenario_base, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                             calc_r_eff = TRUE, out_type = "long", parallel = TRUE) 
	wide_out = lapply(scen_out, function(x) shape_data_wide(x))
	save(scen_out, wide_out, file = paste0("out/scenario_", names(scenarios)[i], "_data.rdata"))
	rm(list = c("scen_out", "wide_out"))
	print(paste("scenario", names(scenarios)[i]))
}

foreach(i=1:length(scenarios)) %dopar%
{ 
	scen_out = get_model_data_param_sets(param_set, param_names, scenarios[[i]], scenarios_temp_kids[[i]],
                             state_scenario_base, NULL, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"),
                             calc_r_eff = TRUE, out_type = "long", parallel = TRUE)  
	wide_out = lapply(scen_out, function(x) shape_data_wide(x))  
	save(scen_out, wide_out, file = paste0("out/scenario_KIDS_", names(scenarios)[i], "_data.rdata"))   
	rm(list = c("scen_out", "wide_out"))   
	print(paste("scenario kids", names(scenarios)[i]))        
}

stopImplicitCluster()
