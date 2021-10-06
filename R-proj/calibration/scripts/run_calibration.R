########Initial Calibration Script
#Created: March 1st, 2021
#Author: Mia Moore
#Outputs: Calibrated parameters_file (TODO document parameter_file format)
#Description: Calibrate the base model with a single strain, and no vaccines, started at Date XXX and ending at Date YYY

#Mia: 3/1/21 I'm starting by copying and pasting the calibration code from the other branch.
# Model fitting
library(lubridate)
library(mco)


library(HutchCOVID)
# 
# Read in diag County actual data for calibration ####
# 
source("kc_read_data.R")
# 
source("../Initial_Calibration/Calibration_Functions/calibrate_single_period.R")

# calibration  parameters
params_calib = params_base

params_calib_temporal = list()
        

# data off for calibration is May 1st (lockdown)
date.list = c(as.Date("2020-1-01"),
              as.Date("2020-4-30"),
              as.Date("2020-5-31"),
              as.Date("2020-6-30"),
              as.Date("2020-7-31"),
              as.Date("2020-8-31"),
              as.Date("2020-9-30"),
              as.Date("2020-10-31"),
              as.Date("2020-11-30"),
              as.Date("2020-12-31"))
suffix=c("lockdown",
         "May",
         "June",
         "July",
         "Aug",
         "Sept",
         "Oct",
         "Nov",
         "Dec")

current_state = initial_state(n_age = 4, n_strain = 1, n_vaccine = 1, n_recovered = 1) 

# here are all the parameters that are calibrated monthly, the first period is a little different
params_kc_guess = c(sd_1 = 0.7,
                    sd_2 = 0.65,
                    sd_3 = 0.6,
                    sd_4 = 0.65,
                    delta_I_1 = 0.01,   
                    delta_I_2 = 0.1,   
                    delta_I_3 = 0.1,   
                    delta_I_4 = 0.1,   
                    h_1 = 0.15,
                    h_2 = 0.15,
                    h_3 = 0.15,
                    h_4 = 0.25
)
params_kc_lower = c(sd_1 = 0.05,
                    sd_2 = 0.05,
                    sd_3 = 0.05,
                    sd_4 = 0.05,
                    delta_I_1 = 0.0001,   
                    delta_I_2 = 0.0001,   
                    delta_I_3 = 0.0001,   
                    delta_I_4 = 0.0001,   
                    h_1 = 0.1,
                    h_2 = 0.1,
                    h_3 = 0.1,
                    h_4 = 0.1
)
params_kc_upper = c(sd_1 = 0.7,
                    sd_2 = 0.7,
                    sd_3 = 0.7,
                    sd_4 = 0.7,
                    delta_I_1 = 0.2,   
                    delta_I_2 = 0.2,   
                    delta_I_3 = 0.2,   
                    delta_I_4 = 0.2,   
                    h_1 = 0.3,
                    h_2 = 0.3,
                    h_3 = 0.3,
                    h_4 = 0.3
)

###### Calibrate start of epidemic and first lockdown ############
#
# Note there are a few parameters that are only calibrated in this first period
params_kc_guess_init = c(bstar = 0.015, 
                         beta_d = 0.5, 
                         first_inf_day = 14,
                         rho_A = 0.25,
                         params_kc_guess)
params_kc_lower_init = c(bstar = 0.01, 
                         beta_d = 0.25, 
                         first_inf_day = 7,
                         rho_A = 0.15,
                         params_kc_lower)
params_kc_upper_init = c(bstar = 0.025, 
                         beta_d = 0.9, 
                         first_inf_day = 25,
                         rho_A = 0.3,
                         params_kc_upper)

# Model calibration by sum of squared errors (not used) ####
# Calculate error at starting values and bounds
Check_Parameters(current_state,
                 params_kc_guess_init,
                 params_calib,
                 kc_data,
                 date.list[1],
                 date.list[2],
                 params_calib_temporal,
                 start_from_first_inf = TRUE)
print("For lower bounds...")
Check_Parameters(current_state,
                 params_kc_lower_init,
                 params_calib,
                 kc_data,
                 date.list[1],
                 date.list[2],
                 params_calib_temporal,
                 start_from_first_inf = TRUE)

print("For upper bounds...")
Check_Parameters(current_state,
                 params_kc_upper_init,
                 params_calib,
                 kc_data,
                 date.list[1],
                 date.list[2],
                 params_calib_temporal,
                 start_from_first_inf = TRUE)

Sys.time()
res = fit_period(current_state,
                 params_kc_guess_init,
                 params_kc_lower_init, 
                 params_kc_upper_init, 
                 params_calib, 
                 kc_data,
                 date.list[1],
                 date.list[2],
                 params_calib_temporal,
                 TRUE)
Sys.time()

# now we need to extract the parameters from that run, save them, and get the model state at the end of that run
save(res, file = paste0("calib/res_", suffix[1], ".Rdata"))
pars_fit = res$par
params_calib = get_params(pars_fit, names(params_kc_guess_init))
out = run_model_by_date(params_calib, params_calib_temporal, current_state, 
                        get_date_from_model_day(params_calib$first_inf_day, params_calib$model_day0_date), date.list[2])
current_state = out[nrow(out),-1]

# now to optimize the rest of the monthly periods with the monthly changing parameters, 
# skip the first lockdown period that was already fit
for(i in which( !( seq_along(date.list) %in% c(1, length(date.list)) ) )){
  res = fit_period(current_state,
                   params_kc_guess,
                   params_kc_lower,
                   params_kc_upper,
                   params_calib,
                   kc_data,
                   date.list[i],
                   date.list[i + 1],
                   params_calib_temporal,
                   FALSE)

	#Extract best fit parameters
  pars_fit = res$par

  # Here we are just overwriting the parameters witht eh next set for this month
  # TODO we will eventually need to write a temporal parameters list with all the monthly parameters
  params_calib = get_params(pars_fit, names(params_kc_guess_init))

  # Run the ODE with those parameters
  out = run_model_by_date(params_calib, params_calib_temporal, current_state, date.list[i], date.list[i + 1])

	#Update the current state
  current_state = out[nrow(out),-1]
	
  Check_Parameters(current_state,
                   pars_fit,
                   params_calib,
                   kc_data,
                   date.list[i],
                   date.list[i + 1],
                   params_calib_temporal,
                   start_from_first_inf = FALSE)
  
  save(res, file = paste0("calib/res_", suffix[i], ".Rdata"))
}




