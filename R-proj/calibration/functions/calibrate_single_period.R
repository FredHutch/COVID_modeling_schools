##Calibrate Single Period
#Author: Mia
#Creation Date: March 1st, 2021
#Purpose: Define a function for the calibration for a single time period

fit_period = function(initial_state,
                      params_guess,
                      params_lower,
                      params_upper,
                      params_base,
                      the_data,
                      start.date,
                      end.date,
                      params_temporal = NULL,
                      start_from_first_inf = FALSE
 ){
  data_calib = subset(the_data, date >= start.date & date <= end.date)
  res = optim(par = params_guess,
              fn = calc_sse_simple,
              gr = NULL,
              names(params_guess), params_base, params_temporal, initial_state, data_calib, start_from_first_inf,
              method = "L-BFGS-B",
              control = c(maxit=1000,trace=2),
              lower = params_lower,
              upper = params_upper)
  
  return(res)
}
 
