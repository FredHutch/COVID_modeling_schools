##Check Parameters
#Author: Mia
#Creation Date: March 3rd, 2021
#Purpose: Evaluate the quality of a fit given a single set of parameters

Check_Parameters = function(initial_state,
                            params_guess,
                            params_base,
                            the_data,
                            start.date,
                            end.date,
                            params_temporal = NULL,
                            start_from_first_inf = FALSE){
print_debug = 1
data_calib = subset(the_data, date >= start.date & date <= end.date)

sse = calc_sse_multi(pars = params_guess, pars_names = names(params_guess), 
                   pars_base = params_base, pars_temporal = params_temporal, state = initial_state, 
                   data_actual = data_calib, start_from_first_inf = start_from_first_inf)
print_debug = 0
print("For initial values...")
print(sse)
print(paste("score", sum(sse), sep = '='))
}
