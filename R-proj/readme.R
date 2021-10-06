# This file gives examples for how to set parameters and run the model

# model code and load data
library(HutchCOVID)
source("kc_read_data.R")

# plotting code
source("covid-model-plotting.R")

# to run the model starting with the first infection with default parameters for 300 days
out_new = run_model(params_base, NULL, 300, state)

# to run the model specifying start and stop dates
out_new = run_model_by_date(params_base, NULL, state, start_date = ymd("2020-02-01"), end_date = ymd("2020-04-30"))

### to specify temporal parameters ###
# it is a list of lists, here each list must include a date to take effect, then the name/value pairs
# for the parameters to change
# note that we append to the master params_temporal_base which has several time-varying parameters already defined
params_temporal = append(params_temporal_base, list(
  list( date = ymd("2020-06-21"), 
        delta_IM = c(0.25, 0.25, 0.25, 0.25), 
        delta_IS = c(0.4, 0.3, 0.3, 0.4), 
        rho_A = 0.25),
  list( date = ymd("2020-05-31"),
        sd = c(0.1, 0.1, 0.2, 0.35)),
  list( date = ymd("2020-07-01"),
        sd = c(0.15, 0.4, 0.35, 0.5)),
  list( date = ymd("2020-09-01"),
        sd = c(0.3, 0.2, 0.35, 0.35))
))

# then run the model with temporal parameters
# model can be run from first infection date
out_new = run_model(params_base, params_temporal, 300, state)

# calculate R effective

# R effective is calculated by default (otherwise set calc_r_eff = FALSE when calling run_model, i.e. during calibration, to save time)
# we'll use the long for of the data to plot it
out_long = shape_data_long(out_new, params_base$model_day0_date)
out_reff = filter(out_long, state == "r_eff")

plot_r_eff(out_new, params_base)

out_data = shape_data_wide(shape_data_long(out_new, params_base$model_day0_date)) 
lines(out_data$date, out_data$tot_inf / max(out_data$tot_inf, na.rm = TRUE))

# or by specifying time period
out_new = run_model_by_date(params_base, params_temporal, state, start_date = ymd("2020-07-01"), end_date = ymd("2020-10-31"))


### to run the model with multiple strains and/or vaccines ###

# define the relavent parameters
ve = c(0, 0.95, 0.8, 0.95)
params_2s_2v = params_base
params_2s_2v$n_strain = 2
params_2s_2v$n_vaccine = 2
params_2s_2v$n_recovered = 2
params_2s_2v$vep = matrix(c(ve, ve), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$ves = matrix(c(ve, ve), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$vei = matrix(c(ve, ve), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$veh = matrix(c(ve, ve), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$vac_rate = 0
params_2s_2v$vac_dist = c(0, 0.1, 0.1, 0.8)
params_2s_2v$vac_priority = 4:2
params_2s_2v$vac_coverage = 0.8
params_2s_2v$strain_infectivity = c(1, 1.7)
params_2s_2v$strain_severity = c(1, 1.2)
params_2s_2v$strain_import = matrix(0, nrow = params_2s_2v$n_strain, ncol = params_2s_2v$n_age)
params_2s_2v$W = create_W_no_waning(params_2s_2v$n_age, params_2s_2v$n_vaccine, params_2s_2v$n_recovered)
params_2s_2v$Y = create_Y_recovered_by_vax(params_2s_2v$n_age, params_2s_2v$n_vaccine, params_2s_2v$n_strain)
params_2s_2v$V = create_V_recovered_by_vax(params_2s_2v$n_age, params_2s_2v$n_vaccine)

verify_params(params_2s_2v)

# make an inital state for the strains and vaccines
state_2p_2v = initial_state(params_2s_2v$n_age, params_2s_2v$n_strain, params_2s_2v$n_vaccine, params_2s_2v$n_recovered) 

# and run the model
out_new = run_model(params_2s_2v, params_temporal, 300, state_2p_2v)

# TODO problem calculating R_eff for strains/vaccines
# Mia fix: 6/1 change params_base to params_2s_2v
r_eff_date = get_r_effective(params_2s_2v, params_temporal, out_new)


# now lets start adding the new strain on July 1 (yes way early)
params_temporal_strain = append(params_temporal_base, list(
  list( date = ymd("2020-05-01"), 
        sd = c(0.3, 0.35, 0.35, 0.55)),
  list( date = ymd("2020-06-01"),
        sd = c(0.1, 0.1, 0.1, 0.5)),
  list( date = ymd("2020-07-01"),
        sd = c(0.2, 0.3, 0.3, 0.5),
        strain_import = matrix(c(c(0, 0, 0, 0), c(0, 5, 5, 0)), 
                               nrow = params_2s_2v$n_strain, ncol = N.age.groups, byrow = TRUE)),
  list( date = ymd("2020-08-01"),
        vac_rate = 5000)
))

out_new = run_model(params_2s_2v, params_temporal_strain, 300, state_2p_2v)

# lets try even more stains and vaccines with a sequential introduction and rollout for both strains and vaccines
ve = c(0, 0.95, 0.5, 0.8, 0.95, 0.8) # efficacy of vaccine / previous infection
params_many_s_v = params_base
params_many_s_v$n_strain = 5
params_many_s_v$n_vaccine = 3
params_many_s_v$n_recovered = 3
params_many_s_v$vep = matrix(c(ve, ve, 0.8 * ve, 0.5 * ve, 0.7 * ve),
                             nrow = params_many_s_v$n_strain, byrow = TRUE)
params_many_s_v$ves = matrix(c(ve, ve, 0.8 * ve, 0.5 * ve, 0.7 * ve),
                             nrow = params_many_s_v$n_strain, byrow = TRUE)
params_many_s_v$vei = matrix(c(ve, ve, 0.8 * ve, 0.5 * ve, 0.7 * ve),
                             nrow = params_many_s_v$n_strain, byrow = TRUE)
params_many_s_v$veh = matrix(c(ve, ve, 0.8 * ve, 0.5 * ve, 0.7 * ve),
                             nrow = params_many_s_v$n_strain, byrow = TRUE)
params_many_s_v$vac_rate = 0
params_many_s_v$vac_dist = c(0, 0.1, 0.1, 0.8)
params_many_s_v$vac_priority = 4:2
params_many_s_v$vac_coverage = 0.8
params_many_s_v$strain_infectivity = c(1, 1.7, 1.3, 1.5, 1.2)
params_many_s_v$strain_severity = rep(1, params_many_s_v$n_strain)
params_many_s_v$strain_import = matrix(0, nrow = params_many_s_v$n_strain, ncol = params_many_s_v$n_age)
params_many_s_v$W = create_W_most_recent(params_many_s_v$n_age, params_many_s_v$n_vaccine, params_many_s_v$n_recovered, 
                                         matrix(rep(c(0, 0.001, 0.001, 0.005, 0.005, 0.005), each = 4), ncol = 4, byrow = TRUE) )
params_many_s_v$Y = create_Y_recovered_by_vax(params_many_s_v$n_age, params_many_s_v$n_vaccine, params_many_s_v$n_strain)
params_many_s_v$V = create_V_recovered_by_vax(params_many_s_v$n_age, params_many_s_v$n_vaccine)

verify_params(params_many_s_v)

state_many_s_v = initial_state(params_many_s_v$n_age, params_many_s_v$n_strain, params_many_s_v$n_vaccine, params_many_s_v$n_recovered) 

params_temporal_sv = append(params_temporal_base, list(
  list( date = ymd("2020-05-01"), 
        sd = c(0.5, 0.6, 0.6, 0.8)),
  list( date = ymd("2020-06-01"),
        sd = c(0.1, 0.1, 0.1, 0.5)),
  list( date = ymd("2020-07-01"),
        sd = c(0.1, 0.35, 0.35, 0.5),
        strain_import = matrix(c(c(0, 0, 0, 0), c(0, 5, 5, 0), c(0, 1, 1, 0), c(0, 0, 0, 0), c(0, 0, 0, 0)),
                               nrow = params_many_s_v$n_strain, ncol = params_many_s_v$n_age, byrow = TRUE),
        vac_rate = 2000,
        V = create_V_recovered_by_vax(params_many_s_v$n_age, params_many_s_v$n_vaccine, vac_prop = c(1, 0))),
  list( date = ymd("2020-08-01"),
        sd = c(0.1, 0.1, 0.1, 0.5),
        strain_import = matrix(c(c(0, 0, 0, 0), c(0, 5, 5, 0), c(0, 1, 1, 0), c(0, 0, 0, 0), c(0, 0, 0, 0)),
                               nrow = params_many_s_v$n_strain, ncol = params_many_s_v$n_age, byrow = TRUE),
        vac_rate = 6000,
        V = create_V_recovered_by_vax(params_many_s_v$n_age, params_many_s_v$n_vaccine, vac_prop = c(5/6, 1/6))),
  list( date = ymd("2020-09-01"),
        sd = c(0.1, 0.35, 0.35, 0.5),
        strain_import = matrix(c(c(0, 0, 0, 0), c(0, 5, 5, 0), c(0, 5, 5, 0), c(0, 2, 2, 0), c(0, 2, 2, 0)),
                               nrow = params_many_s_v$n_strain, ncol = N.age.groups, byrow = TRUE),
        vac_rate = 10000,
        V = create_V_recovered_by_vax(N.age.groups, params_many_s_v$n_vaccine, vac_prop = c(0.5, 0.5)))
))

out_new = run_model(params_many_s_v, params_temporal_sv, 700, state_many_s_v)
r_eff_s_v = get_r_effective(params_many_s_v, params_temporal_sv, out_new)


### model output ###
# here are the two formats available

# the long table form
out_long = shape_data_long(out_new, params_base$model_day0_date)

# the wide table form, with per-age group and totals summing across ages, both daily and cummulative
out_data = shape_data_wide(out_long)

### plot the output!! ###

# here's how to plot the shaped data
plot_age_fit(out_data, "cases", kc_data)
plot_age_fit(out_data, "hosp", kc_data)
plot_age_fit(out_data, "deaths", kc_data)

# alternatively the plotting function can run the model for you too

# either calibration parameters directly
plot_age_fit_from_params(c(0.015, 5), c("bstar", "first_inf_day"), params_base, params_temporal_base, state, "deaths", kc_data_calib)

# or just some base and/or temporal parameters, just pass in NULL if you don't want to override any parameters
plot_age_fit_from_params(NULL, NULL, params_many_s_v, params_temporal_sv, state_many_s_v, "cases", kc_data_calib)

### calibration ###

# here's how to calculate error for a given parameter vector and data
# this will run for the dates in the data (assumes state corresponds to first day in data, 
# this is not actually correct!)
calc_sse_multi(c(0.03, 0.15, 0.15, 0.15, 0.15), c("bstar", "sd_1", "sd_2", "sd_3", "sd_4"), 
               params_base, NULL, state, kc_data_calib)

# here's how to run the model from first infection date through April
calc_sse_multi(c(0.03, 0.4, 0.4, 0.4, 0.4), c("bstar", "sd_1", "sd_2", "sd_3", "sd_4"), 
               params_base, NULL, state, 
               filter(kc_data_calib, date <= ymd("2020-04-30")), 
               start_from_first_inf = TRUE)

# and with temporal parameters too
calc_sse_multi(c(0.03, 0.4, 0.4, 0.4, 0.4), c("bstar", "sd_1", "sd_2", "sd_3", "sd_4"), 
               params_base, params_temporal, state, kc_data_calib, start_from_first_inf = TRUE)

# we can also just check the default parameters
calc_sse_multi(NULL, NULL, params_base, params_temporal, state, kc_data_calib, start_from_first_inf = TRUE)

# and here's an example calibration based on current model params
# TODO this is out of date and shows the gradient algorithm, look at the scripts in the calibration directory

# bounds (not sure on all of these!! may need updating)
params_lower <- c(bstar = 0.01, 
                     beta_d = 0.5, 
                     first_inf_day = 7,
                     sd_1 = 0.05,
                     sd_2 = 0.05,
                     sd_3 = 0.05,
                     sd_4 = 0.05,
                     delta_I_1 = 0.0001,   
                     delta_I_2 = 0.0001,   
                     delta_I_3 = 0.0001,   
                     delta_I_4 = 0.0001,   
                     h_1 = 0.01,
                     h_2 = 0.01,
                     h_3 = 0.01,
                     h_4 = 0.01
)
params_upper <- c(bstar = 0.025, 
                     beta_d = 0.75, 
                     first_inf_day = 17,
                     sd_1 = 0.7,
                     sd_2 = 0.7,
                     sd_3 = 0.7,
                     sd_4 = 0.7,
                     delta_I_1 = 0.1,   
                     delta_I_2 = 0.1,   
                     delta_I_3 = 0.1,   
                     delta_I_4 = 0.1,   
                     h_1 = 0.5,
                     h_2 = 0.5,
                     h_3 = 0.5,
                     h_4 = 0.5
)

# calibrate from first infection to April 30
# for future periods, change TRUE to FALSE and filter data to period
res = optim(par = (params_lower + params_upper) / 2, # probably need to define a better guess...
            fn = calc_sse_simple,
            gr = NULL,
            names(params_lower), params_base, NULL, state, filter(kc_data_calib, date <= ymd("2020-04-30")), TRUE, 
            method = "L-BFGS-B",
            control = c(maxit=1000,trace=2),
            lower = params_lower,
            upper = params_upper)


### dynamic sd ###

# the basic idea is to run the model with calibrated parameters up to a certain point, 
# then take the state from that and run forward from that point on adjusting sd
# note that we now include the value of sd in the output, so there columns need to be removed (like the time column)
# before the last line of the matrix output can be used as the new state, there is now a helper function for this

# first run the calibrated part
out_calib = run_model_by_date(params_base, params_temporal, state, 
                              start_date = get_date_from_model_day(params_base$first_inf_day, params_base$model_day0_date), 
                              end_date = ymd("2020-10-01"))
state_at_calib_end = extract_final_state(out_calib)

data_calib = shape_data_wide(shape_data_long(out_calib, params_base$model_day0_date))
plot_age_fit(data_calib, "cases", kc_data)

# now we can run the dynamic sd part, the default is cases for the threshold
params_dynamic_sd = params_base
params_dynamic_sd$dynamic_sd = TRUE
params_dynamic_sd$dynamic_sd_init_date = ymd("2020-11-01")

# optionally run these lines to enable a threshold for hospitalizations instead
params_dynamic_sd$dynamic_sd_func = "calculate_dynamic_sd_hosp"
params_dynamic_sd$dynamic_sd_lock_thresh = 20 * kc_pop / 100000
params_dynamic_sd$dynamic_sd_release_thresh = 10 * kc_pop / 100000

# and here's for a combined cases/hospitalizations 
params_dynamic_sd$dynamic_sd_func = "calculate_dynamic_sd_cases_and_hosp"
params_dynamic_sd$dynamic_sd_lock_thresh = c(500, 30) * kc_pop / 100000
params_dynamic_sd$dynamic_sd_release_thresh = c(200, 20) * kc_pop / 100000


out_sd = run_model_by_date(params_dynamic_sd, params_temporal, state_at_calib_end, ymd("2020-10-01"), ymd("2021-06-01")  )

# TODO need new plotting routines for projections
out_sd_long = shape_data_long(out_sd, params_base$model_day0_date)
out_sd_data = shape_data_wide(out_sd_long)
plot_age_fit(out_sd_data, "cases", kc_data)
plot_age_fit(out_sd_data, "hosp", kc_data)

ggplot(data = filter(out_sd_long, state == "sd"), aes(x = date, y = value, group = age)) +
  geom_line(aes(col = age)) +
  theme_classic()

