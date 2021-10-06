library(HutchCOVID)
source("kc_read_data.R")
source("covid-model-plotting.R")


# test Reff for a wide range of bstar values

paramset_bstar = matrix(seq(0.01, 0.04, by = 0.001), ncol = 1)
paramset_name = "bstar"

end_date = ymd("2020-06-01")
bstar_out = get_model_data_param_sets(paramset_bstar, paramset_name, params_base, params_temporal_base, 
                                      state, NULL, NULL, end_date, out_type = "matrix")

bstar_wide = lapply(bstar_out, function(x) shape_data_wide(shape_data_long(x, params_base$model_day0_date)))

# wide range of infection peaks from bstar values
plot_age_fit_from_data_set(bstar_wide, "inf", kc_data, plot_data_as = "p", plot_age_separate = TRUE)

# but r_eff always the same
plot_r_eff_from_model_list(bstar_out, paramset_bstar, paramset_name, params_base)


# test Reff for multiple strains

params_2s_2v = params_base
params_2s_2v$n_strain = 1
params_2s_2v$n_vaccine = 2
params_2s_2v$vep = matrix(rep(0, (params_2s_2v$n_vaccine + params_2s_2v$n_strain) * params_2s_2v$n_strain), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$ves = matrix(rep(0, (params_2s_2v$n_vaccine + params_2s_2v$n_strain) * params_2s_2v$n_strain), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$vei = matrix(rep(0, (params_2s_2v$n_vaccine + params_2s_2v$n_strain) * params_2s_2v$n_strain), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$veh = matrix(rep(0, (params_2s_2v$n_vaccine + params_2s_2v$n_strain) * params_2s_2v$n_strain), nrow = params_2s_2v$n_strain, byrow = TRUE)
params_2s_2v$vac_rate = 0
params_2s_2v$strain_infectivity = rep(1, params_2s_2v$n_strain)
params_2s_2v$strain_severity = rep(1, params_2s_2v$n_strain)
params_2s_2v$strain_import = matrix(0, nrow = params_2s_2v$n_strain, ncol = params_2s_2v$n_age)
params_2s_2v$W = create_W_no_waning(params_2s_2v$n_age, params_2s_2v$n_vaccine, params_2s_2v$n_recovered)
params_2s_2v$Y = create_Y_recovered_by_strain(params_2s_2v$n_age, params_2s_2v$n_vaccine, params_2s_2v$n_strain)
verify_params(params_2s_2v)

state_2p_2v = initial_state(params_2s_2v$n_age, params_2s_2v$n_strain, params_2s_2v$n_vaccine, 1) 
out_2p_2v = run_model(params_2s_2v, params_temporal_base, 100, state_2p_2v)

# Error in max(r_eff) : invalid 'type' (complex) of argument
# r_eff becomes a complex number with multiple strains
plot_r_eff(out_2p_2v, params_2s_2v)

# but no error for single strain
out_base = run_model(params_base, params_temporal_base, 100, state)
plot_r_eff(out_base, params_base)
