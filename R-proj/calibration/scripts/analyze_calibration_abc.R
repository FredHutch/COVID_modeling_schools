# global params, load model
source("calibration/scripts/setup_calibration_abc.R")  
#source("calibration/scripts/setup_calibration_sd134_abc.R")  
#source("calibration/scripts/setup_calibration_biweekly_sd_abc.R")  
#source("calibration/scripts/setup_calibration_firstperiod_abc.R")  
#source("calibration/scripts/setup_calibration_firstperiod_nocases_abc.R")  
source("covid-model-plotting.R")


# helper functions

# returns a vector of TRUE/FALSE for whether r matches each row of matrix M
# this should work but doesn't seem to for doubles???
#row_matches_in_matrix = function(r, M) { colSums(t(M) == r) == ncol(M) }

# return which row in M is closest to r
row_matches_in_matrix = function(r, M) 
{ 
  dists = apply(M, 1, function(i) sum((i - r)^2))
  return(which.min(dists))
}

# this script assumes that you have run the script run_calibration_abc.R and thus have the object abc_mcmc from that
# or that you have loaded the rdata with abc_mcmc
run_name = "abc_mcmc.rdata"
run_name = "out_firstperiod_mcmc.rdata" # all same dist?
run_name = "out_firstperiod_nocases_mcmc.rdata"

load(paste0(run_name, ".rdata"))

# only for firstperiod calibration
end_date = ymd("2020-05-02") # ymd("2020-12-31")

abc_best_params = abc_mcmc$param[which(abc_mcmc$dist < quantile(abc_mcmc$dist, 100/length(abc_mcmc$dist))),]
which.min(abc_mcmc$dist)
best_idx = row_matches_in_matrix(abc_mcmc$param[which.min(abc_mcmc$dist),], abc_best_params)

pdf("out/calib_bestfit.pdf", width = 9, height = 3)
par(mfrow = c(1,3))
plot_age_fit_from_params(abc_best_params[best_idx,], abc_param_names, params_calib_abc, NULL, state_abc, "cases", data_abc)
plot_age_fit_from_params(abc_best_params[best_idx,], abc_param_names, params_calib_abc, NULL, state_abc, "hosp", data_abc)
plot_age_fit_from_params(abc_best_params[best_idx,], abc_param_names, params_calib_abc, NULL, state_abc, "deaths", data_abc)
dev.off()


best_params_data = get_model_data_param_sets(abc_best_params, abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                                             state_abc, data_abc, NULL, end_date)
pdf("out/calib_fit.pdf", width = 8, height = 8)
par(mfrow = c(2,2))
plot_age_fit_from_data_set(best_params_data, "cases", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
plot_age_fit_from_data_set(best_params_data, "inf", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
plot_age_fit_from_data_set(best_params_data, "hosp", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
plot_age_fit_from_data_set(best_params_data, "deaths", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
dev.off()

# parameter hist
pdf("out/calib_param_posteriors.pdf", width = 12, height = 12)
par(mfrow = c(4, 4))
plot_param_posteriors(abc_best_params, abc_param_names, params_priors)
plot_total_infected(best_params_data)
dev.off()


pairs(abc_best_params, labels = abc_param_names)
pairs(param_sets_simp, labels = abc_param_names)

# calculate distance
mod_stats = model_wrapper_first_period(abc_best_params[best_idx,])
sum(sqrt((mod_stats - summary_stats_abc)^2))


output = read.table("output_mcmc", header = FALSE)
output = read.table("output", header = FALSE)
params = output[,1:length(abc_param_names)]
dist = output[,ncol(output)]

sum_stats = output[,(length(abc_param_names) + 1):ncol(output)]
dist = apply(sum_stats, 1, function(x) sqrt(sum((x - summary_stats_abc)^2)))

idxs = which(dist < quantile(dist, 0.1))
idxs_res = apply(abc_rej$param, 1, function(x) row_matches_in_matrix(x, params))

params_data = get_model_data_param_sets(as.matrix(params[which(dist < quantile(dist, 100/length(dist))),]), abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                                        state_abc, data_abc, NULL, end_date)
params_data = get_model_data_param_sets(as.matrix(params[idxs,]), abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                                        state_abc, data_abc, NULL, end_date)

plot_age_fit_from_data_set(params_data, "cases", data_abc)
plot_age_fit_from_data_set(params_data, "inf", data_abc)
plot_age_fit_from_data_set(params_data, "hosp", data_abc)
plot_age_fit_from_data_set(params_data, "deaths", data_abc)

plot_age_fit_from_params(unlist(params[nrow(params),]), abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                         state_abc, "cases", data_abc, NULL, end_date)

