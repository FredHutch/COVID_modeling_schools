# global params, load model
source("calibration/scripts/setup_calibration_abc.R")  
#source("calibration/scripts/setup_calibration_sd134_abc.R")  
#source("calibration/scripts/setup_calibration_biweekly_sd_abc.R")  
#source("calibration/scripts/setup_calibration_firstperiod_abc.R")  
#source("calibration/scripts/setup_calibration_firstperiod_nocases_abc.R")  
#source("calibration/scripts/setup_calibration_firstperiod_noschools_abc.R")  
#source("calibration/scripts/setup_calibration_lastperiod_abc.R")  
source("covid-model-plotting.R")
source("calibration/functions/deviance.R")  
library(abc)

# so we can run in parallel
library(foreach)
library(doParallel)
registerDoParallel(cores = 8)


# this script assumes that you have run the script run_calibration_abc.R and thus have the object abc_rej from that
load("out/abc_rej.rdata")
load("out_firstperiod_rej.rdata")
load("out_rej_new_sd.rdata")

run_name = "out_rej_new_new_sd"
run_name = "out_rej_new_new_sd_upd" # change sd timepoints, LHS
run_name = "out_rej_sd_134" # same as out_rej_new_new_sd_upd, but ages 1, 3, 4 share same sd
run_name = "out_rej_biweekly_sd" # same sd for all ages, no constraints, update bi-weekly
run_name = "out_firstperiod_rej" # just first period, now with sd both sides
run_name = "out_firstperiod_nocases_rej" # first period, sd both sides, fit only hosp
run_name = "out_firstperiod_newbounds_rej" # update bounds for bstar, sd, aand delta_IM
run_name = "out_firstperiod_noschools_rej" # school_sd = 0
run_name = "out_firstperiod_seq" # sequential algorithm 
run_name = "out_firstperiod_again_rej" # first period, now with way more simulations
run_name = "out_lastperiod_rej" # last period with posteriors as priors from firstperiod

load(paste0(run_name, ".rdata"))


# abc_rej = abc_seq # just to reuse code

# only for firstperiod calibration
start_date = NULL
end_date = ymd("2020-05-02") # ymd("2020-12-31")
# start_date = ymd("2021-03-31"); end_date = ymd("2021-06-15")


# check if parameter is in prior bounds, this is not necessarily true with sequential algorithms
create_dynamic_prior = function(sampleName, sampleArgs, densityName, densityArgs, 
                                isUniform = FALSE) {
  # $sampling: sample function with the given arguments. The used function (given
  # by 'name' argument) takes the arguments 'args' for generating a sample.
  # $density: create the density function with the given arguments. The used
  # function (given by 'name' argument) takes as arguments the quantile value and
  # the given 'args' for computing the density.  $isUniform: indicates if this
  # prior follows an uniform distribution (condition for methods that need LHS)
  list(sampling = function() {
    do.call(sampleName, as.list(as.numeric(sampleArgs)))
  }, density = function(value) {
    do.call(densityName, as.list(as.numeric(c(value, densityArgs))))
  }, isUniform = isUniform || sampleName == "runif", sampleArgs = sampleArgs)
}
create_legacy_prior = function(name, args) {
  name = name
  args = args
  switch(EXPR = name, unif = create_dynamic_prior("runif", c(1, args), "dunif", args, TRUE), 
         normal = create_dynamic_prior("rnorm", c(1, args), "dnorm",  args, TRUE), 
         lognormal = create_dynamic_prior("rlnorm", c(1, args), "dlnorm", args, TRUE), 
         exponential = create_dynamic_prior("rexp", c(1, args), "dexp", args, TRUE))
}
is_included <- function(res, prior) {
  test = TRUE
  for (i in 1:length(prior)) {
    test = test && (prior[[i]]$density(res[i]) > 0)
  }
  test
}
prior_dyn = lapply(params_priors, function(x) create_legacy_prior(x[1], x[2:3]))
params_good = apply(abc_rej$param, 1, is_included, prior_dyn)
abc_rej$stats = abc_rej$stats[params_good,]
abc_rej$param = abc_rej$param[params_good,]

# now we will determine the MCMC parameters for each distance measure from the rejection calibration set
stopifnot( length(summary_stats_abc) == dim(abc_rej$stats)[2] )
proposal_phi = 1
dist_simp = apply(abc_rej$stats, 1, function(x) sqrt(sum((x - summary_stats_abc)^2)))
dist_norm = apply(abc_rej$stats, 1, function(x) sqrt(sum((x - summary_stats_abc)^2 / abc_rej$stats_normalization)))

dist_max_simp = sort(dist_simp)[100]
dist_max_norm = sort(dist_norm)[100]

normalization_simp = rep(1, length(summary_stats_abc))
normalization_norm = abc_rej$stats_normalization

param_sets_simp = abc_rej$param[which(dist_simp <= dist_max_simp),]
param_sets_norm = abc_rej$param[which(dist_norm <= dist_max_norm),]

proposal_range_simp = apply(param_sets_simp, 2, sd) *proposal_phi / 2
proposal_range_norm = apply(param_sets_norm, 2, sd) *proposal_phi / 2

params_simp = get_model_data_param_sets(as.matrix(param_sets_simp), abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                                        state_abc, data_abc, start_date, end_date)
params_norm = get_model_data_param_sets(as.matrix(param_sets_norm), abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                                        state_abc, data_abc, start_date, end_date)

estimate_mode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]
}

apply(param_sets_simp, 2, range)
apply(param_sets_simp, 2, median)
apply(param_sets_simp, 2, estimate_mode)


# goodness of fit
# ONLY EXECUTE ONE OF THE TWO FOLLOWING OPTIONS
# for hosp only, need to re-calculate summary stats for all measures to compare with others
# model_stats = t( apply(param_sets_simp, 1, function(x) calc_model_stats(x, abc_param_names, params_calib_abc, params_temporal_calib_abc, state_abc, 
#                                                                         TRUE, dates = dates_abc, rescale_factors = data_means) ) )
# summary_stats_abc = calc_data_stats(data_abc, dates_abc, data_means)

# otherwise take already calculated summary stats
model_stats = abc_rej$stats[which(dist_simp <= dist_max_simp),]

theta_hat = theta_hat_stats(as.matrix(param_sets_simp), "median", abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                            state_abc, dates_abc, data_means)
# optionally to just consider hosp
#theta_hat = theta_hat_stats(as.matrix(param_sets_simp), "median", abc_param_names, params_calib_abc, params_temporal_calib_abc, 
#                            state_abc, dates_abc, data_means, stats_to_include = "hosp")
dic_dist_simp = calculate_DIC(summary_stats_abc, model_stats, theta_hat$theta_hat_summary_stat)
deviance_dist_simp = expected.deviance(summary_stats_abc, model_stats) 
pD_dist_simp = calculate_pD(summary_stats_abc, model_stats, theta_hat$theta_hat_summary_stat)

pdf(paste0("out/sd_changes_dist_simp_", run_name,".pdf"), width = 8, height = 8)
par(mfrow = c(2, 2), mar = c(6, 4, 1, 1), las = 1)
plot_sd_from_param_set(param_sets_simp, abc_param_names, params_calib_abc$init_sd_start_date, params_calib_abc$init_sd_full_date)
dev.off()

pdf(paste0("out/calib_param_posteriors_dist_simp_", run_name, ".pdf"), width = 12, height = 12)
par(mfrow = c(4, 4))
plot_param_posteriors(param_sets_simp, abc_param_names, params_priors)
dev.off()

pdf(paste0("out/calib_fit_dist_simp_", run_name, ".pdf"), width = 8, height = 8)
par(mfrow = c(2,2))
plot_age_fit_from_data_set(params_simp, "cases", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
plot_age_fit_from_data_set(params_simp, "inf", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
plot_age_fit_from_data_set(params_simp, "hosp", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)
plot_age_fit_from_data_set(params_simp, "deaths", filter(data_abc, date %in% dates_abc), plot_data_as = "p", plot_age_separate = TRUE)

plot_r_eff_from_param_set(param_sets_simp, abc_param_names, params_calib_abc, params_temporal_calib_abc, 
                          state_abc, start_date, end_date)
plot_r0_from_param_set(param_sets_simp, abc_param_names, params_calib_abc)
plot_total_infected(params_simp)
plot(0, 0, axes = FALSE, type = "n")
text(0, 0, paste("Deviance =", deviance_dist_simp$expected.deviance, "\nDIC =", dic_dist_simp, "\npD =", pD_dist_simp))
dev.off()


# create parameters list from the first rej data
# NOTE this saves param_sets_simp, so using simple distance measure
source("calibration/functions/manage_parameters.R")
params_firstperiod_rej = create_parameterset_list(param_sets_simp, abc_param_names, params_calib_abc, params_temporal_calib_abc)
save(params_firstperiod_rej, file = paste0("out/params_", run_name, ".csv"))

colnames(param_sets_simp) = abc_param_names
write.csv(param_sets_simp, file = paste0("out/params_", run_name, ".csv"), quote = FALSE, row.names = FALSE)

##############################################################################################################################################

# create plots for model description of calibration
source("calibration/scripts/setup_calibration_lastperiod_abc.R")  
parameter_file = "out/parameter_sets/params_out_lastperiod_rej.csv"
param_set_100 = read.csv(parameter_file, check.names = FALSE) 
param_names = names(param_set_100)
start_date = ymd("2021-03-31"); end_date = ymd("2021-08-15"); calibration_date = ymd("2021-06-15")
dates_plot = c(dates_abc, seq(from = calibration_date + 7, to = end_date, by = 7)) # calibration_date is last day of calibration period though it is only 6 days after


model_100_long = get_model_data_param_sets(as.matrix(param_set_100), param_names, params_calib_abc, params_temporal_calib_abc, 
                                      state_abc, data_abc, start_date, end_date, out_type = "long", parallel = TRUE)
model_100_wide = lapply(model_100_long, shape_data_wide)

pdf("out/params_calib_cases.pdf", width = 8, height = 6)
par(mfrow = c(2, 2), las = 1, mar = c(2, 3, 1, 1) + 0.1)
plot_age_fit_from_data_set(model_100_wide, "cases", filter(kc_data, date %in% dates_plot), 
                           calibration_date = calibration_date, plot_data_as = "p", plot_age_separate = TRUE)
dev.off()

pdf("out/params_calib_hosp.pdf", width = 8, height = 6)
par(mfrow = c(2, 2), las = 1, mar = c(2, 3, 1, 1) + 0.1)
plot_age_fit_from_data_set(model_100_wide, "hosp", filter(kc_data, date %in% dates_plot), plot_data_as = "p", plot_age_separate = TRUE)
abline(h = ymd("2021-06-15"), col = "gray80")
dev.off()

pdf("out/params_calib_death.pdf", width = 8, height = 6)
par(mfrow = c(2, 2), las = 1, mar = c(2, 3, 1, 1) + 0.1)
plot_age_fit_from_data_set(model_100_wide, "deaths", filter(kc_data, date %in% dates_plot), plot_data_as = "p", plot_age_separate = TRUE)
abline(h = ymd("2021-06-15"), col = "gray80")
dev.off()


pdf("out/params_calib_strain_proportion.pdf", width = 4.5, height = 3.5)
par(las = 1, mar = c(2, 4, 1, 1) + 0.1)
plot_strain_proportion_median(model_100_long)
dev.off()


###############################################################################################################################################

### goodness of fit
library("abc")
load("results_new_sd.rdata")


model_fit = gfit(summary_stats_abc, abc_rej$stats[which(dist_simp <= dist_max_simp),], nb.replicate = 100, tol = 0.01)
plot(model_fit)
summary(model_fit)

# expected deviance from Francois & Laval 2011
# DIC = 2 D_1 - D_1(theta_hat) where D_1 is the expected deviance function
# for theta_hat, need to get median from posterior and then calculate summary stats for that

expected.deviance(summary_stats_abc, abc_rej$stats[which(dist_simp <= dist_max_simp),]) # D_1 = 14.88831 for model with sd directional but bad time points DIC = 2*14.9 - 8.7 ??
calculate_deviance(summary_stats_abc, abc_rej$stats[which(dist_simp <= dist_max_simp),])


# 9.132447 for older sd (out_rej_new_new_sd) before adding july and faster november
# 9.217041 for sd_134 where it fits 1st wave then low
# 9.344829 for biweekly with non age specific sd, which fits beginning ok but not nov peak (expected.dev = 24.72419, calc_dev = 9.28207)
# 9.262882 for sd_134 with fixed sd so kinda sorta hits later peaks but not well (expected.dev = 22.50715, calc_dev = 9.219009)


# 10.06982 for firstperiod (different data!) (e.d = 10.24462, c_d = 9.962128)

# 4.60115 for firstperiod hosp only (dist simp)
# 4.594255for firstperiod hosp only (dist norm)

# 15.19897 for firstperiod calibrated to hosp only (dist simp) but for all data
# 14.98029 for firstperiod calibrated to hosp only (dist simp) but for all data


# fit posterior distributions
library(MASS)

for (p in 1:length(abc_param_names))
{
  print(abc_param_names[p])
  x = param_sets_simp[,p]
  
  
  norm_fit = fitdistr(x, densfun = "normal")
  print(norm_fit)
  print(paste("AIC normal", -2 * norm_fit$loglik + 2*2))
  
  exp_fit = fitdistr(x, densfun = "exponential")
  print(exp_fit)
  print(paste("AIC exponential", -2 * exp_fit$loglik + 2*1))
  
  lognorm_fit = fitdistr(x, densfun = "lognormal")
  print(lognorm_fit)
  print(paste("AIC lognormal", -2 * lognorm_fit$loglik + 2*2))
  
  print(range(x))
  print(paste("AIC unif", -2 * sum(log(dunif(x, min(x), max(x)))) + 2*2))
}
