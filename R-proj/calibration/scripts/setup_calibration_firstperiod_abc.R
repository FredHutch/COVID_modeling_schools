# these are the global variables for the abc calibration that are therefore available both for the calibration, 
# as well as the later analysis
# these are variables like the parameter names and date ranges that are important to keep synchronized
# this is necessary because the EasyABC objective function doesn't allow the passing of additional parameters outside the parameter vector of calibrated parameters
# furthermore, to run in parallel, this file must be sources inside the function, or the necessary variables will not be present on other cores than that where the script was launched

library(HutchCOVID)
source("kc_read_data.R")

params_calib_abc = params_base
params_temporal_calib_abc = params_temporal_base
state_abc = state
data_abc = filter(kc_data, date < ymd("2020-05-01"))

# don't normalize by less than 1, since that will inflate error too much
data_means = list( cases = pmax( apply(data_abc[,paste0("pos", 1:4)], 2, mean), rep(1, 4)),
                   deaths = pmax( apply(data_abc[,paste0("deaths", 1:4)] + data_abc[,paste0("hosp_deaths", 1:4)], 2, mean), rep(1, 4)),
                   hosp = pmax( apply(data_abc[,paste0("hosps", 1:4)], 2, mean), rep(1, 4))) #,
# negtests = pmax( apply(data_abc[,paste0("neg", 1:4)], 2, mean), rep(1, 4)))

# previously were doing weekly, but try switching this to 3 (i.e. more like twice weekly) to have more data points, note we 
# are still using 7 day moving average to smooth out weekends/weekdays
dates_abc = c(seq(from = data_abc$date[1], to = data_abc$date[nrow(data_abc)], by = 7), data_abc$date[nrow(data_abc)]) # add the last date that is only 6 days after

summary_stats_abc = calc_data_stats(data_abc, dates_abc, data_means)

params_priors = list(
#  bstar = c("unif", 0.01, 0.025),  # old range R0 1 - 2.5
  bstar = c("unif", 0.01, 0.03),
  beta_d = c("unif", 0.25, 0.9),
  first_inf_day = c("unif", 7, 25),
  sd_1 = c("unif", 0.3, 0.7), # lower bound was 0.05, now 0.3
  sd_2 = c("unif", 0.3, 0.7), 
  sd_3 = c("unif", 0.3, 0.7), 
  sd_4 = c("unif", 0.3, 0.7), 
  delta_IM_1 = c("unif", 0.01, 0.25), # lower bound was 0.05, now 0.01
  delta_IM_2 = c("unif", 0.01, 0.25),
  delta_IM_3 = c("unif", 0.01, 0.25),
  delta_IM_4 = c("unif", 0.01, 0.25),
  delta_IS_1 = c("unif", 0.1, 0.4),
  delta_IS_2 = c("unif", 0.1, 0.4),
  delta_IS_3 = c("unif", 0.1, 0.4),
  delta_IS_4 = c("unif", 0.1, 0.4),
  rho_A_1 = c("unif", 0.15, 0.3),
  rho_A_2 = c("unif", 0.15, 0.3),
  rho_A_3 = c("unif", 0.15, 0.3),
  rho_A_4 = c("unif", 0.15, 0.3),
  m_1 = c("unif", 0.988, 0.996),
  m_2 = c("unif", 0.96, 0.995),
  m_3 = c("unif", 0.87, 0.96),
  m_4 = c("unif", 0.62, 0.87)
)

abc_param_names = names(params_priors)
