# these are the global variables for the abc calibration that are therefore available both for the calibration, 
# as well as the later analysis
# these are valiables like the parameter names and date ranges that are important to keep synchronized
# this is necessary because the EasyABC objective function doesn't allow the passing of additional parameters outside the parameter vector of calibrated parameters
# furthermore, to run in parallel, this file must be sources inside the function, or the necessary variables will not be present on other cores than that where the script was launched

library(HutchCOVID)
source("kc_read_data.R")

params_calib_abc = params_base
params_temporal_calib_abc = params_temporal_base
state_abc = state
data_abc = filter(kc_data, date < ymd("2021-01-02"))

# don't normalize by less than 1, since that will inflate error too much
data_means = list( cases = pmax( apply(data_abc[,paste0("pos", 1:4)], 2, mean), rep(1, 4)),
                   deaths = pmax( apply(data_abc[,paste0("deaths", 1:4)] + data_abc[,paste0("hosp_deaths", 1:4)], 2, mean), rep(1, 4)),
                   hosp = pmax( apply(data_abc[,paste0("hosps", 1:4)], 2, mean), rep(1, 4))) #,
# negtests = pmax( apply(data_abc[,paste0("neg", 1:4)], 2, mean), rep(1, 4)))

# previously were doing weekly, but try switching this to 3 (i.e. more like twice weekly) to have more data points, note we 
# are still using 7 day moving average to smooth out weekends/weekdays
dates_abc = seq(from = data_abc$date[1], to = data_abc$date[nrow(data_abc)], by = 7)

summary_stats_abc = calc_data_stats(data_abc, dates_abc, data_means)

params_priors = list(
  bstar = c("unif", 0.01, 0.025),  
  beta_d = c("unif", 0.25, 0.9),
  first_inf_day = c("unif", 7, 25),
  delta_IM_1 = c("unif", 0.05, 0.25),
  delta_IM_2 = c("unif", 0.05, 0.25),
  delta_IM_3 = c("unif", 0.05, 0.25),
  delta_IM_4 = c("unif", 0.05, 0.25),
  delta_IS_1 = c("unif", 0.1, 0.4),
  delta_IS_2 = c("unif", 0.1, 0.4),
  delta_IS_3 = c("unif", 0.1, 0.4),
  delta_IS_4 = c("unif", 0.1, 0.4),
  rho_A_1 = c("unif", 0.15, 0.3),
  rho_A_2 = c("unif", 0.15, 0.3),
  rho_A_3 = c("unif", 0.15, 0.3),
  rho_A_4 = c("unif", 0.15, 0.3),
  sd_1234 = c("unif", 0.05, 0.7),
  m_1 = c("unif", 0.988, 0.996),
  m_2 = c("unif", 0.96, 0.995),
  m_3 = c("unif", 0.87, 0.96),
  m_4 = c("unif", 0.62, 0.87),
  # h_1 = c("unif", 0.1, 0.3),
  # h_2 = c("unif", 0.1, 0.3),
  # h_3 = c("unif", 0.1, 0.3),
  # h_4 = c("unif", 0.1, 0.3),
  `t-2020.06.21-rho_A_1` = c("unif", 0.15, 0.3),
  `t-2020.06.21-rho_A_2` = c("unif", 0.15, 0.3),
  `t-2020.06.21-rho_A_3` = c("unif", 0.15, 0.3),
  `t-2020.06.21-rho_A_4` = c("unif", 0.15, 0.3),
  `t-2020.06.21-delta_IM_1` = c("unif", 0.15, 0.35),
  `t-2020.06.21-delta_IM_2` = c("unif", 0.15, 0.35),
  `t-2020.06.21-delta_IM_3` = c("unif", 0.15, 0.35),
  `t-2020.06.21-delta_IM_4` = c("unif", 0.15, 0.35),
  `t-2020.06.21-delta_IS_1` = c("unif", 0.2, 0.5),
  `t-2020.06.21-delta_IS_2` = c("unif", 0.2, 0.5),
  `t-2020.06.21-delta_IS_3` = c("unif", 0.2, 0.5),
  `t-2020.06.21-delta_IS_4` = c("unif", 0.2, 0.5),
  `t-2020.05.31-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.06.14-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.06.28-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.07.12-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.07.26-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.08.09-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.08.23-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.09.06-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.09.20-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.10.04-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.10.18-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.11.01-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.11.15-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.11.29-sd_1234` = c("unif", 0.05, 0.7),
  `t-2020.12.13-sd_1234` = c("unif", 0.05, 0.7)
)

abc_param_names = names(params_priors)

