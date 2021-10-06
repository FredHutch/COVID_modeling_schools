# these are the global variables for the abc calibration that are therefore available both for the calibration, 
# as well as the later analysis
# these are valiables like the parameter names and date ranges that are important to keep synchronized
# this is necessary because the EasyABC objective function doesn't allow the passing of additional parameters outside the parameter vector of calibrated parameters
# furthermore, to run in parallel, this file must be sources inside the function, or the necessary variables will not be present on other cores than that where the script was launched

library(HutchCOVID)
source("kc_read_data.R")
source("calibration/functions/empirical_distributions.R")

params_calib_abc = params_base
params_temporal_calib_abc = params_temporal_base

# set up for the strains/vaccines model that we will use for scenarios
params_calib_abc$n_strain = 3
params_calib_abc$n_vaccine = 2
params_calib_abc$n_recovered = 1
params_calib_abc$vep = matrix(rep(c(0, 0.34, 0.3), 3), nrow = params_calib_abc$n_strain, byrow = TRUE)
params_calib_abc$ves = matrix(c(c(0, 0.91, 0.7), c(0, 0.91, 0.7), c(0, 0.82, 0.7)), nrow = params_calib_abc$n_strain, byrow = TRUE)
params_calib_abc$vei = matrix(rep(c(0, 0, 0), 3), nrow = params_calib_abc$n_strain, byrow = TRUE)
params_calib_abc$veh = matrix(rep(c(0, 0.67, 0.67), 3), nrow = params_calib_abc$n_strain, byrow = TRUE)
params_calib_abc$vac_rate = 10000
params_calib_abc$vac_dist = c(0.15, 0.65 ,0.18, 0.02)
params_calib_abc$vac_priority = 4:1
params_calib_abc$vac_coverage = 0.85
params_calib_abc$strain_infectivity = c(1, 1.5, 1.5 * 1.6)
params_calib_abc$strain_severity = c(1, 1.5, 1.5)
params_calib_abc$strain_import = matrix(0, nrow = params_calib_abc$n_strain, ncol = params_calib_abc$n_age)
params_calib_abc$W = 0 * create_W_no_waning(params_calib_abc$n_age, params_calib_abc$n_vaccine, params_calib_abc$n_recovered)
params_calib_abc$Y = create_Y_single_recovered(params_calib_abc$n_age, params_calib_abc$n_vaccine, params_calib_abc$n_strain)
params_calib_abc$V = create_V_single_recovered(params_calib_abc$n_age, params_calib_abc$n_vaccine)
params_calib_abc$dynamic_sd = FALSE
params_calib_abc$ramp_sd = FALSE # don't need to ramp since we're starting in 2021 with set sd value 


# set initial state
load(file = "state_abc.rdata")

# source("Set_initial_conditions/Starting_State.R")
# load("../data/wa_counties_aggr.Rdata")
# load("../data/wa_county_vaccines.Rdata")
# load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")
load("../data/wa_counties_age1_proportions.Rdata")

County = "King County"
Start_Date = "2021-04-01"

# Age 1 breakdown
age1_proportion = age1.prop.by.county[[County]]
# 
# starting_conditions = get_starting_conditions_by_county_date(County, Start_Date)
# 
# 
# #Proportion of current infections by strain, please edit!
# strain_trans_frac = c("Original" = 0.399,
#                       "b.1.1.7" = 0.6,
#                       "b.1.1.617" = 0.001)
# 
# 
# #Estimate of present coverage by age group/vaccine
# vaccine_frac = with(starting_conditions,{
#   list(
#     "None" = (1 - cumulative_incidence) * (1 - proportion_vaccinated),
#     "Vax" = proportion_vaccinated,
#     "Recovered" = cumulative_incidence * (1 - proportion_vaccinated)
#   )
# }
# )
# 
# initial_conditions = get_starting_state(params_calib_abc, 
#                                         starting_conditions,
#                                         vaccine_frac,
#                                         strain_trans_frac)
# 
# state_abc = state_matrix_to_vector(initial_conditions)
# save(state_abc, file = "state_abc.rdata")

# close schools and include vaccination of 12+
params_temporal_calib_abc = append(params_temporal_calib_abc, list(
  list( date = ymd("2020-03-12"), 
        school_sd = 1),
  list( date = ymd("2021-06-01"),
        vac_coverage = 0.85 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`, 1, 1, 1),
        vac_dist = kc_age_prop)
) )

data_abc = filter(kc_data, date >= ymd("2021-04-01") & date <= ymd("2021-06-15")) 
# don't normalize by less than 1, since that will inflate error too much
data_means = list( cases = pmax( apply(data_abc[,paste0("pos", 1:4)], 2, mean), rep(1, 4)),
                   deaths = pmax( apply(data_abc[,paste0("deaths", 1:4)] + data_abc[,paste0("hosp_deaths", 1:4)], 2, mean), rep(1, 4)),
                   hosp = pmax( apply(data_abc[,paste0("hosps", 1:4)], 2, mean), rep(1, 4))) #,
# negtests = pmax( apply(data_abc[,paste0("neg", 1:4)], 2, mean), rep(1, 4)))

# previously were doing weekly, but try switching this to 3 (i.e. more like twice weekly) to have more data points, note we 
# are still using 7 day moving average to smooth out weekends/weekdays
dates_abc = c(seq(from = data_abc$date[1], to = data_abc$date[nrow(data_abc)], by = 7), data_abc$date[nrow(data_abc)]) # add the last date that is only 6 days after

summary_stats_abc = calc_data_stats(data_abc, dates_abc, data_means)

#create empirical priors, read in parameters from firstperiod noschools fit and make for bstar, beta_d, m_1-4
parameter_file = "out/parameter_sets/params_out_firstperiod_noschools_rej.csv"
param_set = read.csv(parameter_file)

bstar_pdf = empirical_pdf(param_set$bstar)
bstar_gen = empirical_generator(param_set$bstar)

beta_d_pdf = empirical_pdf(param_set$beta_d)
beta_d_gen = empirical_generator(param_set$beta_d)

m_1_pdf = empirical_pdf(param_set$m_1)
m_1_gen = empirical_generator(param_set$m_1)

m_2_pdf = empirical_pdf(param_set$m_2)
m_2_gen = empirical_generator(param_set$m_2)

m_3_pdf = empirical_pdf(param_set$m_3)
m_3_gen = empirical_generator(param_set$m_3)

m_4_pdf = empirical_pdf(param_set$m_4)
m_4_gen = empirical_generator(param_set$m_4)

params_priors = list(
  bstar = list(c("bstar_gen", 1), c("bstar_pdf")),
  beta_d = list(c("beta_d_gen", 1), c("beta_d_pdf")),
  `t-2021.03.31-sd_1` = c("unif", 0.05, 0.7),
  `t-2021.03.31-sd_2` = c("unif", 0.05, 0.7),
  `t-2021.03.31-sd_3` = c("unif", 0.05, 0.7),
  `t-2021.03.31-sd_4` = c("unif", 0.05, 0.7),
  `t-2021.04.25-sd_1` = c("unif", 0.05, 0.7),
  `t-2021.04.25-sd_2` = c("unif", 0.05, 0.7),
  `t-2021.04.25-sd_3` = c("unif", 0.05, 0.7),
  `t-2021.04.25-sd_4` = c("unif", 0.05, 0.7),
  delta_IM_1 = c("unif", 0.15, 0.35), 
  delta_IM_2 = c("unif", 0.15, 0.35),
  delta_IM_3 = c("unif", 0.15, 0.35),
  delta_IM_4 = c("unif", 0.15, 0.35),
  delta_IS_1 = c("unif", 0.2, 0.5),
  delta_IS_2 = c("unif", 0.2, 0.5),
  delta_IS_3 = c("unif", 0.2, 0.5),
  delta_IS_4 = c("unif", 0.2, 0.5),
  rho_A_1 = c("unif", 0.15, 0.3),
  rho_A_2 = c("unif", 0.15, 0.3),
  rho_A_3 = c("unif", 0.15, 0.3),
  rho_A_4 = c("unif", 0.15, 0.3),
  m_1 = list(c("m_1_gen", 1), c("m_1_pdf")),
  m_2 = list(c("m_2_gen", 1), c("m_2_pdf")),
  m_3 = list(c("m_3_gen", 1), c("m_3_pdf")),
  m_4 = list(c("m_4_gen", 1), c("m_4_pdf"))
)

abc_param_names = names(params_priors)
