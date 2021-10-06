
#Import contact matrices and rebalance to ensure reciprocity
#source('contact_matrix_construct.R')
source('construct_contact_matrix.R')
# Set starting parameters for King County ####
kc_pop = 2190000 #2118119                         # King County population
kc_age_prop = c(0.2293,	0.4552,	0.2350,	0.0805)   # King County age distribution

contact_matrix_list = constuct_contact_matrix("King County")
#Imperial College Study Rate
#hospitalization.rate.by.decade = c(0, 0.04, 1.04, 3.43, 4.25, 8.16, 11.8, 16.6, 18.4)/100
#death.rate.by.decade = c(0.00161, 0.00695, 0.0309, 0.0844, 0.161, 0.595, 1.93, 4.28, 7.8)/100

#hospitalization.per.infection = age.groupings%*%(age.distribution.by.decade*hospitalization.rate.by.decade)
#deaths.per.infection = age.groupings%*%(age.distribution.by.decade*death.rate.by.decade)

#deaths.given.hospitalization = deaths.per.infection/hospitalization.per.infection

# these are the 1st of month dates for monthly fitted parameters, starting during lockdown then every month from may
monthly_param_dates = ymd(c("2020-01-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01", "2020-10-01"))

# define first, since these variables are used in the calculation of other params
params_base = list(
  n_age = 4,
  inf_presyp = 0.44,            # Proportion of infections due to pre-symptomatic individuals
  id = 5,                       # Duration of infectiousness in days
  time_to_discharge = 25,
  time_to_hospitalization = 10,
  sd_def = c(0.65, 0.65, 0.65, 0.8)       # Social distancing to decrease transmission rate
)
params_base <- append(params_base, list(
  n_strain = 1,                            # Number of strains, 1 is original model with no additional strains
  n_vaccine = 1,                           # Number of vaccines, 1 is no vaccine
  n_recovered = 1,                         # Number of recovered states, i.e. just 1 or per strain
  gamma_1 = 0.33,                          # Progression rates from exposed (E) to infectious (A or P) (latent time)-1
  gamma_2 = 0.5,                           # Progression rates from pre-symptomatic (P) to symptomatic (I) (pre-symptomatic time)-1
  r_a = 0.2,                               # Recovery rate of the asymptomatic cases [10-15%]
  r_m = 1/params_base$id,                              # Recovery rate of the mild symptomatic cases [5-6%]
  r_h = 1/(params_base$time_to_discharge - 
             params_base$time_to_hospitalization),     # Recovery rate of the hospitalized cases
  p = c(0.25, 0.33, 0.55, 0.70),             # Proportion of the infections which become symptomatic by age (From Davies)
  init_sd_start_date = ymd("2020-03-08"),    # Day to transition diagnotic rate, start social distancing , was march 5
  init_sd_full_date = ymd("2020-03-29"),    # Day to attain full social distancing , was march 24
  init_sd_relax_date = ymd("2020-05-03"), # pick sunday
  model_day0_date = ymd("2019-12-31"),     # Day 0 of the model, so Jan 1, 2020 is day 1 etc.
  first_inf_day = 20,      # First infection (unobserved), used to align model time and calendar time
  m = c(0.998, 0.973, 0.931, 0.829),     # Proportion of symptomatic case which remain mild by age (From Sero MCMC)
  hfr = c(0.019, 0.047, 0.194, 0.488),   # initial hospital fatality rate calculated from KC data (March-April)
  kappa = c(0.5, 1, 1, 1),               # Relative susceptibility by age 
  beta_a1 = 0.75 * params_base$inf_presyp / (1 - params_base$inf_presyp) * params_base$id / 2,  # transmission rate from asymptomatic 1 relative to presymptomatic (0.75 * beta_p)
  beta_a2 = 0.75,                        # transmission rate from asymptomatic 2 relative to symptomatic 
  beta_p = params_base$inf_presyp / (1 - params_base$inf_presyp) * params_base$id / 2,        # transmission rate from pre-symptomatic, 2 comes from gamma_2
  beta_s = 1,                              # relative transmission rate from severe symptoms
  beta_m = 1,                              # relative transmission rate from mild symptoms
  beta_h = 0,                              # transmission rate from hospitalized
  # contact matrix        
  C.work = contact_matrix_list$C.work,                         # Contact matrix (work)
  C.home = contact_matrix_list$C.home,                         # Contact matrix (home)
  C.school = contact_matrix_list$C.school,                     # Contact matrix (school)
  C.other = contact_matrix_list$C.other,                       # Contact matrix (other)
  # vaccine params, in general length needs to be n_vaccine + n_strain
  # vaccine efficacies, are a matrix with a row for each strain and a column for each vaccine (n_vaccine + n_strain)
  # note that the first vaccine corresponds to 'no vaccine' so should have a 0 efficacy
  # after the n_vaccine vaccines (for no vaccines then actual vaccines if any) there are a further n_strain vaccines 
  # for efficacy of being infected by that strain against being reinfected by that strain and others
  vep = matrix(c(0, 1), nrow = 1, ncol = 2), # effectiveness against proportion symptomatic
  ves = matrix(c(0, 1), nrow = 1, ncol = 2), # effectiveness against symptoms
  vei = matrix(c(0, 1), nrow = 1, ncol = 2), # effectiveness against transmissibility
  veh = matrix(c(0, 0.66), nrow = 1, ncol = 2), # efficacy against severe disease, this is also odds ratio relative to unvaccinated similar to strain severity
  vac_rate = 0,                         # daily number of vaccines distributed across all vaccines
  vac_dist = kc_age_prop,          # vaccine distribution by age group
  vac_priority = 4:1,                      # which age group to vaccinate next in order after coverage reached
  vac_coverage = 0.8,                      # what percent of each age group to vaccinate before stopping
  # strain params, in general length needs to be n_strain
  strain_infectivity = 1,                  # 1 is same as original, 1.5 is 50 % more infective
  strain_severity = 1,                     # modifies m, odds ratio relative to original, so (1 - m_strain) / m_strain = severity * (1 - m) / m
  # strain import is a matrix with a row for each strain and column for each age
  strain_import = matrix(0, nrow = 1, ncol = params_base$n_age),    # how many people to move from each age group S to strain E
  # waning parameters
  W = create_W_no_waning(params_base$n_age, 1, 1),
  Y = create_Y_single_recovered(params_base$n_age, 1, 1),
  # dynamic sd for future projections
  # king county indicators
  # cases per 100K per 14 days: phase 1: > 350, phase 2: 200-350, phase 1: <200
  # hosp per 100K per 7 days: phase 1: > 10, phase 2: 5-10, phase 1: <5
  dynamic_sd = FALSE,                      # whether to enable dynamic sd calculation
  dynamic_sd_init_date = NULL,             # which day to start the automatic sd adjustments
  dynamic_sd_func = "calculate_dynamic_sd_cases",  # which function to use to calculate the dynamic sd, this function takes two arguments, the parameters and the sd metrics
  dynamic_sd_lock_thresh = 300 * kc_pop / 100000,   # number of cases/hosp in period to increase sd
  dynamic_sd_release_thresh = 200 * kc_pop / 100000, # number of cases/hosp in period to start releasing sd
  dynamic_sd_period = 14,	                 #  how often to check/adjust sd (14 days)
  dynamic_sd_delta = 0.1,                  # how much to decrease sd by during release
  dynamic_sd_min = c(0.1, 0.1, 0.1, 0.2),  # minimum sd by age
  dynamic_sd_max = c(0.45, 0.45, 0.45, 0.7),  # maximum sd by age
  ramp_sd = TRUE,                          # whether to ramp weekly (linearly) between sd temporal parameters
  # calibrated parameters (defaults)
  bstar = 0.02,               # base transmission rate
  beta_d = 0.75,              # transmission rate from diagnosed
  # default values for parameters calibrated monthly
  h = c(0.15, 0.15, 0.15, 0.15),          # Hospitalization rate among severe cases, by age
  delta_IM = c(0.15, 0.15, 0.15, 0.15),   # Tests per capita/day for those WITH symptoms MILD
  delta_IS = c(0.3, 0.2, 0.2, 0.3),   # Tests per capita/day for those WITH symptoms SEVERE
  delta_notI = c(0, 0, 0, 0),            # Tests per capita/day for those WITHOUT symptoms, not currently used!!
  delta_H = 0.75,                        # Diagnostic rate for hospitalized, rough approx from KC data
  rho_A = c(0.2, 0.2, 0.2, 0.2),                          # Relative likelihood of A and P to be diagnosed (multiplied by delta_I)
  # sd that is per age 
  sd = c(0.4, 0.4, 0.4, 0.4),        # Social distancing before any lockdown
  # location specific sd, this can be turned on or off with the temporal parameters but is not age-specific, this is *in addition* to sd
  work_sd = 0,                         # Contact matrix (work)
  home_sd = 0,                         # Contact matrix (home)
  school_sd = 0,                     # Contact matrix (school)
  other_sd = 0,                       # Contact matrix (other)
  bfact_d = c(0.6, 0.6, 0.6, 0.6)    # Social distancing for diagnosed (1 - sd), this parameter is not relaxed
))

# these are the base temporal parameters to use as a starting point to add additional changes
# the model runner code will handle reordering the temporal parameters if necessary and aggregating 
# multiple lists for the same date
params_temporal_base = list(
  list( date = ymd("2020-05-01"),
        # hfr from KC data for May-July
        hfr = c(0.012, 0.032, 0.138, 0.389) ),
  list( date = ymd("2020-08-02"),
        # hfr from KC data for Aug - Dec (last good data)
        hfr = c(0.009, 0.024, 0.105, 0.318) )
)


state = initial_state(params_base$n_age, params_base$n_strain, params_base$n_vaccine, params_base$n_recovered) 

