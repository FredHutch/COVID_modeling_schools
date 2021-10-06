
# Define contact matrix as single age group 
contact_matrix = matrix(1, nrow = 1, ncol = 1)

# Set starting parameters for King County ####
the_pop = 2190000 

# define first, since these variables are used in the calculation of other params
params_oneage = list(
  inf_presyp = 0.44,            # Proportion of infections due to pre-symptomatic individuals
  id = 5,                       # Duration of infectiousness in days
  time_to_discharge = 25,
  time_to_hospitalization = 10
)
params_oneage <- append(params_oneage, list(
  n_age = 1,
  n_strain = 1,                            # Number of strains, 1 is original model with no additional strains
  n_vaccine = 1,                           # Number of vaccines, 1 is no vaccine
  n_recovered = 1,                         # Number of recovered states, i.e. just 1 or per strain
  gamma_1 = 0.33,                          # Progression rates from exposed (E) to infectious (A or P) (latent time)-1
  gamma_2 = 0.5,                           # Progression rates from pre-symptomatic (P) to symptomatic (I) (pre-symptomatic time)-1
  r_a = 0.2,                               # Recovery rate of the asymptomatic cases [10-15%]
  r_m = 1/params_oneage$id,                              # Recovery rate of the mild symptomatic cases [5-6%]
  r_h = 1/(params_oneage$time_to_discharge - 
             params_oneage$time_to_hospitalization),     # Recovery rate of the hospitalized cases
  p = 0.5,             # Proportion of the infections which become symptomatic by age
  init_sd_start_date = ymd("2020-03-08"),    # Day to transition diagnotic rate, start social distancing , was march 5
  init_sd_full_date = ymd("2020-03-29"),    # Day to attain full social distancing , was march 24
  init_sd_relax_date = ymd("2020-05-03"), # pick sunday
  model_day0_date = ymd("2019-12-31"),     # Day 0 of the model, so Jan 1, 2020 is day 1 etc.
  first_inf_day = 20,      # First infection (unobserved), used to align model time and calendar time
  m = 0.96,     # Proportion of symptomatic case which remain mild by age
  hfr = 0.11,   # initial hospital fatality rate calculated from KC data (March-April)
  kappa = 1,               # Relative susceptibility by age 
  beta_a1 = 0.75 * params_oneage$inf_presyp / (1 - params_oneage$inf_presyp) * params_oneage$id / 2,  # transmission rate from asymptomatic 1 relative to presymptomatic (0.75 * beta_p)
  beta_a2 = 0.75,                        # transmission rate from asymptomatic 2 relative to symptomatic 
  beta_p = params_oneage$inf_presyp / (1 - params_oneage$inf_presyp) * params_oneage$id / 2,        # transmission rate from pre-symptomatic, 2 comes from gamma_2
  beta_s = 1,                              # relative transmission rate from severe symptoms
  beta_m = 1,                              # relative transmission rate from mild symptoms
  beta_h = 0,                              # transmission rate from hospitalized
  # contact matrix        
  C = contact_matrix,
  # vaccine params, in general length needs to be n_vaccine + n_strain
  # vaccine efficacies, are a matrix with a row for each strain and a column for each vaccine (n_vaccine + n_strain)
  # note that the first vaccine corresponds to 'no vaccine' so should have a 0 efficacy
  # after the n_vaccine vaccines (for no vaccines then actual vaccines if any) there are a further n_strain vaccines 
  # for efficacy of being infected by that strain against being reinfected by that strain and others
  vep = matrix(c(0, 1), nrow = 1, ncol = 2), # effectiveness against proportion symptomatic
  ves = matrix(c(0, 1), nrow = 1, ncol = 2), # effectiveness against symptoms
  vei = matrix(c(0, 1), nrow = 1, ncol = 2), # effectiveness against transmissibility
  veh = matrix(c(0, 1), nrow = 1, ncol = 2), # efficacy against severe disease, this is also odds ratio relative to unvaccinated similar to strain severity
  vac_rate = 0,                         # daily number of vaccines distributed across all vaccines
  vac_dist = 1,          # vaccine distribution by age group
  vac_priority = 1,                      # which age group to vaccinate next in order after coverage reached
  vac_coverage = 0.8,                      # what percent of each age group to vaccinate before stopping
  # strain params, in general length needs to be n_strain
  strain_infectivity = 1,                  # 1 is same as original, 1.5 is 50 % more infective
  strain_severity = 1,                     # modifies m, also relative to original
  # strain import is a matrix with a row for each strain and column for each age
  strain_import = matrix(0, nrow = 1, ncol = N.age.groups),    # how many people to move from each age group S to strain E
  # waning parameters
  W = create_W_no_waning(1, 1, 1),
  Y = create_Y_single_recovered(1, 1, 1),
  # dynamic sd for future projections
  dynamic_sd = FALSE,                      # whether to enable dynamic sd calculation
  dynamic_sd_init_date = NULL,             # which day to start the automatic sd adjustments
  dynamic_sd_func = "calculate_dynamic_sd_cases",  # which function to use to calculate the dynamic sd, this function takes two arguments, the parameters and the sd metrics
  dynamic_sd_lock_thresh = 300 * the_pop / 100000,   # number of cases/hosp in period to increase sd
  dynamic_sd_release_thresh = 200 * the_pop / 100000, # number of cases/hosp in period to start releasing sd
  dynamic_sd_period = 14,	                 #  how often to check/adjust sd (14 days)
  dynamic_sd_delta = 0.2,                  # how much to decrease sd by during release
  dynamic_sd_min = 0.1,  # minimum sd by age
  dynamic_sd_max = 0.5,  # maximum sd by age
  ramp_sd = TRUE,                          # whether to ramp weekly (linearly) between sd temporal parameters
  # calibrated parameters (defaults)
  bstar = 0.02,               # base transmission rate
  beta_d = 0.75,              # transmission rate from diagnosed
  # default values for parameters calibrated monthly
  h = 0.15,          # Hospitalization rate among severe cases, by age
  delta_IM = 0.15,   # Tests per capita/day for those WITH symptoms MILD
  delta_IS = 0.25,   # Tests per capita/day for those WITH symptoms SEVERE
  delta_notI = 0,            # Tests per capita/day for those WITHOUT symptoms, not currently used!!
  delta_H = 0.75,                        # Diagnostic rate for hospitalized, rough approx from KC data
  rho_A = 0.2,                          # Relative likelihood of A and P to be diagnosed (multiplied by delta_I)
  sd = 0.4,        # Social distancing before any lockdown
  bfact_d = 0.6    # Social distancing for diagnosed (1 - sd), this parameter is not relaxed
))

# these are the base temporal parameters to use as a starting point to add additional changes
# the model runner code will handle reordering the temporal parameters if necessary and aggregating 
# multiple lists for the same date
params_temporal_oneage = list(
  list( date = ymd("2020-05-01"),
        # hfr from KC data for May-July
        hfr = 0.08 ),
  list( date = ymd("2020-08-02"),
        # hfr from KC data for Aug - Dec (last good data)
        hfr = 0.06 )
)

state_oneage = initial_state(params_oneage$n_age, params_oneage$n_strain, params_oneage$n_vaccine, params_oneage$n_recovered, S_init = c(the_pop - 2, 0), E_init = c(2, 0)) 

