##Run Interim
# Creation date: 3/5/2021
# Purpose: This script simulates the vaccine rollout using our current best fit parameters.
# 1. Parameters are pulled from the kc_globalparams.R file
# 2. Initial conditions are pulled from ???? (suggestions welcome)
# 3. Start date is set to <Current Date?> (maybe this can be changed

#setwd("R-proj")
library(HutchCOVID)
source("kc_read_data.R")
source("covid-model-plotting.R")
source("Set_initial_conditions/Starting_State.R")
load("../data/wa_counties_aggr.Rdata")
load("../data/wa_county_vaccines.Rdata")
load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")
load("../data/wa_counties_age1_proportions.Rdata")

# parallel processing
library(foreach)
library(doParallel)

n_cores = 5 # set appropriate cores for your system
set.seed(20)

if (n_cores > 1)
{
  registerDoParallel(cores = n_cores)
} else
{
  registerDoSEQ()
}

### Vaccine and Strain Assumptions #####
# define the baseline parameters
scenario_base = params_base
scenario_base$n_strain = 2
scenario_base$n_vaccine = 2
scenario_base$n_recovered = 1
# define vaccine efficacy assumptions
scenario_base$vep = matrix(rep(c(0, 0.34, 0.3), 2), nrow = scenario_base$n_strain, byrow = TRUE)
scenario_base$ves = matrix(rep(c(0, 0.78, 0.7), 2), nrow = scenario_base$n_strain, byrow = TRUE)
scenario_base$vei = matrix(rep(c(0, 0, 0), 2), nrow = scenario_base$n_strain, byrow = TRUE)
scenario_base$veh = matrix(rep(c(0, 0.66, 0.66), 2), nrow = scenario_base$n_strain, byrow = TRUE)
scenario_base$vac_rate = 5000
scenario_base$vac_dist = c(0.09, 0.31 ,0.23, 0.37)
scenario_base$vac_priority = 4:1
scenario_base$vac_coverage = 0
scenario_base$strain_infectivity = c(1.5, 1.5 * 1.2)
scenario_base$strain_severity = c(1.55, 1.55)
scenario_base$strain_import = matrix(0, nrow = scenario_base$n_strain, ncol = N.age.groups)
scenario_base$W = 0 * create_W_no_waning(N.age.groups, scenario_base$n_vaccine, scenario_base$n_recovered)
scenario_base$Y = create_Y_single_recovered(N.age.groups, scenario_base$n_vaccine, scenario_base$n_strain)
scenario_base$V = create_V_single_recovered(N.age.groups, scenario_base$n_vaccine, scenario_base$n_strain)
scenario_base$dynamic_sd = TRUE
scenario_base$dynamic_sd_init_date = ymd("2021-06-01")
scenario_base$dynamic_sd_func = "calculate_dynamic_sd_hosp"
scenario_base$dynamic_sd_lock_thresh = 10 * kc_pop / 100000
scenario_base$dynamic_sd_release_thresh = 5 * kc_pop / 100000
scenario_base$dynamic_sd_period = 7
scenario_base$ramp_sd = FALSE # don't need to ramp since we're starting in 2021 with set sd value and just dynamic

verify_params(scenario_base)

# need to override calibrated sd value before simulations start
scenario_temporal_base = append(params_temporal_base, list(
  list( date = ymd("2021-06-01"),
        sd = c(0.1, 0.1, 0.1, 0.2))
))

# Starting State Assumptions

#Choose a county (might break with smaller counties)
County = "King County" 
Start_Date = "2021-06-01"
current.conditions = subset(wa.current[[County]], as.numeric(ENDDT - as.Date(Start_Date)) %in% seq(-6, 0))

#Population pulled from census data
total_population = wa.counties.pop[[County]]
names(total_population) = paste0("a", 1:4)

# Age 1 breakdown
age1_proportion = age1.prop.by.county[[County]]


##############################################################################################
#Starting Conditions

#Option 1: Customize your own!
starting_conditions = list(
  total_prevalence = 0.003,
  total_population = c(a1 = 5e5, a2 = 1e6, a3 = 5e5, a4 = 2e5),
  cumulative_incidence = c(a1 = 0.30, a2 = 0.15, a3 = 0.10, a4 = 0.11),
  proportion_vaccinated = c(a1 = 0.20, a2 = 0.50, a3 = 0.70, a4 = 0.80)
)

#Option 2: Trust Mia and select by County and date
starting_conditions = get_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01"
)
#############################################################################################


###########################################################################################
# Variants and immune states

# Option1 Set manually

#Set the strains manually
strain_prevalence_frac = c("Alpha" = 0.80,
                      "Delta" = 0.20)


#Set the susceptible class initial conditions manually
vaccine_frac = with(starting_conditions,{
  list(
    "None" = (1 - cumulative_incidence) * (1 - proportion_vaccinated),
    "Vax" = proportion_vaccinated,
    "Recovered" = cumulative_incidence * (1 - proportion_vaccinated)
  )
}
)

initial_conditions = get_starting_state(scenario_base,
                                        starting_conditions,
                                        vaccine_frac,
                                        strain_prevalence_frac)


state_scenario_base = state_matrix_to_vector(initial_conditions)

# this just runs against the uncalibrated parameters to check
#out_new = run_model_by_date(scenario_base, scenario_temporal_base, state_scenario_base, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"))


# now make parameter sets for the CSTE scenarios

scenario_A = scenario_base
scenario_A$vac_coverage = 0.86 * c(age1_proportion$`age18-19`, 1, 1, 1)
scenario_A$strain_infectivity = c(1.55, 1.55 * 1.2)

scenario_B = scenario_base
scenario_B$vac_coverage = 0.86 * c(age1_proportion$`age18-19`, 1, 1, 1)
scenario_B$strain_infectivity = c(1.55, 1.55 * 1.6)

scenario_C = scenario_base
scenario_C$vac_coverage = 0.75 * c(age1_proportion$`age18-19`, 1, 1, 1)
scenario_C$strain_infectivity = c(1.55, 1.55 * 1.2)

scenario_D = scenario_base
scenario_D$vac_coverage = 0.75 * c(age1_proportion$`age18-19`, 1, 1, 1)
scenario_D$strain_infectivity = c(1.55, 1.55 * 1.6)


scenario_temporal_AB = append(scenario_temporal_base, list(
  list( date = ymd("2021-06-01"),
        vac_coverage = 0.86 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`, 1, 1, 1),
        vac_dist = c(0.25, 0.25 ,0.25, 0.25))
))

scenario_temporal_CD = append(scenario_temporal_base, list(
  list( date = ymd("2021-06-01"),
        vac_coverage = 0.75 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`, 1, 1, 1),
        vac_dist = c(0.25, 0.25 ,0.25, 0.25))
))

scenario_temporal_kids_AB = append(scenario_temporal_AB, list(
  list( date = ymd("2021-09-01"),
        vac_coverage = 0.86 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

scenario_temporal_kids_CD = append(scenario_temporal_CD, list(
  list( date = ymd("2021-08-01"),
        vac_coverage = 0.75 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

# and run the scenarios against the calibrated parameter set
scenarios = list(A = scenario_A, B = scenario_B, C = scenario_C, D = scenario_D)
scenarios_temp = list(A = scenario_temporal_AB, B = scenario_temporal_AB, C = scenario_temporal_CD, D = scenario_temporal_CD)
scenarios_temp_kids = list(A = scenario_temporal_kids_AB, B = scenario_temporal_kids_AB, C = scenario_temporal_kids_CD, D = scenario_temporal_kids_CD)

