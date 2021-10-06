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

n_cores = 10 # set appropriate cores for your system
set.seed(20)

if (n_cores > 1)
{
  registerDoParallel(cores = n_cores)
} else
{
  registerDoSEQ()
}

# these come from kc_globalparams
scenario_base = params_base
scenario_temporal_base = params_temporal_base
                                
#New option (from Mia as of 7/14): Use the following configuration object
# It has a flexible structure but should ensure consistency between initial conditions and parameters
scenario.specs = list(
  strains.list = list(
    "Alpha" = list(rel.transmissability = 1.5,
                   rel.severity = 1.5,
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.80,
                   frac.cum.inc = NULL), 
    "Delta" = list(rel.transmissability = 1.5 * 1.6,
                   rel.severity = 1.5,
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.20,
                   frac.cum.inc = NULL)
  ),
  
  #List all vaccines and the percentage of doses the represent
  vaccination.list = list(
    #Could potentially put vaccination rates and distribution here
    "mRNA" = list(
      fraction.of.vaccines = 1
    )
  ),
  
  #List all immune classes here
  
  immune.list = list(
    "None" = list(
      #Vaccine efficacy against susceptibility
      "VEsusc" = list(
        "Alpha" = 0, #Note that these have to match the strains listed above
        "Delta" = 0
      ),
      #Vaccine efficacy against symptoms
      "VEsymp" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      #Vaccine efficacy against infectiousness
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      #Vaccine efficacy against hospitalization
      "VEhosp" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      
      #What happens to people in this class after infection (each must sum to 1) and vaccination
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0, "WanedRec" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0, "WanedRec" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      ),
      
      #How rapidly individuals transition to the given class (per capita daily rate, any positive number is fine, transitioning back to the same class does nothing)
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      )
    ),
    "SuscVax" = list(
      "VEsusc" = list(
        "Alpha" = 0.46,
        "Delta" = 0.41
      ),
      "VEsymp" = list(
        "Alpha" = 0.34,
        "Delta" = 0.34
      ),
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEhosp" = list(
        "Alpha" = 0.95,
        "Delta" = 0.90
      ),
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1, "WanedRec" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1, "WanedRec" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      )
    ),
    "Vax" = list(
      "VEsusc" = list(
        "Alpha" = 0.91,
        "Delta" = 0.82
      ),
      "VEsymp" = list(
        "Alpha" = 0.34,
        "Delta" = 0.34
      ),
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEhosp" = list(
        "Alpha" = 0.67,
        "Delta" = 0.67
      ),
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 1/365, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      )
    ),
    "Recovered" = list(
      "VEsusc" = list(
        "Alpha" = .70,
        "Delta" = .70
      ),
      "VEsymp" = list(
        "Alpha" = 0.30,
        "Delta" = 0.30
      ),
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEhosp" = list(
        "Alpha" = 0.67,
        "Delta" = 0.67
      ),
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0, "WanedRec" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0, "WanedRec" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 1/365)
      )
    ),
    "RecVax" = list(
      "VEsusc" = list(
        "Alpha" = 0.70,
        "Delta" = 0.70
      ),
      "VEsymp" = list(
        "Alpha" = 0.34,
        "Delta" = 0.34
      ),
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEhosp" = list(
        "Alpha" = 0.67,
        "Delta" = 0.67
      ),
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1, "WanedRec" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1, "WanedRec" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 1/365, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      )
    ),
    "WanedRec" = list(
      "VEsusc" = list(
        "Alpha" = 0.35,
        "Delta" = 0.35
      ),
      "VEsymp" = list(
        "Alpha" = 0.34,
        "Delta" = 0.34
      ),
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEhosp" = list(
        "Alpha" = 0.90,
        "Delta" = 0.85
      ),
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0, "WanedRec" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0, "WanedRec" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0, "WanedRec" = 0)
      )
    )
  )
)

scenario.params = get_parameters_from_specs(scenario.specs, scenario_base, params_temporal_base)

scenario_base = scenario.params$params

# these parameters get different values from the call to "get_parameters_from_specs" and must be reset here
scenario_base$vac_rate = 5000
scenario_base$vac_dist = kc_age_prop
scenario_base$vac_priority = 4:1
scenario_base$dynamic_sd = TRUE
scenario_base$dynamic_sd_init_date = ymd("2021-06-01")
scenario_base$dynamic_sd_min = c(0.10, 0.10, 0.10, 0.2)
scenario_base$dynamic_sd_max = c(0.3, 0.3, 0.3, 0.5)
scenario_base$dynamic_sd_func = "calculate_dynamic_sd_hosp"
scenario_base$dynamic_sd_lock_thresh = 10 * kc_pop / 100000
scenario_base$dynamic_sd_release_thresh = 5 * kc_pop / 100000
scenario_base$dynamic_sd_period = 7
scenario_base$dynamic_sd_delta = 0.10                  # how much to decrease sd by during release
scenario_base$ramp_sd = FALSE # don't need to ramp since we're starting in 2021 with set sd value and just dynamic

verify_params(scenario_base)

# sd value will be replaced with calibrated sd value before simulations start

#Choose a county (might break with smaller counties)
County = "King County" 
Start_Date = "2021-06-01"
current.conditions = subset(wa.current[[County]], as.numeric(ENDDT - as.Date(Start_Date)) %in% seq(-6, 0))

#Population pulled from census data
total_population = wa.counties.pop[[County]]
names(total_population) = paste0("a", 1:4)

# Age 1 breakdown
age1_proportion = age1.prop.by.county[[County]]

#Cumulative incidence crudely estimated from hospitalizations
cumulative_incidence = current.conditions$CUM.INCIDENCE
names(cumulative_incidence) = paste0("a", 1:4)

#Option 2: Trust Mia and select by County and date
starting_conditions = get_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01"
)
proportion_vaccinated = starting_conditions[["proportion_vaccinated"]]

initial_conditions = get_starting_state_from_scenario(scenario_base, 
                                                      starting_conditions, 
                                                      scenario.specs)
state_scenario_base = state_matrix_to_vector(initial_conditions)

# this just runs against the uncalibrated parameters to check
#out_new = run_model_by_date(scenario_base, scenario_temporal_base, state_scenario_base, start_date = ymd("2021-06-01"), end_date = ymd("2022-06-01"))


# now make parameter sets for the four coverage scenarios!

scenario_A = scenario_base
scenario_A$vac_coverage = 0.80 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`, 1, 1, 1)

scenario_B = scenario_base
scenario_B$vac_coverage = 0.85 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`, 1, 1, 1)

scenario_C = scenario_base
scenario_C$vac_coverage = 0.90 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15`, 1, 1, 1)


scenario_temporal_A = scenario_temporal_base
scenario_temporal_B = scenario_temporal_base
scenario_temporal_C = scenario_temporal_base


scenario_temporal_kids_A = append(scenario_temporal_A, list(
  list( date = ymd("2021-10-01"),
        vac_coverage = 0.80 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

scenario_temporal_kids_B = append(scenario_temporal_B, list(
  list( date = ymd("2021-10-01"),
        vac_coverage = 0.85 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

scenario_temporal_kids_C = append(scenario_temporal_C, list(
  list( date = ymd("2021-10-01"),
        vac_coverage = 0.90 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

scenario_temporal_kids2_A = append(scenario_temporal_A, list(
  list( date = ymd("2022-01-01"),
        vac_coverage = 0.80 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

scenario_temporal_kids2_B = append(scenario_temporal_B, list(
  list( date = ymd("2022-01-01"),
        vac_coverage = 0.85 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

scenario_temporal_kids2_C = append(scenario_temporal_C, list(
  list( date = ymd("2022-01-01"),
        vac_coverage = 0.90 * c(age1_proportion$`age18-19` + age1_proportion$`age16-17` + age1_proportion$`age12-15` + age1_proportion$`age5-11`, 1, 1, 1))
))

# and run the scenarios against the calibrated parameter set
scenarios = list(A = scenario_A, B = scenario_B, C = scenario_C)
scenarios_temp = list(A = scenario_temporal_A, B = scenario_temporal_B, C = scenario_temporal_C)
scenarios_temp_kids = list(A = scenario_temporal_kids_A, B = scenario_temporal_kids_B, C = scenario_temporal_kids_C)
scenarios_temp_kids2 = list(A = scenario_temporal_kids2_A, B = scenario_temporal_kids2_B, C = scenario_temporal_kids2_C)
