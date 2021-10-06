##Run Interim
# Creation date: 3/5/2021
# Purpose: This script runs
# 1. Parameters are pulled from the kc_globalparams.R file
# 2. Initial conditions are pulled from hospitalization data

#Shift to needed directory
cwd = getwd()
containing_dir = strsplit(cwd, "COVID_modeling", fixed = T)[[1]][1]
setwd(paste0(containing_dir, "COVID_modeling/R-proj"))
library(HutchCOVID)
library(tidyr)
library(dplyr)
source("kc_globalparams.R")
source("Set_initial_conditions/Starting_State.R")
source("Set_initial_conditions/Back_Calculate_Infections.R")
load("../data/wa_counties_aggr.Rdata") #Load Populations
load("../data/wa_county_vaccines.Rdata") #Load Vaccinations

#Shift back to where you started
setwd(cwd)

########################################################################################
#############Parameters######################
########################################################################################


##########################################
#Basic Parameters
# For more info on setting parameters, see the version 2 readme (under R-proj)
#Set baseline parameters
scenario_base = params_base

#Add assumptions on social distancing
scenario_base$sd = c(0.1, 0.1, 0.1, 0.2)
scenario_base$dynamic_sd = TRUE
scenario_base$dynamic_sd_init_date = ymd("2021-06-11")
scenario_base$dynamic_sd_func = "calculate_dynamic_sd_hosp"
scenario_base$dynamic_sd_lock_thresh = 10 * kc_pop / 100000
scenario_base$dynamic_sd_release_thresh = 5 * kc_pop / 100000
scenario_base$dynamic_sd_period = 7
##########################################




############################################
#Vaccines and strains parameters

scenario.specs = list(
  strains.list = list(
    "Alpha" = list(rel.transmissability = 1.5,
                   rel.severity = list("base" = 1.5, "2021-06-15" = 1.6),
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.80,
                   frac.cum.inc = NULL), #Necessary if you intend to have different recovered classes for each strain
    "Delta" = list(rel.transmissability = 1.5 * 1.6,
                   #You could make severity change over time for some reason
                   #You probably shouldn't but you technically can
                   rel.severity = list("base" = 1.2, "2021-06-11" = 1.6),
                   #Example of making a temporal parameter
                   import.rate = list("base" = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                                      "2021-06-15" = c("a1" = 0, "a2" = 1, "a3" = 0, "a4" = 0),
                                      "2021-07-29" = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0)),
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0)
      ),
      
      #How rapidly individuals transition to the given class (per capita daily rate, any positive number is fine, transitioning back to the same class does nothing)
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      
      "Coverage" = c("None" = 0, "SuscVax" = 1, "Vax" = 1, "Recovered" = 0, "RecVax" = 1)
    ),
    "SuscVax" = list(
      "VEsusc" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEsymp" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "VEhosp" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      "Fate" = list(
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = list(
        "None" = 0, "SuscVax" = c(0,0,0,1/730), "Vax" = 0, "Recovered" = 0, "RecVax" = 0
      ),
      "Coverage" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = c(
        c("None" = 1/730, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c("None" = 0, "SuscVax" = 1, "Vax" = 1, "Recovered" = 0, "RecVax" = 1)
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 1/730, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
    )
  )
)

scenario.params = get_parameters_from_specs(scenario.specs, scenario_base, params_temporal_base)

scenario_base = scenario.params$params
scenario_temporal = scenario.params$params_temporal
verify_params(scenario_base)



##############################################################################################################
####Temporal Parameters#####

#Can add additional temporal parameters if you want
scenario_temporal = append(scenario_temporal, list(
  list( date = ymd("2021-03-01"),
        vac_rate = 5000,
        strain_import = matrix(c(c(5, 5, 5, 5), c(0, 0, 0, 0)),
                               nrow = scenario_base$n_strain, ncol = 4, byrow = TRUE)),
  list( date = ymd("2021-04-04"),
        vac_rate = 10000,
        strain_import = matrix(c(c(5, 5, 5, 5), c(5, 5, 5, 5)),

                               nrow = scenario_base$n_strain, ncol = 4, byrow = TRUE))
))
##############################################################################################################


##############################################################################################################
################################### Starting Conditions###################################
##############################################################################################################


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
starting_conditions = generate_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01"
)

#Option 3: Force initial conditions to use selected parameters during calculation
starting_conditions = generate_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01",
  parameters = scenario_base
)


#Option 4: Generate multiple realizations of the initial conditions (caution: will return a list)
starting_conditions = generate_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01",
  N = 2
)


#Option 5: Account for uncertainty due to low numbers of hospitalizations (in development)
starting_conditions = generate_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01",
  Resample = T
)
#############################################################################################


###########################################################################################

initial_conditions = get_starting_state_from_scenario(scenario_base, 
                                                      starting_conditions, 
                                                      scenario.specs)

state_scenario_base = state_matrix_to_vector(initial_conditions)

# this just runs against the uncalibrated parameters to check
out_new = run_model_by_date(scenario_base, scenario_temporal, state_scenario_base, start_date = ymd("2021-06-01"), end_date = ymd("2022-04-01"))


out_long = data.table::data.table(shape_data_long(out_new, params_base$model_day0_date))


