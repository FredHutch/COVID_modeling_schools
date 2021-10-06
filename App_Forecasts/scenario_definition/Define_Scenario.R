





scenario_base = params_base
best_guess_param_set = 50

starting_conditions = get_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01"
)




#Add assumptions on social distancing
scenario_base$sd = c(0.1, 0.1, 0.1, 0.2)
scenario_base$dynamic_sd = TRUE
scenario_base$dynamic_sd_init_date = ymd("2021-06-11")
scenario_base$dynamic_sd_func = "calculate_dynamic_sd_cases"
scenario_base$dynamic_sd_period = 7



scenario.specs = list(
  strains.list = list(
    "Alpha" = list(rel.transmissability = 1.5,
                   rel.severity = 1.6,
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.80,
                   frac.cum.inc = NULL), #Necessary if you intend to have different recovered classes for each strain
    "Delta" = list(rel.transmissability = 1.5 * 1.6,
                   rel.severity = 1.6,
                   #Example of making a temporal parameter
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0)
      ),
      
      #How rapidly individuals transition to the given class (per capita daily rate, any positive number is fine, transitioning back to the same class does nothing)
      "Waning" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0)
      )
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
      "Coverage" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 1/730, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 1, "RecVax" = 0),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = c(
        c("None" = 1/730, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 1, "Recovered" = 0, "RecVax" = 0)
      )
    ),
    "RecVax" = list(
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
        "Alpha" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1),
        "Delta" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 1),
        "mRNA" = c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "SuscVax" = 1/730, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "SuscVax" = 0, "Vax" = 0, "Recovered" = 0, "RecVax" = 0)
      )
    )
  )
)




scenario.specsA = scenario.specs
scenario.specsA$immune.list$Vax$Waning["SuscVax"] = 0
scenario.specsA$immune.list$RecVax$Waning["SuscVax"] = 0
scenario.specsA$immune.list$Recovered$Waning["None"] = 0


scenario.specsB = scenario.specs
scenario.specsB$strains.list$Delta$rel.transmissability = 1.5 * 1.2


scenario.specsC = scenario.specsA
scenario.specsC$strains.list$Delta$rel.transmissability = 1.5 * 1.2


scenario.list = list(
  list(
    scenario_name = "Wane_16",
    specs = scenario.specs,
    p_base = scenario_base,
    p_temporal_base = params_temporal_base,
    starting_state = starting_conditions
  ),
  list(
    scenario_name = "NoWane_16",
    specs = scenario.specsA,
    p_base = scenario_base,
    p_temporal_base = params_temporal_base,
    starting_state = starting_conditions
  ),
  list(
    scenario_name = "Wane_12",
    specs = scenario.specsB,
    p_base = scenario_base,
    p_temporal_base = params_temporal_base,
    starting_state = starting_conditions
  ),
  list(
    scenario_name = "NoWane_12",
    specs = scenario.specsC,
    p_base = scenario_base,
    p_temporal_base = params_temporal_base,
    starting_state = starting_conditions
  )
)