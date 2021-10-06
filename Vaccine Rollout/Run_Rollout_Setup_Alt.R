####Alternative Run Rollout Setup####

#Author: Mia Moore
#Date: 7/21/2021
#Purpose: Define scenario with waning that prevents revaccination

#output:
# scenarios.specs

#Contains the following immune states:

#None: Never vaccinated or infected
#Vax: Vaccinated and still protected, both with and without prior infection
#Recovered: Never vaccinated, but previously infected
#Recovered_Vax: Vaccinated but without protection due to waning, with prior infection
#None_Vax: Vaccinated but without protection due to waning


scenario.specs = list(
  strains.list = list(
    "Alpha" = list(rel.transmissability = 1.5,
                   rel.severity = 1.55,
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.80,
                   frac.cum.inc = NULL), #Necessary if you intend to have different recovered classes for each strain
    "Delta" = list(rel.transmissability = 1.5 * 1.6,
                   rel.severity = 1.55,
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
        "Alpha" = c("None" = 0, "Vax" = 0, "Recovered" = 1, "Recovered_Vax" = 0, "None_Vax" = 0),
        "Delta" = c("None" = 0, "Vax" = 0, "Recovered" = 1, "Recovered_Vax" = 0, "None_Vax" = 0),
        "mRNA" = c("None" = 0, "Vax" = 1, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      
      #How rapidly individuals transition to the given class (per capita daily rate, any positive number is fine, transitioning back to the same class does nothing)
      "Waning" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 1, "Recovered" = 0, "Recovered_Vax" = 1, "None_Vax" = 1)
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
        "Alpha" = c("None" = 0, "Vax" = 1, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0),
        "Delta" = c("None" = 0, "Vax" = 1, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0),
        "mRNA" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Waning" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 1/730)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
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
        "Alpha" = c("None" = 0, "Vax" = 0, "Recovered" = 1, "Recovered_Vax" = 0, "None_Vax" = 0),
        "Delta" = c("None" = 0, "Vax" = 0, "Recovered" = 1, "Recovered_Vax" = 0, "None_Vax" = 0),
        "mRNA" = c("None" = 0, "Vax" = 1, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Waning" = c(
        c("None" = 1/730, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 1, "Recovered" = 0, "Recovered_Vax" = 1, "None_Vax" = 1)
      )
    ),
    "Recovered_Vax" = list(
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
        "Alpha" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 1, "None_Vax" = 0),
        "Delta" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 1, "None_Vax" = 0),
        "mRNA" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Waning" = c(
        c("None" = 1/730, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      )
    ),
    "None_Vax" = list(
      "VEsusc" = list(
        "Alpha" = 0, #Note that these have to match the strains listed above
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
        "Alpha" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 1, "None_Vax" = 0),
        "Delta" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 1, "None_Vax" = 0),
        "mRNA" = c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      
      #How rapidly individuals transition to the given class (per capita daily rate, any positive number is fine, transitioning back to the same class does nothing)
      "Waning" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0, "Recovered_Vax" = 0, "None_Vax" = 0)
      )
    )
  )
)