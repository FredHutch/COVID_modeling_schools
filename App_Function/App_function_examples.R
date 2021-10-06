run.specs = list(
  County = "King County", #pick from list?
  #List all strains here
  strains.list = list(
    "Alpha" = list(rel.transmissability = 1.5,
                   rel.severity = 1.5,
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.80,
                   frac.cum.inc = NULL), #Necessary if you intend to have different recovered classes for each strain
    "Delta" = list(rel.transmissability = 1.5 * 1.6,
                   rel.severity = 1.5,
                   import.rate = c("a1" = 0, "a2" = 0, "a3" = 0, "a4" = 0),
                   frac.prevalence = 0.20,
                   frac.cum.inc = NULL)
  ),
  
  
  vaccination.list = list(
    #Could potentially put vaccination rates and distribution here
    "mRNA" = list(
      fraction.of.vaccines = 1
    )
  ),
  
  
    
  #List all immune classes here
  
  immune.list = list(
    "None" = list(
      immune.type = "naive", #Will just be calculated based on what's left over
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
      #Vaccine effecicay against infectiousness
      "VEinf" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      #Vaccine efficacy against hospitalization
      "VEhosp" = list(
        "Alpha" = 0,
        "Delta" = 0
      ),
      
      #What happens to people in this class after infection (each must sum to 1)
      "Fate" = list(
        "Alpha" = c("None" = 0, "Vax" = 0, "Recovered" = 1),
        "Delta" = c("None" = 0, "Vax" = 0, "Recovered" = 1),
        "mRNA" = c("None" = 0, "Vax" = 1, "Recovered" = 0)
      ),
      
      #How rapidly individuals transition to the given class (per capita daily rate, any positive number is fine, transitioning back to the same class does nothing)
      "Waning" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 1, "Recovered" = 0)
      )
    ),
    "Vax" = list(
      immune.type = "vaccine",
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
        "Alpha" = c("None" = 0, "Vax" = 1, "Recovered" = 0),
        "Delta" = c("None" = 0, "Vax" = 1, "Recovered" = 0),
        "mRNA" = c("None" = 0, "Vax" = 0, "Recovered" = 0)
      ),
      "Waning" = c(
        c("None" = 1/730, "Vax" = 0, "Recovered" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0)
      )
    ),
    "Recovered" = list(
      immune.type = "recovered", #Calculate from prior infections
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
        "Alpha" = c("None" = 0, "Vax" = 0, "Recovered" = 1),
        "Delta" = c("None" = 0, "Vax" = 0, "Recovered" = 1),
        "mRNA" = c("None" = 0, "Vax" = 1, "Recovered" = 0)
      ),
      "Waning" = c(
        c("None" = 1/730, "Vax" = 0, "Recovered" = 0)
      ),
      "Coverage" = c(
        c("None" = 0, "Vax" = 0, "Recovered" = 0)
      )
    )
  ),
  
  dynamic.sd.parameters = list(
    dynamic.func = "calculate_dynamic_sd_hosp", #choose from list
    lock.threshold = 10/100000,
    release.threshold = 5/100000,
    period = 7,
    sd.min = c("a1" = 0.1, "a2" = 0.1, "a3" = 0.1, "a4" = 0.2),
    sd.max = c("a1" = 0.45, "a2" = 0.45, "a3" = 0.45, "a4" = 0.7)
  ),
  
  vaccination.parameters = list(
    rate.per.day = 3000,
    distribution.by.age = c("a1" = 0.20, "a2" = 0.6, "a3" = 0.15, "a4" = 0.05),
    max.coverage = c("a1" = 0.75, "a2" = 0.75, "a3" = 0.90, "a4" = 0.90)
  )
  

)





# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# col.strain  = c("Original" = "black", "b.1.617.2" = "indianred", "b.1.1.7" = "skyblue")
# 
# col.immune  = c("None" = "black", "mRNA" = "indianred", "Recovered" = "skyblue")
# 
# col.age  = c("indianred", "skyblue", "seagreen", "goldenrod")
# 
# projection_parameters = list(
#   coverage = 0.86,
#   delta_infectivity = 1.6,
#   alpha_infectivity = 1.55,
#   sd_min = 0.1,
#   sd_min_senior = 0.2,
#   VE_h = 0.67,
#   wane_R = 0,
#   wane_V = 0,
#   stop_date = "2022-12-31"
# )
# 
# 
# out1 = Run_Projection(projection_parameters)
# par(mfrow = c(1, 3))
# plot(V1~date,out1$by.strain, xlab = "date", ylab = "hospitalizations", col = col.strain[strain], pch = 16)
# legend("topright", legend = names(col.strain), col = col.strain, pch = 16)
# 
# plot(V1~date,out1$by.immune, xlab = "date", ylab = "hospitalizations", col = col.immune[vac], pch = 16)
# legend("topright", legend = names(col.immune), col = col.immune, pch = 16)
# 
# plot(V1~date,out1$by.age, xlab = "date", ylab = "hospitalizations", col = col.age[age], pch = 16)
# legend("topright", legend = c("0-19", "20-49", "50-69", "70+"), col = col.age, pch = 16)
# 
# 
# projection_parameters2 = list(
#   coverage = 0.86,
#   delta_infectivity = 1.6,
#   alpha_infectivity = 1.55,
#   sd_min = 0.1,
#   sd_min_senior = 0.2,
#   VE_h = 0.67,
#   wane_R = 1/730,
#   wane_V = 1/730,
#   stop_date = "2022-12-31"
# )
# 
# 
# out2 = Run_Projection(projection_parameters2)
# plot(V1~date,out2$by.strain, xlab = "date", ylab = "hospitalizations", col = col.strain[strain], pch = 16)
# legend("topright", legend = names(col.strain), col = col.strain, pch = 16)
# 
# plot(V1~date,out2$by.immune, xlab = "date", ylab = "hospitalizations", col = col.immune[vac], pch = 16)
# legend("topright", legend = names(col.immune), col = col.immune, pch = 16)
# 
# plot(V1~date,out2$by.age, xlab = "date", ylab = "hospitalizations", col = col.age[age], pch = 16)
# legend("topright", legend = c("0-19", "20-49", "50-69", "70+"), col = col.age, pch = 16)
# 
# 
# projection_parameters3 = list(
#   coverage = 0.86,
#   delta_infectivity = 1.6,
#   alpha_infectivity = 1.2,
#   sd_min = 0.1,
#   sd_min_senior = 0.2,
#   VE_h = 0.67,
#   wane_R = 0,
#   wane_V = 0,
#   stop_date = "2022-12-31"
# )
# 
# out3 = Run_Projection(projection_parameters3)
# plot(V1~date,out3$by.strain, xlab = "date", ylab = "hospitalizations", col = col.strain[strain], pch = 16)
# legend("topright", legend = names(col.strain), col = col.strain, pch = 16)
# 
# plot(V1~date,out3$by.immune, xlab = "date", ylab = "hospitalizations", col = col.immune[vac], pch = 16)
# legend("topright", legend = names(col.immune), col = col.immune, pch = 16)
# 
# plot(V1~date,out3$by.age, xlab = "date", ylab = "hospitalizations", col = col.age[age], pch = 16)
# legend("topright", legend = c("0-19", "20-49", "50-69", "70+"), col = col.age, pch = 16)
# 
# projection_parameters4 = list(
#   coverage = 0.75,
#   delta_infectivity = 1.6,
#   alpha_infectivity = 1.55,
#   sd_min = 0.1,
#   sd_min_senior = 0.2,
#   VE_h = 0.67,
#   wane_R = 1/730,
#   wane_V = 1/730,
#   stop_date = "2022-12-31"
# )
# out4 = Run_Projection(projection_parameters4)
# plot(V1~date,out4$by.strain, xlab = "date", ylab = "hospitalizations", col = col.strain[strain], pch = 16)
# legend("topright", legend = names(col.strain), col = col.strain, pch = 16)
# 
# plot(V1~date,out4$by.immune, xlab = "date", ylab = "hospitalizations", col = col.immune[vac], pch = 16)
# legend("topright", legend = names(col.immune), col = col.immune, pch = 16)
# 
# plot(V1~date,out4$by.age, xlab = "date", ylab = "hospitalizations", col = col.age[age], pch = 16)
# legend("topright", legend = c("0-19", "20-49", "50-69", "70+"), col = col.age, pch = 16)
# 
# 
# 
# projection_parameters5 = list(
#   coverage = 0.86,
#   delta_infectivity = 1.6,
#   alpha_infectivity = 1.55,
#   sd_min = 0.2,
#   sd_min_senior = 0.2,
#   VE_h = 0.67,
#   wane_R = 1/730,
#   wane_V = 1/730,
#   stop_date = "2022-12-31"
# )
# 
# out5 = Run_Projection(projection_parameters5)
# plot(V1~date,out5$by.strain, xlab = "date", ylab = "hospitalizations", col = col.strain[strain], pch = 16)
# legend("topright", legend = names(col.strain), col = col.strain, pch = 16)
# 
# plot(V1~date,out5$by.immune, xlab = "date", ylab = "hospitalizations", col = col.immune[vac], pch = 16)
# legend("topright", legend = names(col.immune), col = col.immune, pch = 16)
# 
# plot(V1~date,out5$by.age, xlab = "date", ylab = "hospitalizations", col = col.age[age], pch = 16)
# legend("topright", legend = c("0-19", "20-49", "50-69", "70+"), col = col.age, pch = 16)
# #points(V1~date, out1$by.immune)