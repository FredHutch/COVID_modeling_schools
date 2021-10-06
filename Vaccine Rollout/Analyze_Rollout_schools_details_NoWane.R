# analyze/ plot results of RunRollout.R

library(HutchCOVID)
library(lubridate)
library(dplyr)
source("covid-model-plotting.R")
source("Set_initial_conditions/Starting_State.R")
load("../data/wa_counties_aggr.Rdata")
load("../data/wa_county_vaccines.Rdata")
load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")

# data from Run_Rollout needed for plotting
County = "King County" 
N.age.groups = 4

#Population pulled from census data
total_population = wa.counties.pop[[County]]
names(total_population) = paste0("a", 1:4)

#Option 2: Trust Mia and select by County and date
starting_conditions = get_starting_conditions_by_county_date(
  County = "King County",
  Start_Date = "2021-06-01"
)
proportion_vaccinated = starting_conditions[["proportion_vaccinated"]]

# load scenario results
out_long = list()


scenario_names = c("B")
school_scenario_names = c("0", "50", "75", "100")

# load scenario results
out_long = list()

for(scens in 1:(length(school_scenario_names))) #%dopar%
{
  load(file = paste0("big_out/scenario",school_scenario_names[scens],"_NoWane_B_data.rdata"))   
  out_long[[scens]] = scen_out
  rm(list = c("scen_out", "wide_out"))   
  gc()
}

pdf("out/NoWane_scenario_schools_detail.pdf", width = 8, height = 8)
par(mfrow = c(2,2))
# vaccine coverage
for (scen in 1:length(school_scenario_names))
{
  plot_vac_across_compartments_from_long_set(out_long[[scen]], total_population, c("Vax"), sum_vacs = TRUE, title = paste("Scenario", scen))
}

# strains
for (scen in 1:length(school_scenario_names))
{
  plot_strain_proportion_current_inf(out_long[[scen]], title = paste("Scenario", scen))
}

# sd
for (scen in 1:length(school_scenario_names))
{
  plot_sd_from_long_set(out_long[[scen]], title = paste("Scenario", scen))
}

# Reff
for (scen in 1:length(school_scenario_names))
{
  plot_r_eff_from_long_list(out_long[[scen]], title = paste("Scenario", scen))
}

dev.off()

rm(list = c("out_long"))
gc()

############# KIDS ################################################################################################

out_kids_long = list()
out_kids_wide = list()

for(scens in 1:(length(school_scenario_names))) #%dopar%
{
  load(file = paste0("big_out/scenario",school_scenario_names[scens],"_KIDS_NoWane_B_data.rdata"))   
  out_kids_long[[scens]] = scen_out
  rm(list = c("scen_out", "wide_out"))   
  gc()
}

pdf("out/NoWane_KIDS_scenario_schools_details.pdf", width = 8, height = 8)
par(mfrow = c(2,2))
# vaccine coverage
for (scen in 1:length(school_scenario_names))
{
  plot_vac_across_compartments_from_long_set(out_kids_long[[scen]], total_population, c("Vax"), sum_vacs = TRUE, title = paste("Scenario", school_scenario_names[scen]))
}

# strains
for (scen in 1:length(school_scenario_names))
{
  plot_strain_proportion_current_inf(out_kids_long[[scen]], title = paste("Scenario", school_scenario_names[scen]))
}

# sd
for (scen in 1:length(school_scenario_names))
{
  plot_sd_from_long_set(out_kids_long[[scen]], title = paste("Scenario", school_scenario_names[scen]))
}

# Reff
for (scen in 1:length(school_scenario_names))
{
  plot_r_eff_from_long_list(out_kids_long[[scen]], title = paste("Scenario", school_scenario_names[scen]))
}

dev.off()


############# KIDS ################################################################################################

out_kids_long = list()
out_kids_wide = list()

for(scens in 1:(length(school_scenario_names))) #%dopar%
{
  load(file = paste0("big_out/scenario",school_scenario_names[scens],"_KIDS2_NoWane_B_data.rdata"))   
  out_kids_long[[scens]] = scen_out
  rm(list = c("scen_out", "wide_out"))   
  gc()
}

pdf("out/NoWane_KIDS2_scenario_schools_details.pdf", width = 8, height = 8)
par(mfrow = c(2,2))
# vaccine coverage
for (scen in 1:length(school_scenario_names))
{
  plot_vac_across_compartments_from_long_set(out_kids_long[[scen]], total_population, c("Vax"), sum_vacs = TRUE, title = paste("Scenario", school_scenario_names[scen]))
}

# strains
for (scen in 1:length(school_scenario_names))
{
  plot_strain_proportion_current_inf(out_kids_long[[scen]], title = paste("Scenario", school_scenario_names[scen]))
}

# sd
for (scen in 1:length(school_scenario_names))
{
  plot_sd_from_long_set(out_kids_long[[scen]], title = paste("Scenario", school_scenario_names[scen]))
}

# Reff
for (scen in 1:length(school_scenario_names))
{
  plot_r_eff_from_long_list(out_kids_long[[scen]], title = paste("Scenario", school_scenario_names[scen]))
}

dev.off()


