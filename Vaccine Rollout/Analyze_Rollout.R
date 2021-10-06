# analyze/ plot results of RunRollout.R

library(HutchCOVID)
source("kc_read_data.R")
source("covid-model-plotting.R")
load("../data/wa_counties_aggr.Rdata")

# data from Run_Rollout needed for plotting
County = "King County" 

#Population pulled from census data
total_population = wa.counties.pop[[County]]
names(total_population) = paste0("a", 1:4)

scenario_names = c("A", "B", "C", "D")

# load scenario results
out_long = list()
out_wide = list()

for (i in 1:length(scenario_names))
{ 
  load(file = paste0("out/scenario_", scenario_names[i], "_data.rdata"))   
  out_wide[[scenario_names[i]]] = wide_out
  rm(list = c("scen_out", "wide_out"))   
  gc()
  print(i)
}


### plots ###

pdf("out/scenario_strain_inf_coverage.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

# cases / inf / hosp / deaths
case_lim = calc_y_lim_from_scenario_list(out_wide, "cases", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_wide[[scen]], "cases", paste("Scenario", scen), y_lim = case_lim)
}
inf_lim = calc_y_lim_from_scenario_list(out_wide, "inf", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_wide[[scen]], "inf", paste("Scenario", scen), y_lim = inf_lim)
}
hosp_lim = calc_y_lim_from_scenario_list(out_wide, "hosp", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_wide[[scen]], "hosp", paste("Scenario", scen), y_lim = hosp_lim)
}
death_lim = calc_y_lim_from_scenario_list(out_wide, "deaths", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_wide[[scen]], "deaths", paste("Scenario", scen), y_lim = death_lim)
}

dev.off()


# calculate metrics
final_cum_death = lapply(out_wide, function(s) sapply(s, function(x) x$cum_tot_death[nrow(x)]))
final_cum_inf = lapply(out_wide, function(s) sapply(s, function(x) x$cum_tot_inf[nrow(x)]))

print(paste("death", lapply(final_cum_death, median), "range", lapply(final_cum_death, range)))
print(paste("inf", lapply(final_cum_inf, median), "range", lapply(final_cum_inf, range)))

pdf("out/scenario_strain_inf_coverage_cum_hist.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

for (i in 1:length(scenario_names))
{
  hist(final_cum_death[[i]], xlab = "Cummulative deaths", main = paste("Scenario", scenario_names[i]))
  abline(v = median(final_cum_death[[i]]))
}
for (i in 1:length(scenario_names))
{
  hist(final_cum_inf[[i]], xlab = "Cummulative infections", main = paste("Scenario", scenario_names[i]))
  abline(v = median(final_cum_inf[[i]]))
}

dev.off()

rm(list = c("out_wide"))   
gc()

############# KIDS ################################################################################################

out_kids_long = list()
out_kids_wide = list()

for (i in 1:length(scenario_names))
{ 
  load(file = paste0("out/scenario_KIDS_", scenario_names[i], "_data.rdata"))   
  out_kids_long[[scenario_names[i]]] = scen_out
  out_kids_wide[[scenario_names[i]]] = wide_out
  rm(list = c("scen_out", "wide_out"))   
  gc()
  print(i)
}


pdf("out/scenario_KIDS_strain_inf_coverage.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

# cases / inf / hosp / deaths
# lim = calc_y_lim_from_scenario_list(out_kids_wide, "cases", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_kids_wide[[scen]], "cases", paste("Scenario", scen), y_lim = case_lim)
}
# lim = calc_y_lim_from_scenario_list(out_kids_wide, "inf", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_kids_wide[[scen]], "inf", paste("Scenario", scen), y_lim = inf_lim)
}
# lim = calc_y_lim_from_scenario_list(out_kids_wide, "hosp", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_kids_wide[[scen]], "hosp", paste("Scenario", scen), y_lim = hosp_lim)
}
# lim = calc_y_lim_from_scenario_list(out_kids_wide, "deaths", by_age = FALSE)
for (scen in scenario_names)
{
  plot_projection_from_data_set(out_kids_wide[[scen]], "deaths", paste("Scenario", scen), y_lim = death_lim)
}

dev.off()

print("kids")
final_cum_death = lapply(out_kids_wide, function(s) sapply(s, function(x) x$cum_tot_death[nrow(x)]))
final_cum_inf = lapply(out_kids_wide, function(s) sapply(s, function(x) x$cum_tot_inf[nrow(x)]))

print(paste("death", lapply(final_cum_death, median), "range", lapply(final_cum_death, range)))
print(paste("inf", lapply(final_cum_inf, median), "range", lapply(final_cum_inf, range)))

pdf("out/scenario_KIDS_strain_inf_coverage_cum_hist.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

for (i in 1:length(scenario_names))
{
  hist(final_cum_death[[i]], xlab = "Cummulative deaths", main = paste("Scenario", scenario_names[i]))
  abline(v = median(final_cum_death[[i]]))
}
for (i in 1:length(scenario_names))
{
  hist(final_cum_inf[[i]], xlab = "Cummulative infections", main = paste("Scenario", scenario_names[i]))
  abline(v = median(final_cum_inf[[i]]))
}

dev.off()

# plots with long
rm(list = c("out_kids_wide"))
gc()

# for (i in 1:length(scenario_names))
# { 
#   load(file = paste0("out/scenario_KIDS_", scenario_names[i], "_data.rdata"))   
#   out_kids_long[[scenario_names[i]]] = scen_out
#   rm(list = c("scen_out", "wide_out"))   
#   gc()
#   print(i)
# }
# 
# pdf("out/scenario_KIDS_strain_inf_coverage_details.pdf", width = 8, height = 8)
# par(mfrow = c(2,2))
# 
# 
# # vaccine coverage
# for (scen in scenario_names)
# {
#   plot_vac_across_compartments_from_long_set(out_kids_long[[scen]], total_population, title = paste("Scenario", scen))
# }
# 
# # strains
# for (scen in scenario_names)
# {
#   plot_strain_proportion_current_inf(out_kids_long[[scen]], title = paste("Scenario", scen))
# }
# 
# # sd
# for (scen in scenario_names)
# {
#   plot_sd_from_long_set(out_kids_long[[scen]], title = paste("Scenario", scen))
# }
# 
# # Reff
# for (scen in scenario_names)
# {
#   plot_r_eff_from_long_list(out_kids_long[[scen]], title = paste("Scenario", scen))
# }
# 
# dev.off()
# 
