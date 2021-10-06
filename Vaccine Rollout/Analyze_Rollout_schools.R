# analyze/ plot results of RunRollout.R (only those w/o childhood vaccination - ages 5-12)

library(foreach)

# set up parallel, might need to change for your system
library(doParallel)
registerDoParallel(cores=4)

library(HutchCOVID)
source("covid-model-plotting.R")
load("../data/wa_counties_aggr.Rdata")

# data from Run_Rollout needed for plotting
County = "King County" 

#Population pulled from census data
total_population = wa.counties.pop[[County]]
names(total_population) = paste0("a", 1:4)

scenario_names = c("B")
school_scenario_names = c("0", "50", "75", "100")

# load scenario results
out_long = list()
out_wide = list()

for(scens in 1:(length(school_scenario_names))) #%dopar%
{
  load(file = paste0("big_out/scenario",school_scenario_names[scens],"_Wane_B_data.rdata"))   
  out_wide[[scens]] = wide_out
  rm(list = c("scen_out", "wide_out"))   
  gc()
}

# calculate metrics
final_cum_death = lapply(out_wide, function(s) sapply(s, function(x) x$cum_tot_death[nrow(x)]))
final_cum_inf = lapply(out_wide, function(s) sapply(s, function(x) x$cum_tot_inf[nrow(x)]))

print(paste("death", lapply(final_cum_death, median), "range", lapply(final_cum_death, range)))
print(paste("inf", lapply(final_cum_inf, median), "range", lapply(final_cum_inf, range)))

### plots ###

pdf("out/Wane_school_scenarios_coverage.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

# cases / inf / hosp / deaths
case_lim = calc_y_lim_from_scenario_list(out_wide, "cases", by_age = FALSE)
for (scen in 1:length(school_scenario_names))
{
  plot_projection_from_data_set(out_wide[[scen]], "cases", paste("Scenario", school_scenario_names[scen]), y_lim = case_lim)
}
inf_lim = calc_y_lim_from_scenario_list(out_wide, "inf", by_age = FALSE)
for (scen in 1:length(school_scenario_names))
{
  plot_projection_from_data_set(out_wide[[scen]], "inf", paste("Scenario", school_scenario_names[scen]), y_lim = inf_lim)
}
hosp_lim = calc_y_lim_from_scenario_list(out_wide, "hosp", by_age = FALSE)
for (scen in 1:length(school_scenario_names))
{
  plot_projection_from_data_set(out_wide[[scen]], "hosp", paste("Scenario", school_scenario_names[scen]), y_lim = hosp_lim)
}
death_lim = calc_y_lim_from_scenario_list(out_wide, "deaths", by_age = FALSE)
for (scen in 1:length(school_scenario_names))
{
  plot_projection_from_data_set(out_wide[[scen]], "deaths", paste("Scenario", school_scenario_names[scen]), y_lim = death_lim)
}

dev.off()


pdf("out/Wane_school_scenario_cum_hist.pdf", width = 8, height = 8)
par(mfrow = c(2,2))

for (scen in 1:length(school_scenario_names))
{
  hist(final_cum_death[[scen]], xlab = "Cummulative deaths", main = paste("Scenario", school_scenario_names[scen]))
  abline(v = median(final_cum_death[[scen]]))
}
for (scen in 1:length(school_scenario_names))
{
  hist(final_cum_inf[[scen]], xlab = "Cummulative infections", main = paste("Scenario", school_scenario_names[scen]))
  abline(v = median(final_cum_inf[[scen]]))
}

dev.off()

rm(list = c("out_wide"))   
gc()
