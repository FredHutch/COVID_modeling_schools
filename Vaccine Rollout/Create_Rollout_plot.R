# create vaccine rollout plot

library(HutchCOVID)
source("../Vaccine Rollout/Run_Rollout_NoWane_Setup.R")
source("covid-model-plotting.R")

parameter_file = "out/parameter_sets/params_out_lastperiod_rej.csv"
param_set = read.csv(parameter_file, check.names = FALSE) # this preserves the dash in termporal parameter names
param_names = names(param_set)

# scenario B is the one with 0.85 coverage

cols = c("#7fbf7b", "#67a9cf", "#af8dc3") # for vaccinate kids 10/1 and 1/1, no kids, 
start_date = ymd("2021-06-01")
end_date = ymd("2022-06-01")
vac_name = "Vax"

# run the scenarios
no_kids = get_model_data_param_sets(param_set[1,,drop = FALSE], param_names, scenario_B, scenario_temporal_B,
                                     state_scenario_base, NULL, start_date = start_date, end_date = end_date,
                                     calc_r_eff = TRUE, out_type = "long") 
kids_oct = get_model_data_param_sets(param_set[1,,drop = FALSE], param_names, scenario_B, scenario_temporal_kids_B,
                                    state_scenario_base, NULL, start_date = start_date, end_date = end_date,
                                    calc_r_eff = TRUE, out_type = "long") 
kids_jan = get_model_data_param_sets(param_set[1,,drop = FALSE], param_names, scenario_B, scenario_temporal_kids2_B,
                                    state_scenario_base, NULL, start_date = start_date, end_date = end_date,
                                    calc_r_eff = TRUE, out_type = "long") 
results = list(kids_oct[[1]], kids_jan[[1]], no_kids[[1]])

plot_vac_across_compartments_from_long_set(results, total_pop = total_population, vac_names = vac_name, by_age = TRUE)



pdf("out/rollout_schematic.pdf", height = 3.5, width = 6)
par(mar = c(3, 4, 1, 1) + 0.1)
plot(0,0, type = "n", las = 1, xlab = "", ylab = "Vacine coverage",
     xlim = c(start_date, end_date), ylim = c(0,100), xaxt = "n" )
draw_month_axis(start_date, end_date, by = 2)

# adults, just plot one since they're all the same

data_adult = filter_non_compartments(results[[1]]) %>%
  filter(vac %in% vac_name & age %in% 2:4) %>%
  group_by(time, date) %>%
  summarise(value = sum(value), .groups = "drop") 
lines(data_adult$date, data_adult$value / sum(total_population[2:4]) * 100, lwd = 3, col = "black")

for (i in 1:3)
{
  data_kid = filter_non_compartments(results[[i]]) %>%
    filter(vac %in% vac_name & age == 1) %>%
    group_by(time, date) %>%
    summarise(value = sum(value), .groups = "drop") 
  lines(data_kid$date, data_kid$value / sum(total_population[1])* 100, lwd = 3, col = cols[i])
}

legend("bottomright", legend = c("Age 20+", "Age 0-19, October vaccination ages 5-11", "Age 0-19, January vaccination ages 5-11", "Age 0-19, no vaccination ages 5-11"),
       col = c("black", cols), lwd = 3, bty = "n")

dev.off()
