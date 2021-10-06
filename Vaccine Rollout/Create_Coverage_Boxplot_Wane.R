library(HutchCOVID)
library(lubridate)
library(dplyr)
source("covid-model-plotting.R")

#out_dir = "out"
out_dir = "big_out"

do_box_plots_24 = function(final_cumul, days_at_max_sd, max_daily, final_cumul_ylab, max_daily_ylab)
{
  # for now we assume the first half has SD control, second half does not
  cols = c("#7fbf7b", "#67a9cf", "#af8dc3") # for vaccinate kids 10/1 and 1/1, no kids, 
  cols_light = c("#d9f0d3", "#d1e5f0", "#e7d4e8") # green, blue, purple

  final_cumul$scenario_name = factor(final_cumul$scenario_name, levels = unique(final_cumul$scenario_name), ordered = TRUE) # keep original order
  final_cumul$scenario_school = factor(final_cumul$scenario_school, levels = unique(final_cumul$scenario_school), ordered = TRUE) 
  final_cumul$scenario_vac = factor(final_cumul$scenario_vac, levels = unique(final_cumul$scenario_vac), ordered = TRUE) 
  final_cumul$scenario_wane = factor(final_cumul$scenario_wane, levels = unique(final_cumul$scenario_wane), ordered = TRUE) 

  # box plot for scenarios summed across age and vaccination status
  final_cumul_totals = final_cumul %>% group_by(sim, scenario_name) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 
  n = length(unique(final_cumul_totals$scenario_name))
  metrics = final_cumul_totals %>% 
    group_by(scenario_name) %>% 
    summarise(med = median(value), mean = mean(value), max = max(value), .groups = "drop") 
  print(metrics)
  
  plot_data = matrix(rep(0,24),nrow=2)
  for (i in 1:length(metrics$mean))
  {
      if (i %% 2 == 1)
      {
	 plot_data[1,((i+1) / 2)] = metrics$mean[i]
      } else {
	 plot_data[2,(i / 2)] = metrics$mean[i]
      }
  }
  barplot(plot_data,
          xlab = "", ylab = paste("Mean",final_cumul_ylab),beside=TRUE, xaxt = "n",
          col = c(rep(c(cols_light[1], cols[1]),4),rep(c(cols_light[2], cols[2]),4),rep(c(cols_light[3], cols[3]),4)))
  axis(side = 1, line = -1, at = c(1.5 + 3 * (0:3), 2 + 3 * (4:7), 2.5 + 3 * (8:11)), labels = c("school 0%","50%","75%","100%","school 0%","50%","75%","100%","school 0%","50%","75%","100%"), tick = FALSE, cex.axis = 0.8)
  axis(side = 1, line = 0, at = c(4.5, 18, 31.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE)
  legend("topright",legend=c("No Waning","Waning"),pch=15,
          col = c("grey70","grey5"),inset=c(-0.30,0),xpd = TRUE)

  final_cumul_by_vacdate = final_cumul %>% group_by(sim, scenario_name, scenario_school, scenario_vac, scenario_wane) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 

  print(final_cumul_by_vacdate)

  final_cumul_by_vacdate$scenario_name_vac = paste0(as.character(final_cumul_by_vacdate$scenario_school), "\n", final_cumul_by_vacdate$scenario_wane)
  final_cumul_by_vacdate$scenario_name_vac = factor(final_cumul_by_vacdate$scenario_name_vac, levels = unique(final_cumul_by_vacdate$scenario_name_vac), ordered = TRUE)

  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vacdate, subset = scenario_vac == "October",
          at = 1:8, xlim = c(0.5, 25.5), xaxt = "n",main = "Overall",
          xlab = "", ylab = final_cumul_ylab,
          col = c(cols_light[1], cols[1]))

  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vacdate, subset = scenario_vac == "January",
          at = 9.5:16.5, add = TRUE, xaxt = "n",
          col = c(cols_light[2], cols[2]))

  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vacdate, subset = scenario_vac == "None",
          at = 18:25, add = TRUE, xaxt = "n",
          col = c(cols_light[3], cols[3]))

  axis(side = 1, line = -1, at = c(1.5 + 2 * (0:3), 2 + 2 * (4:7), 2.5 + 2 * (8:11)), labels = rep(unique(final_cumul_by_vacdate$scenario_school), 3), tick = FALSE, cex.axis = 0.6)
  axis(side = 1, line = 0, at = c(4.5, 12.5, 20.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright",legend=c("No Waning","Waning"),pch=15,
          col = c("grey70","grey5"),inset=c(-0.25,0),xpd = TRUE)

  max_daily_by_vacdate = max_daily %>% group_by(sim, scenario_name, scenario_school, scenario_vac,scenario_wane) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 

  max_daily_by_vacdate$scenario_name_vac = paste0(as.character(max_daily_by_vacdate$scenario_school), "\n", max_daily_by_vacdate$scenario_wane)
  max_daily_by_vacdate$scenario_name_vac = factor(max_daily_by_vacdate$scenario_name_vac, levels = unique(max_daily_by_vacdate$scenario_name_vac), ordered = TRUE)
  boxplot(value ~ scenario_name_vac, data = max_daily_by_vacdate, subset = scenario_vac == "October",
          at = 1:8, xlim = c(0.5, 25.5), xaxt = "n",main = "Daily Peak",
          xlab = "", ylab = max_daily_ylab,
          col = c(cols_light[1], cols[1]))
  boxplot(value ~ scenario_name_vac, data = max_daily_by_vacdate, subset = scenario_vac == "January",
          at = 9.5:16.5, add = TRUE, xaxt = "n",
          col = c(cols_light[2], cols[2]))
  boxplot(value ~ scenario_name_vac, data = max_daily_by_vacdate, subset = scenario_vac == "None",
          at = 18:25, add = TRUE, xaxt = "n",
          col = c(cols_light[3], cols[3]))
  axis(side = 1, line = -1, at = c(1.5 + 2 * (0:3), 2 + 2 * (4:7), 2.5 + 2 * (8:11)), labels = rep(unique(max_daily_by_vacdate$scenario_school), 3), tick = FALSE, cex.axis = 0.6)
  axis(side = 1, line = 0, at = c(4.5, 12.5, 20.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright",legend=c("No Waning","Waning"),pch=15,
          col = c("grey70","grey5"),inset=c(-0.25,0),xpd = TRUE)
}

#read in Rdata files, and extract necessary data
start_date = ymd("2021-09-01")
end_date = ymd("2022-06-01")

scenario_files = c(paste0(out_dir, "/scenario0_KIDS_NoWane_B_data.rdata"),
		   paste0(out_dir, "/scenario0_KIDS_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS_Wane_B_data.rdata"),
		   paste0(out_dir, "/scenario0_KIDS2_NoWane_B_data.rdata"),
		   paste0(out_dir, "/scenario0_KIDS2_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS2_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS2_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS2_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_Wane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_Wane_B_data.rdata"))

scenario_names = c("0% school\nOctober\nNo Wane","0% school\nOctober\nWane", "50%\nOctober\nNo Wane", "50%\nOctober\nWane",
		    "75%\nOctober\nNo Wane", "75%\nOctober\nWane","100%\nOctober\nNo Wane","100%\nOctober\nWane",
		    "0% school\nJanuary\nNo Wane","0% school\nJanuary\nWane", "50%\nJanuary\nNo Wane", "50%\nJanuary\nWane",
		    "75%\nJanuary\nNo Wane", "75%\nJanuary\nWane","100%\nJanuary\nNo Wane","100%\nJanuary\nWane",
		    "0% school\nNone\nNo Wane","0% school\nNone\nWane", "50%\nNone\nNo Wane", "50%\nNone\nWane",
		    "75%\nNone\nNo Wane", "75%\nNone\nWane","100%\nNone\nNo Wane","100%\nNone\nWane")

if (file.exists("big_out/boxplot_hosp_schools_wane_data.rdata"))
{
    load(file="big_out/boxplot_hosp_schools_wane_data.rdata")
} else {
    final_hosp = consolidate_scenarios(scenario_files, scenario_names,
			  state_to_extract = "cum_hosp", start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(final_hosp$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_wane")
    final_hosp = cbind(final_hosp, scenario_parts)

    days_at_max_sd = consolidate_max_sd(scenario_files, scenario_names, report_as_percentage = TRUE,
					max_sd_age_1 = 0.3, start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(days_at_max_sd$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_wane")
    days_at_max_sd = cbind(days_at_max_sd, scenario_parts)

    max_hosp = consolidate_max_scenarios(scenario_files, scenario_names,
					c("H", "DH"), start_date = start_date, end_date = end_date)

    scenario_parts = plyr::ldply(strsplit(max_hosp$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_wane")
    max_hosp = cbind(max_hosp, scenario_parts)
    save(final_hosp, days_at_max_sd, max_hosp, file = "big_out/boxplot_hosp_schools_wane_data.rdata")
}

pdf("out/boxplot_hosp_schools_Wane.pdf", width = 8, height = 4)
par(mgp=c(3.5, 1.2, 0), las = 1, mar = c(3, 4.5, 2, 8) + 0.1)

do_box_plots_24(final_hosp, days_at_max_sd, max_hosp, "Cumulative hospitalizations","Peak hospitalizations")

dev.off()

if (file.exists("big_out/boxplot_death_schools_wane_data.rdata"))
{
    load(file="big_out/boxplot_death_schools_wane_data.rdata")
} else {
    final_death = consolidate_scenarios(scenario_files, scenario_names,
				       state_to_extract = "cum_death", start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(final_death$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_wane")
    final_death = cbind(final_death, scenario_parts)

    max_death = consolidate_max_scenarios(scenario_files, scenario_names,
					 c("F", "DF"), start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(max_death$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_wane")
    max_death = cbind(max_death, scenario_parts)

    save(final_death, days_at_max_sd, max_death, file = "big_out/boxplot_death_schools_wane_data.rdata")
}

#--------------------------------------------------------------

pdf("out/boxplot_death_schools_Wane.pdf", width = 10, height = 4)
par(mgp=c(3.5, 1.2, 0), las = 1, mar = c(3, 4.5, 2, 8) + 0.1)

do_box_plots_24(final_death, days_at_max_sd, max_death, "Cumulative deaths", "Peak deaths")

dev.off()
