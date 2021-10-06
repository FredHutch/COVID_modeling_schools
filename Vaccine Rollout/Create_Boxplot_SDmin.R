library(HutchCOVID)
library(lubridate)
library(dplyr)
library(RColorBrewer)
source("covid-model-plotting.R")

#out_dir = "out"
out_dir = "big_out"

do_box_plots_24 = function(final_cumul, days_at_max_sd, max_daily, final_cumul_ylab, max_daily_ylab)
{
  cols_light = c(brewer.pal(9, "PRGn")[7],brewer.pal(9, "RdBu")[7],(brewer.pal(9, "PRGn")[3]))
  cols = c(brewer.pal(9, "PRGn")[8],brewer.pal(9, "RdBu")[8],(brewer.pal(9, "PRGn")[2]))
  cols_dark = c(brewer.pal(9, "PRGn")[9],brewer.pal(9, "RdBu")[9],(brewer.pal(9, "PRGn")[1]))

  final_cumul$scenario_name = factor(final_cumul$scenario_name, levels = unique(final_cumul$scenario_name), ordered = TRUE) # keep original order
  final_cumul$scenario_school = factor(final_cumul$scenario_school, levels = unique(final_cumul$scenario_school), ordered = TRUE) 
  final_cumul$scenario_vac = factor(final_cumul$scenario_vac, levels = unique(final_cumul$scenario_vac), ordered = TRUE) 
  final_cumul$scenario_sdmin = factor(final_cumul$scenario_sdmin, levels = unique(final_cumul$scenario_sdmin), ordered = TRUE) 

  # box plot for scenarios summed across age and vaccination status
  final_cumul_totals = final_cumul %>% group_by(sim, scenario_name) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 
  n = length(unique(final_cumul_totals$scenario_name))

  print(final_cumul_totals)

  metrics = final_cumul_totals %>% 
    group_by(scenario_name) %>% 
    summarise(med = median(value), mean = mean(value), max = max(value), .groups = "drop") 
  print(metrics)
  print(metrics$mean)
  
  plot_data = matrix(rep(0,36),nrow=3)
  print(plot_data[2,12])
  print(plot_data[3,12])
  
  for (i in 1:length(metrics$mean))
  {
      print(i)
      if (i %% 3 == 1) {
	 print(paste("1,",(as.integer(i/3) + 1)))
	 plot_data[1,(as.integer(i/3) + 1)] = metrics$mean[i]
      } else if (i %% 3 == 2) {
	 print(paste("2,",(as.integer(i/3) + 1)))
	 plot_data[2,(as.integer(i/3) + 1)] = metrics$mean[i]
      } else {
	 print(paste("3,",as.integer(i / 3)))
	 plot_data[3,as.integer(i / 3)] = metrics$mean[i]
      }
      print(metrics$mean[i])
  }
  colnames(plot_data) = c("school 0%","50%","75%","100%","school 0%","50%","75%","100%","school 0%","50%","75%","100%")
  rownames(plot_data) = c("5% SD Min","10% SD Min","15% SD Min")
  print(plot_data)
  barplot(plot_data,
          xlab = "", ylab = paste("Mean",final_cumul_ylab),beside=TRUE, xaxt = "n",xlim = c(0.5, 49.5),
          col = c(rep(c(cols_light[1], cols[1], cols_dark[1]),4),rep(c(cols_light[2], cols[2], cols_dark[2]),4),rep(c(cols_light[3], cols[3], cols_dark[3]),4)))
  axis(side = 1, line = -1, at = c(1.5 + 4 * (0:3), 2 + 4 * (4:7), 2.5 + 4 * (8:11)), labels = c("school 0%","50%","75%","100%","school 0%","50%","75%","100%","school 0%","50%","75%","100%"), tick = FALSE, cex.axis = 0.6)
  axis(side = 1, line = 0, at = c(4.5, 22, 36.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright",legend=c("5% SD Min","10% SD Min","15% SD Min"),pch=15,
          col = c("grey70","grey30","grey5"),inset=c(-0.25,0),xpd = TRUE)

  final_cumul_by_vacdate = final_cumul %>% group_by(sim, scenario_name, scenario_school, scenario_vac,scenario_sdmin) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 

  print(final_cumul_by_vacdate)

  final_cumul_by_vacdate$scenario_name_vac = paste0(as.character(final_cumul_by_vacdate$scenario_school), "\n", final_cumul_by_vacdate$scenario_sdmin)
  final_cumul_by_vacdate$scenario_name_vac = factor(final_cumul_by_vacdate$scenario_name_vac, levels = unique(final_cumul_by_vacdate$scenario_name_vac), ordered = TRUE)

  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vacdate, subset = scenario_vac == "October",
          at = 1:12, xlim = c(0.5, 37.5), xaxt = "n",main="Overall",
          xlab = "", ylab = final_cumul_ylab,
          col = c(cols_light[1], cols[1], cols_dark[1]))
  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vacdate, subset = scenario_vac == "January",
          at = 13.5:24.5, add = TRUE, xaxt = "n",
          col = c(cols_light[2], cols[2], cols_dark[2]))
  boxplot(value ~ scenario_name_vac, data = final_cumul_by_vacdate, subset = scenario_vac == "None",
          at = 26:37, add = TRUE, xaxt = "n",
          col = c(cols_light[3], cols[3], cols_dark[3]))
  axis(side = 1, line = -1, at = c(1.5 + 3 * (0:3), 2 + 3 * (4:7), 2.5 + 3 * (8:11)), labels = rep(unique(final_cumul_by_vacdate$scenario_school), 3), tick = FALSE, cex.axis = 0.6)
  axis(side = 1, line = 0, at = c(4.5, 18.5, 31.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright",legend=c("5% SD Min","10% SD Min","15% SD Min"),pch=15,
          col = c("grey70","grey30","grey5"),inset=c(-0.30,0),xpd = TRUE)
  
  # box plot for scenarios summed across age and vaccination status
  max_daily$scenario_name = factor(max_daily$scenario_name, levels = unique(max_daily$scenario_name), ordered = TRUE) # keep original order
  max_daily$scenario_school = factor(max_daily$scenario_school, levels = unique(max_daily$scenario_school), ordered = TRUE) 
  metrics = max_daily %>% 
    group_by(scenario_name) %>% 
    summarise(med = median(value), mean = mean(value), max = max(value), .groups = "drop") 
  print(metrics)
  
  max_daily_by_vacdate = max_daily %>% group_by(sim, scenario_name, scenario_school, scenario_vac,scenario_sdmin) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") 

  print(max_daily_by_vacdate)

  max_daily_by_vacdate$scenario_name_vac = paste0(as.character(max_daily_by_vacdate$scenario_school), "\n", max_daily_by_vacdate$scenario_sdmin)
  max_daily_by_vacdate$scenario_name_vac = factor(max_daily_by_vacdate$scenario_name_vac, levels = unique(max_daily_by_vacdate$scenario_name_vac), ordered = TRUE)

  boxplot(value ~ scenario_name_vac, data = max_daily_by_vacdate, subset = scenario_vac == "October",
          at = 1:12, xlim = c(0.5, 37.5), xaxt = "n",main="Daily Peak",
          xlab = "", ylab = max_daily_ylab,
          col = c(cols_light[1], cols[1], cols_dark[1]))
  boxplot(value ~ scenario_name_vac, data = max_daily_by_vacdate, subset = scenario_vac == "January",
          at = 13.5:24.5, add = TRUE, xaxt = "n",
          col = c(cols_light[2], cols[2], cols_dark[2]))
  boxplot(value ~ scenario_name_vac, data = max_daily_by_vacdate, subset = scenario_vac == "None",
          at = 26:37, add = TRUE, xaxt = "n",
          col = c(cols_light[3], cols[3], cols_dark[3]))
  axis(side = 1, line = -1, at = c(1.5 + 3 * (0:3), 2 + 3 * (4:7), 2.5 + 3 * (8:11)), labels = rep(unique(max_daily_by_vacdate$scenario_school), 3), tick = FALSE, cex.axis = 0.6)
  axis(side = 1, line = 0, at = c(4.5, 18.5, 31.5), labels = c("October vaccination", "January vaccination", "No vaccination"), tick = FALSE, cex.axis = 0.8)
  legend("topright",legend=c("5% SD Min","10% SD Min","15% SD Min"),pch=15,
          col = c("grey70","grey30","grey5"),inset=c(-0.30,0),xpd = TRUE)
  
}

#read in Rdata files, and extract necessary data
start_date = ymd("2021-09-01")
end_date = ymd("2022-06-01")

scenario_files = c(paste0(out_dir, "/scenario0_KIDS_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario0_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_KIDS_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario0_KIDS2_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario0_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_KIDS2_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS2_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS2_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS2_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS2_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS2_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS2_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario0_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario0_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario50_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario50_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario75_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario75_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_NoWane_SD_15_30_data.rdata"),
                   paste0(out_dir, "/scenario100_NoWane_SD_5_30_data.rdata"),
                   paste0(out_dir, "/scenario100_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_NoWane_SD_15_30_data.rdata"))

scenario_names = c("0% school\nOctober\n5% SD Min","0% school\nOctober\n10% SD Min", "0% school\nOctober\n15% SD Min",
		    "50%\nOctober\n5% SD Min","50%\nOctober\n10% SD Min", "50%\nOctober\n15% SD Min",
		    "75%\nOctober\n5% SD Min","75%\nOctober\n10% SD Min", "75%\nOctober\n15% SD Min",
		    "100%\nOctober\n5% SD Min","100%\nOctober\n10% SD Min", "100%\nOctober\n15% SD Min",
		    "0% school\nJanuary\n5% SD Min","0% school\nJanuary\n10% SD Min", "0% school\nJanuary\n15% SD Min",
		    "50%\nJanuary\n5% SD Min","50%\nJanuary\n10% SD Min", "50%\nJanuary\n15% SD Min",
		    "75%\nJanuary\n5% SD Min","75%\nJanuary\n10% SD Min", "75%\nJanuary\n15% SD Min",
		    "100%\nJanuary\n5% SD Min","100%\nJanuary\n10% SD Min", "100%\nJanuary\n15% SD Min",
		    "0% school\nNone\n5% SD Min","0% school\nNone\n10% SD Min", "0% school\nNone\n15% SD Min",
		    "50%\nNone\n5% SD Min","50%\nNone\n10% SD Min", "50%\nNone\n15% SD Min",
		    "75%\nNone\n5% SD Min","75%\nNone\n10% SD Min", "75%\nNone\n15% SD Min",
		    "100%\nNone\n5% SD Min","100%\nNone\n10% SD Min", "100%\nNone\n15% SD Min")

if (file.exists("big_out/NoWane_SDmin_hosp_schools_data.rdata"))
{
    load(file="big_out/NoWane_SDmin_hosp_schools_data.rdata")
} else {
    final_hosp = consolidate_scenarios(scenario_files, scenario_names,
			  state_to_extract = "cum_hosp", start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(final_hosp$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_sdmin")
    final_hosp = cbind(final_hosp, scenario_parts)

    days_at_max_sd = consolidate_max_sd(scenario_files, scenario_names, report_as_percentage = TRUE,
					max_sd_age_1 = 0.3, start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(days_at_max_sd$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_sdmin")
    days_at_max_sd = cbind(days_at_max_sd, scenario_parts)

    max_hosp = consolidate_max_scenarios(scenario_files, scenario_names,
					c("H", "DH"), start_date = start_date, end_date = end_date)

    scenario_parts = plyr::ldply(strsplit(max_hosp$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_sdmin")
    max_hosp = cbind(max_hosp, scenario_parts)


    save(final_hosp, days_at_max_sd, max_hosp, file = "big_out/NoWane_SDmin_hosp_schools_data.rdata")
}

pdf("out/NoWane_SDmin_hosp_schools.pdf", width = 8, height = 4)
par(mgp=c(3.5, 1.2, 0), las = 1, mar = c(3, 4.5, 2, 8) + 0.1)

do_box_plots_24(final_hosp, days_at_max_sd, max_hosp, "Cumulative hospitalizations","Peak hospitalizations")

dev.off()

if (file.exists("big_out/NoWane_SDmin_death_schools_data.rdata"))
{
    load(file="big_out/NoWane_SDmin_death_schools_data.rdata")
} else {
    final_death = consolidate_scenarios(scenario_files, scenario_names,
				       state_to_extract = "cum_death", start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(final_death$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_sdmin")
    final_death = cbind(final_death, scenario_parts)

    max_death = consolidate_max_scenarios(scenario_files, scenario_names,
					 c("F", "DF"), start_date = start_date, end_date = end_date)
    scenario_parts = plyr::ldply(strsplit(max_death$scenario_name, '\n'))
    names(scenario_parts) = c("scenario_school", "scenario_vac", "scenario_sdmin")
    max_death = cbind(max_death, scenario_parts)
    save(final_death, days_at_max_sd, max_death, file = "big_out/NoWane_SDmin_death_schools_data.rdata")
}

#--------------------------------------------------------------

pdf("out/NoWane_SDmin_death_schools.pdf", width = 10, height = 4)
par(mgp=c(3.5, 1.2, 0), las = 1, mar = c(3, 4.5, 2, 8) + 0.1)

do_box_plots_24(final_death, days_at_max_sd, max_death, "Cumulative deaths", "Peak deaths")

dev.off()

