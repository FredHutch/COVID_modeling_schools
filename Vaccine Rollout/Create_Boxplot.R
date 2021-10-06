library(HutchCOVID)
library(lubridate)
library(dplyr)
library(RColorBrewer)
source("covid-model-plotting.R")
source("functions/school_boxplots.R")

out_dir = "out"
#out_dir = "big_out"


#read in Rdata files, and extract necessary data
start_date = ymd("2021-09-01")
end_date = ymd("2022-06-01")

######### No waning SD_10_30 scenarios ##################################


scenario_files = c(paste0(out_dir, "/scenario0_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_KIDS2_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario0_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario50_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario75_NoWane_B_data.rdata"),
                   paste0(out_dir, "/scenario100_NoWane_B_data.rdata"))

scenario_names = c("0%\nOctober", "50%\nOctober", "75%\nOctober", "100%\nOctober",
                   "0%\nJanuary", "50%\nJanuary", "75%\nJanuary", "100%\nJanuary",
                   "0%\nNo", "50%\nNo", "75%\nNo", "100%\nNo")

schools_hosp_data_file = paste0(out_dir, "/boxplot_hosp_schools_data.rdata")
if (file.exists(schools_hosp_data_file))
{
  load(file = schools_hosp_data_file)
} else {
  final_hosp = consolidate_scenarios(scenario_files, scenario_names,
                                     state_to_extract = "cum_hosp", start_date = start_date, end_date = end_date)
  final_hosp = make_factors(final_hosp)
  
  days_at_max_sd = consolidate_max_sd(scenario_files, scenario_names, report_as_percentage = TRUE,
                                      max_sd_age_1 = 0.3, start_date = start_date, end_date = end_date)
  days_at_max_sd = make_factors(days_at_max_sd)
  
  max_hosp = consolidate_max_scenarios(scenario_files, scenario_names,
                                       c("H", "DH"), start_date = start_date, end_date = end_date)
  max_hosp = make_factors(max_hosp)
  save(final_hosp, days_at_max_sd, max_hosp, file = schools_hosp_data_file)
}

#--------------------------------------------------------------

pdf("out/NoWane_boxplot_hosp_schools_SD_10_30.pdf", width = 11, height = 4)
par(mgp=c(3.5, 1.2, 0), las = 1, mar = c(3, 4.5, 2, 8) + 0.1)

do_box_plots(final_hosp, days_at_max_sd, max_hosp, "hospitalizations")

dev.off()

#--------------------------------------------------------------

schools_death_data_file = paste0(out_dir, "/boxplot_death_schools_data.rdata")
if (file.exists(schools_death_data_file))
{
  load(file=schools_death_data_file)
} else {
  final_death = consolidate_scenarios(scenario_files, scenario_names,
                                      state_to_extract = "cum_death", start_date = start_date, end_date = end_date)
  final_death = make_factors(final_death)
  
  max_death = consolidate_max_scenarios(scenario_files, scenario_names,
                                        c("F", "DF"), start_date = start_date, end_date = end_date)
  max_death = make_factors(max_death)
}

#--------------------------------------------------------------

pdf("out/NoWane_boxplot_death_schools_SD_10_30.pdf", width = 11, height = 4)
par(mgp=c(3.5, 1.2, 0), las = 1, mar = c(3, 4.5, 2, 8) + 0.1)

do_box_plots(final_death, days_at_max_sd, max_death, "deaths")

dev.off()
