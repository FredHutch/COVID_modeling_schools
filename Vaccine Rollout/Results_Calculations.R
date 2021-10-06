# results file calculations

# calculate numbers of vaccines for different coverages

# new from Mia's initial state
source("Set_initial_conditions/Starting_State.R")
load("../data/wa_counties_aggr.Rdata")
load("../data/wa_county_vaccines.Rdata")
load("../data/Hosp_Data/CUMULATIVE_INCIDENCE.Rdata")
load("../data/wa_counties_age1_proportions.Rdata")

County = "King County"
Start_Date = "2021-06-01"
starting_conditions = get_starting_conditions_by_county_date(County, Start_Date)
age1_proportion = age1.prop.by.county[[County]]

# number age 5-11
pop_5_11 = starting_conditions$total_population[1] * age1_proportion$`age5-11`
0.85 * pop_5_11

# number 12+
pop_12_plus = starting_conditions$total_population[1] * (age1_proportion$`age12-15` + age1_proportion$`age16-17` + age1_proportion$`age18-19`) +
  sum(starting_conditions$total_population[2:4])
0.05 * pop_12_plus

# 85% coverage of 12+ is what for total pop
pop_12_plus * 0.85 / sum(starting_conditions$total_population)

# 85% coverage of 5+
(pop_5_11 + pop_12_plus) * 0.85 / sum(starting_conditions$total_population)


# percent age 1 unvaccinated if vaccinate 12+ at 85%
# unvaccinated population
school_unvac = (age1_proportion$`age5-11` + 0.15 * (age1_proportion$`age12-15` + age1_proportion$`age16-17`)) * starting_conditions$total_population[1]
nonschool_unvac = (age1_proportion$`age1-4` + 0.15 * age1_proportion$`age18-19`) * starting_conditions$total_population[1] + 
  0.15 * sum(starting_conditions$total_population[2:4])
school_unvac / (school_unvac + nonschool_unvac)

# or if vaccinate 5-11
school_w5_unvac = 0.15 * (age1_proportion$`age5-11` + age1_proportion$`age12-15` + age1_proportion$`age16-17`) * starting_conditions$total_population[1]
school_w5_unvac / (school_w5_unvac + nonschool_unvac)


# load results summary statistics

out_dir = "out/"
source("functions/school_boxplots.R")

# First do base scenarios...

#Non-waning school closure data...
load(paste0(out_dir, "boxplot_death_schools_data.rdata"))
load(paste0(out_dir, "boxplot_hosp_schools_data.rdata"))

# if these are regenerated this won't be necessary but these versions of the rdata weren't saved this way...
final_hosp = make_factors(final_hosp)
days_at_max_sd = make_factors(days_at_max_sd)
max_hosp = make_factors(max_hosp)

# report median and IQR for schools 100% vs 0%, No vaccination
final_hosp_totals = get_totals_all(final_hosp)
round(tapply(final_hosp_totals$value, final_hosp_totals$scenario_name, median))
round(tapply(final_hosp_totals$value, final_hosp_totals$scenario_name, quantile, 0.25))
round(tapply(final_hosp_totals$value, final_hosp_totals$scenario_name, quantile, 0.75))

# peak hospitalizations for schools 100% vs 0%, No vaccination
round(tapply(max_hosp$value, max_hosp$scenario_name, median))
round(tapply(max_hosp$value, max_hosp$scenario_name, quantile, 0.25))
round(tapply(max_hosp$value, max_hosp$scenario_name, quantile, 0.75))

# and for just children
round(tapply(max_hosp$children, max_hosp$scenario_name, median))
round(tapply(max_hosp$children, max_hosp$scenario_name, quantile, 0.25))
round(tapply(max_hosp$children, max_hosp$scenario_name, quantile, 0.75))

# how much do schools at 50% or 75% reduce
# (50%\nNo - 100%\nNo) / 100%\nNo
(4732 - 4945) / 4945
# (75%\nNo - 100%\nNo) / 100%\nNo
(4843 - 4945) / 4945

# how much does October vaccination reduce
total_medians = round(tapply(final_hosp_totals$value, final_hosp_totals$scenario_name, median))
(total_medians[9:12] -total_medians[1:4]) / total_medians[9:12]

# 75% to 100% with October vaccination
(3951 - 4516) / 4516

# 50% to 100% with October vaccination
(3488 - 4516) / 4516

# 75% October vaccination compared to 75% no vaccination
diff_75 = arrange(filter(final_hosp_totals, scenario_name == "75%\nNo"), sim)$value - 
  arrange(filter(final_hosp_totals, scenario_name == "75%\nOctober"), sim)$value
round(quantile(diff_75, c(0.25, 0.5, 0.75)))

# child hospitalizations, median for schools 100% vs 0%, No vaccination
final_hosp_child = filter(get_totals_by_age(final_hosp), age == 1)
round(tapply(final_hosp_child$value, final_hosp_child$scenario_name, median))
round(tapply(final_hosp_child$value, final_hosp_child$scenario_name, quantile, 0.25))
round(tapply(final_hosp_child$value, final_hosp_child$scenario_name, quantile, 0.75))

# how much does October vaccination reduce
child_medians = round(tapply(final_hosp_child$value, final_hosp_child$scenario_name, median))
(child_medians[9:12] -child_medians[1:4]) / child_medians[9:12]

# how much do schools at 50% or 75% reduce
# (50%\nNo - 100%\nNo) / 100%\nNo
(249 - 325) / 325
# (75%\nNo - 100%\nNo) / 100%\nNo
(300 - 325) / 325

# percentage time at max sd, schools 100% open, no vaccination
round(tapply(days_at_max_sd$value, days_at_max_sd$scenario_name, median))
round(tapply(days_at_max_sd$value, days_at_max_sd$scenario_name, quantile, 0.25))
round(tapply(days_at_max_sd$value, days_at_max_sd$scenario_name, quantile, 0.75))

# no vaccination, 100% to 75% open, convert to days
num_days = ymd("2022-06-01") - ymd("2021-09-01") + 1
tapply(days_at_max_sd$value * num_days / 100, days_at_max_sd$scenario_name, median)

# how many avoid restrictions completely
tapply(days_at_max_sd$value == 0, days_at_max_sd$scenario_name, sum)

# per capita rate of hospitalizations for vaccinated vs unvaccinated, 100%, no vaccination
final_hosp_by_vac_no = filter(get_totals_by_vac(final_hosp), scenario_vac == "No")
round(tapply(final_hosp_by_vac_no$per_capita, final_hosp_by_vac_no$scenario_name_vac, median))
# so now divide 100%\nunvaccinated  by 100%\nvaccinated 
615 / 77 

### now compare coverage

#Vax coverage comparison data (non-waning)...
load(paste0(out_dir, "boxplot_death_schools_cov_data.rdata"))
load(paste0(out_dir, "boxplot_hosp_schools_cov_data.rdata"))

# if these are regenerated this won't be necessary but these versions of the rdata weren't saved this way...
final_hosp = make_factors(final_hosp)
days_at_max_sd = make_factors(days_at_max_sd)


final_hosp_totals = get_totals_all(final_hosp)
round(tapply(final_hosp_totals$value, final_hosp_totals$scenario_name, median))

# 85 to 90% coverage, no vaccine, schools 75%
(4843 - 3539) / 4843

# 85 to 90% coverage, October vaccine, schools 75%
(3951 - 1559) / 3951

# 80 to 85% coverage, no vaccine, schools 75%
5430 - 4843

# 80 to 85% coverage, October vaccine, schools 75%
5132 - 3951

# now compare no SD

#No SD Control comparison data (non-waning)...
load(paste0(out_dir, "boxplot_hosp_schools_nosd_data.rdata"))
load(paste0(out_dir, "boxplot_death_schools_nosd_data.rdata"))

# if these are regenerated this won't be necessary but these versions of the rdata weren't saved this way...
final_hosp = make_factors(final_hosp)
days_at_max_sd = make_factors(days_at_max_sd)

final_hosp_totals = get_totals_all(final_hosp)
round(tapply(final_hosp_totals$value, final_hosp_totals$scenario_name, median))

# labeling: "No Waning" corresponds to "SD control" and Waning to "No SD Control" 
# no vaccination, 100% schools, difference SD and no sd
(11031 - 4945) / 4945

# no vaccination, 0% schools, difference SD and no sd
(4719 - 3675) / 3675
  
# october vaccination, 100% schools, difference SD and no sd
(7464 - 4516) / 4516

########################################
# other result files (not in main results)

#SDmin comparison data (non-waning)...
load(paste0(out_dir, "NoWane_SDmin_death_schools_data.rdata"))
load(paste0(out_dir, "NoWane_SDmin_hosp_schools_data.rdata"))

#Waning/Non-waning comparison data...
load(paste0(out_dir, "boxplot_death_schools_wane_data.rdata"))
load(paste0(out_dir, "boxplot_hosp_schools_wane_data.rdata"))

#Non-waning school closure data...
load(paste0(out_dir, "boxplot_death_schools_data.rdata"))
load(paste0(out_dir, "boxplot_hosp_schools_data.rdata"))

#No SD control only (non-waning) school closure data...
load(paste0(out_dir, "nosd_boxplot_death_schools_data.rdata"))
load(paste0(out_dir, "nosd_boxplot_hosp_schools_data.rdata"))

#Waning Only school closure data...
load(paste0(out_dir, "wane_boxplot_death_schools_data.rdata"))
load(paste0(out_dir, "wane_boxplot_hosp_schools_data.rdata"))


