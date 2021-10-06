library(dplyr)

###############################################################################
#Note all there parameters are named "kc" which should be changed
#Has to be repeated here! Bad!!!!! No like!!! Must Change!!!!!!!!!!
# starting_conditions = get_starting_conditions_by_county_date(
#   County = "King County",
#   Start_Date = "2021-06-01"
# )

kc.pop = 2252782#sum(starting_conditions$total_population)
kc_age_prop = c(0.22, 0.46, 0.23, 0.09) #starting_conditions$total_population / kc.pop
kc.vax = c(5000, 10000, 15000)
kc.cover = c(0.7, 0.8, 0.9)
kc.cmax = c(200, 350, 500)
kc.cmin = c(50, 100, 200)
kc.sdmin = c(0.1, 0.2, 0.3)

names.management = c(
  "vac_coverage",
  "vac_rate",
  "dynamic_sd_lock_thresh",
  "dynamic_sd_release_thresh",
  "dynamic_sd_min",
  "management"
)


mgmt_params = expand.grid(
  cover = kc.cover,
  vrate = kc.vax,
  cmax = kc.cmax,
  cmin = kc.cmin,
  sdmin = kc.sdmin
)

mgmt_params_internal = mgmt_params

mgmt_params_internal$cmax = mgmt_params_internal$cmax/100000 * kc.pop
mgmt_params_internal$cmin = mgmt_params_internal$cmin/100000 * kc.pop


best_guess_mgmt = 122

##################Deprecated??#####################################
extremes_only = mgmt_params %>%
  filter(cover != kc.cover[2],
         vrate != kc.vax[2],
         cmax != kc.cmax[2],
         cmin != kc.cmin[2],
         sdmin != kc.sdmin[2])

mgmt_extremes = c(extremes_only$management, best_guess_mgmt)
###################################################################

##################################
#Random Crap

vax.date = ymd("2021-01-15")
calib.date = ymd("2020-06-01")



