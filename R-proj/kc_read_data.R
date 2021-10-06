# this file reads in the data used to fit the model and processes it into the required format

require(lubridate)
require(dplyr)

# Original sources ####

# data file downloaded from the DOH site uses data from King County downloaded on Oct 9th (cut down to 9-30)
# this is the data used to calibrate the model 
#
# tests, cases, deaths and hospitalizations are daily and by age gropu (0-19,20-49,50-69,70+)
# dates must be saved in the format year-mo-day (ex. 2020-08-31)

# To get more recent: refilter IDM raw data from sft.wa.gov (PRIVATE!) into IDM_king_county.csv (checked in) run the
# script select_data.sh in the data directory (change input file if you have a new one) then...
#
# get tests, hosp & deaths separately using the following scripts:
#     tally_tests.R, tally_hosp.R and tally_deaths.R
# sort and tally for each date using the following scripts:
#     sort_test_data.sh, sort_hosp_data.sh and sort_death_data.sh
# then merge into one file using "merge_files.R"
#
# The file is "king_county_by_age.csv".  Its header is shown here:
#date,tests1,tests2,tests3,tests4,pos1,pos2,pos3,pos4,daily pos,cases,hosps1,hosps2,hosps3,hosps4,Hospitalizations,tot hosp,deaths1,deaths2,deaths3,deaths4,daily deaths,deaths
#
# Make sure the 1st date is 2/26 (not 1/14) and replace "NA"s with 0s before proceeding (except cumulative deaths!)
# The NAs come from joins of the files by date on days when there were no deaths!
#
kc_data = read.csv("../data/king_county_by_age.csv", stringsAsFactors = FALSE)
kc_data$date = ymd(kc_data$date)

# calculate negative tests
kc_data = cbind(kc_data, kc_data[,paste0("tests", 1:4)] - kc_data[,paste0("pos", 1:4)])
names(kc_data)[(ncol(kc_data) - 3):ncol(kc_data)] = paste0("neg", 1:4)
  
# do the moving average on the full data as it will be more accurate that way
kc_data = kc_data %>%
  mutate_at(vars(contains("tests")), .funs = list(ma = ~moving_avg(.))) %>%
  mutate_at(vars(contains("hosps")), .funs = list(ma = ~moving_avg(.))) %>%
  mutate_at(vars(contains("pos")), .funs = list(ma = ~moving_avg(.))) %>%
  mutate_at(vars(contains("neg")), .funs = list(ma = ~moving_avg(.))) %>%
  mutate_at(vars(contains("deaths")), .funs = list(ma = ~moving_avg(.))) %>%
  rename_at( vars( contains( "_ma") ), list( ~paste("ma", gsub("_ma", "", .), sep = "_") ) )

# new data off for calibration is Dec 31th
end_date = ymd("2020-12-31")
kc_data_calib = filter(kc_data, date <= end_date)


mean_tests = function(date){
  start = yday(ymd(kc_data$date[1]))
  now = yday(ymd(date))
  range = seq(-3,3) + (now - start + 1)
  
  mean(kc_data$tests[range])
}

# these are the mid moth dates used for tests and other calculated parameters
test_dates = c("2020-03-15", "2020-04-15", "2020-05-15", "2020-06-15", "2020-07-15", "2020-08-15", "2020-09-15", "2020-10-15")

# this is the calculated data from DOH data
kc_data_monthly = list(
  # number of tests performed
  tests = data.frame( date = ymd(test_dates), 
                      value = sapply(test_dates, mean_tests)),	    # Days at which to capture daily testing avg to use for the month
  # TODO for now these monthly values are copy/pasted, but could move calculation to R like the tests?
  # fractions based fraction of tests in each age group 
  # obtained by running "./get_tests_by_age.sh" on "IDM_king_county.csv"
  age_test_frac = data.frame(date = ymd(test_dates),
                             value = matrix(c(c(0.0740358,0.509446,0.294448,0.12207), # based on tests taken 2/1/20 - 3/31/20
                                              c(0.0640876,0.521905,0.297245,0.116763), # based on tests taken during April
                                              c(0.0715118,0.471296,0.299394,0.157798), # based on tests taken during May
                                              c(0.0594408,0.569942,0.245141,0.125476), # based on tests taken during June
                                              c(0.100093,0.597033,0.220479,0.0823942), # based on tests taken during July
                                              c(0.121455,0.559161,0.22471,0.0946729), # based on tests taken during Aug
                                              c(0.13549,0.54648,0.226248,0.0917822), # based on tests taken during Sept
                                              c(0.13058,0.562911,0.232331,0.0741788)), ncol = 4, byrow = TRUE)), # based on tests taken during Oct
  # fractions based percent in an age group that are hospitalized (based on date of hospital admission)
  # obtained by running "./get_hosp_data.sh" on "IDM_king_county.csv"
  hosp_frac = data.frame(date = ymd(test_dates),
                         value = matrix(c(c(0.0833333,0.0870359,0.233359,0.614841), # based on hospital admissions during 2/1/20 - 3/31/20
                                          c(0.0185185,0.0938338,0.236869,0.531697), # based on hospital admissions during April
                                          c(0.0206186,0.065389,0.198324,0.435185),  # based on hospital admissions during May
                                          c(0.0108696,0.030285,0.130631,0.358974),  # based on hospital admissions during June
                                          c(0.00858896,0.030137,0.122622,0.318493), # based on hospital admissions during July
                                          c(0.00256082,0.0264188,0.122503,0.377193), # based on hospital admissions during Aug
                                          c(0.0105448,0.02547,0.102041,.333333), # based on hospital admissions during Sept
                                          c(0.00735294,0.0158991,0.0797546,0.372694)), ncol = 4, byrow = TRUE)) # based on hospital admissions during Oct
  
)


# Set global starting parameters ####
source("kc_globalparams.R")

