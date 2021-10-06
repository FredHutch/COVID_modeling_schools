#' Reshapes long form model output into that expected by shiny app
#'
#' @param scenario_output this is a list of data frames, where each data frame is a single model run in long form output, 
#' i.e. the result of running run_model and shape_long
#' @param result_filename the file name to save the rdata object of the reshaped data
#'
#' @return nothing
shape_shiny = function(scenario_output)
{
  require(reshape2)
  
  # here is an example using the recent scenario runs
  # not that REff should be calculated as part of running the model and is one of the states
  out_long <- melt(scenario_output, id = c("state", "strain", "vac", "age", "time", "date")) %>%
    rename(management = L1)
  
  # here are the different values for series (aka state) that the shiny app expects
  #  "tests"    "cases"    "deaths"   "cum_hosp" "inf"      "Reff"     "inf1"     "inf2"     "vax"     
  # "cases_1"  "cases_2"  "cases_3"  "cases_4"  "deaths_1" "deaths_2" "deaths_3" "deaths_4" 
  # "hosp_1"   "hosp_2"   "hosp_3"  "hosp_4"   "sd_1"     "sd_2"     "sd_3"     "sd_4"     "vac_1"    "vac_2"    "vac_3"    "vac_4"   
  
  # map states to names used in shiny app
  out_long$state <- recode(out_long$state,
                          cum_exp = "inf", #Take difference to get daily number
                          # cum_hosp = "cum_hosp", #Take difference to get daily number
                          DH = "hosp", #Currently hospitalized
                          H = "hosp", #Currently hospitalized
                          cum_vac = "vax", #Take difference to get daily number
                          cum_death = "deaths", #Take difference to get daily number
                          cum_testpos = "tests",
                          cum_testneg = "tests",
                          sd = "sd", #check that this will result in the right # of values, don't sum
                          cum_diag = "cases",
                          r_eff = "Reff"
                          )
                       
  
  # pull out infections by strain
  out_strains <- out_long %>%
    filter(state %in% c("cases")) %>%
    group_by(management, strain, date) %>%
    summarize(value = sum(value)) %>%
    mutate(Daily = value - lag(value)) %>%
    rename(series = strain, Date = date, Cumulative = value) %>%
    ungroup()
  # replace "s1", etc with "inf1" etc - IGNORE FOR NOW
  
    
  # split age things from others
  out_ages <- out_long %>%
    filter(state %in% c("cases", "deaths", "hosp", "vax", "sd")) %>%
    group_by(management, age, state, date) %>%
    summarize(value = sum(value))  %>%
    melt(id = c("management", "date", "state", "age"))%>%
    mutate(Daily = value - lag(value),
           series = paste(state, age, sep = "_")) %>%
    dplyr::select(management, series, date, value, Daily) %>%
    rename(Date = date, Cumulative = value)
  out_ages$series <- recode(out_ages$series,
                            vax_1 = "vac_1",
                            vax_2 = "vac_2",
                            vax_3 = "vac_3",
                            vax_4 = "vac_4")
  
  # sum non-age things by all ages and strains and vac
  out_totals <- out_long %>%
    filter(state %in% c("cases", "deaths", "cum_hosp", "vax", "inf", "tests")) %>%
    group_by(management, state, date) %>%
    summarize(Cumulative = sum(value))%>%
    mutate(Daily = Cumulative - lag(Cumulative)) %>%
    rename(series = state, Date = date) %>%
    ungroup()
  
  # pull out Reff (single string per simulation)
  out_reff <- out_long %>%
    filter(state == "Reff") %>%
    dplyr::select(management, state, date, value) %>%
    rename(series = state, Date = date, Cumulative = value) %>%
    mutate(Daily = Cumulative)
  
  
  # make the following three tables align to the current expected format for shiny below
  # names(out_ages)
  # names(out_strains)
  # names(out_totals)
  # names(out_reff)
  
  # str(out_ages)
  # str(out_strains)
  # str(out_totals)
  # str(out_reff)
  # unique(out_strains$series)
  
  out_shiny <- rbind(out_totals,
                     out_ages,
                     out_strains,
                     out_reff)
  
  # summary(factor(out_shiny$series))
  
  out_shiny
}
