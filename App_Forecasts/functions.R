get_model_app_data_param_sets = function(pars_matrix, params_names, params_base, params_temporal, 
                                         init_state, kc_data = NULL, start_date = NULL, end_date = NULL,
                                         n_cores = 1)
{
  require(foreach)
  
  if (n_cores > 1)
  {
    registerDoParallel(cores = n_cores)
  } else
  {
    registerDoSEQ()
  }
  pars_matrix = as.matrix(pars_matrix)
  results = foreach (i = 1:nrow(pars_matrix)) %dopar%
    {
      parameters = get_params(pars_matrix[i,], params_names, params_base)
      parameters_temporal = get_temporal_params(pars_matrix[i,], params_names, params_temporal)
      
      if (is.null(start_date)) { start = get_date_from_model_day(parameters$first_inf_day, parameters$model_day0_date) }
      else { start = start_date }
      if (is.null(end_date)) 
      {
        if (is.null(kc_data)) { end = start_date + 100 }
        else { end = max(kc_data$date) }
      }
      else { end = end_date }
      
      model_out = run_model_by_date(parameters, parameters_temporal, init_state, start, end, calc_r_eff = T)
      
      out_data = shape_data_long(model_out, parameters$model_day0_date)
      out_data = shape_shiny(out_data)
      out_data$sim = parameters$sim
      out_data$management = parameters$management
      out_data
    }
  
  return(results)
}


shape_shiny = function(scenario_output)
{
  require(reshape2)
  
  # here is an example using the recent scenario runs
  # not that REff should be calculated as part of running the model and is one of the states
  out_long <- melt(scenario_output, id = c("state", "strain", "vac", "age", "time", "date"))
  
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
                           # sd = "sd", #check that this will result in the right # of values, don't sum
                           cum_diag = "cases",
                           r_eff = "Reff"
  )
  
  
  # pull out infections by strain
  out_strains <- out_long %>%
    filter(state %in% c("cases")) %>%
    group_by(strain, date) %>%
    summarize(value = sum(value)) %>%
    mutate(Daily = value - lag(value)) %>%
    rename(series = strain, Date = date, Cumulative = value) %>%
    ungroup()
  # replace "s1", etc with "inf1" etc - IGNORE FOR NOW
  
  
  # split age things from others
  out_ages <- out_long %>%
    filter(state %in% c("cases", "deaths", "hosp", "vax", "sd")) %>%
    group_by(age, state, date) %>%
    summarize(value = sum(value)) %>% melt(id = c("date", "state", "age"))
  
  out_ages = data.table::data.table(out_ages)
  
  out_ages[,series := paste(state, age, sep = "_"),]
  out_ages[,Daily := value - lag(value), by = c("state", "age")]
  
  out_ages <- out_ages %>%
    dplyr::select(series, date, value, Daily) %>%
    rename(Date = date, Cumulative = value)
  out_ages$series <- recode(out_ages$series,
                            vax_1 = "vac_1",
                            vax_2 = "vac_2",
                            vax_3 = "vac_3",
                            vax_4 = "vac_4")
  
  # sum non-age things by all ages and strains and vac
  out_totals <- out_long %>%
    filter(state %in% c("cases", "deaths", "cum_hosp", "vax", "inf", "tests")) %>%
    group_by(state, date) %>%
    summarize(Cumulative = sum(value))%>%
    mutate(Daily = Cumulative - lag(Cumulative)) %>%
    rename(series = state, Date = date) %>%
    ungroup()
  
  # pull out Reff (single string per simulation)
  out_reff <- out_long %>%
    filter(state == "Reff") %>%
    dplyr::select(state, date, value) %>%
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
  
  rbind(out_totals,
        out_ages,
        out_strains,
        out_reff)
  
  
}


get.max  = function(out_shiny){
  .tmp = data.table::data.table(out_shiny)
  .tmp[,max:="Daily",]
  
  .tmp2 = data.table::data.table(out_shiny)
  .tmp2[,max:="Cumulative",]
  .tmp3 = rbind(.tmp[,max(Daily, na.rm = T), by = c("sim", "management", "series", "max")],
                .tmp2[,max(Cumulative, na.rm = T), by = c("sim", "management", "series", "max")]
  )
  
  .tmp3[,max.val := V1,]
  .tmp3[,V1 := NULL,]
}

construct_ds_maxvals = function(out_shiny_list){
  data.frame(Reduce("rbind", lapply(out_shiny_list, get.max)))
}


save.scenario = function(input, output_path){
  
  with(input,{
    save_path = paste0(output_path, scenario_name, ".Rdata")
    scenario.params = get_parameters_from_specs(specs, p_base, p_temporal_base)
    
    p_base = scenario.params$params
    p_temporal = scenario.params$params_temporal
    
    initial_conditions = get_starting_state_from_scenario(p_base, 
                                                          starting_state, 
                                                          specs)
    
    state_scenario_init = state_matrix_to_vector(initial_conditions)
    
    N.age.groups = length(p_base$m)#Temporary until removal as global parameter
    save(p_base,
         p_temporal,
         state_scenario_init,
         scenario_name,
         N.age.groups,
         file = save_path
    )
  })
  
}

