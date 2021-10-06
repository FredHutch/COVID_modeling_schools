#' Calculate error between model and data
#'
#' This functions runs the model with the specified parameters and compares the results to data. It is designed to 
#' be called from an optimizer, thus parameters are specified in several ways, see below.
#'
#' @param pars the vector of parameters from the optimization routine, and age-specific parameters should have _age
#' appended to the parameter name (e.g. sd_1)
#' @param pars_names the names of the parameters
#' @param pars_base the baseline set of parameters to which pars are applied
#' @param pars_temporal any parameters that need to be updated through time, specified as a list of lists, 
#' where each list has a date=<calendar date> and the parameter name-value pairs
#' @param state the model state at the start, either from a previous run or generated from initial_state()
#' @param data_actual the data to compare to 
#' @param start_from_first_inf (optional) if the model should start from the first infection 
#' rather than the start of the data
#' @param start_date (optional) the date to start if not the start of data or first infection
#' @param end_date (optional) the date to end if not the end of data
#' @name calc_sse_multi
#'
#' @return vector of errors for cases, deaths, hospitalizations, and tests
#' @importFrom tidyr drop_na
#' @export
calc_sse_multi = function(pars, pars_names, pars_base, pars_temporal, state, data_actual, 
                          start_from_first_inf = FALSE, start_date = NULL, end_date = NULL)
{
  # To calibrate temporal parameters: treat them as regualar parameters during the calibration interval, 
  # then iteratively add them to the temporal parameters as they're calibrated
  
  # get the parameters that are calibrated in the pars vector 
  parameters = get_params(pars, pars_names, pars_base)
  parameters_temporal = get_temporal_params(pars, pars_names, pars_temporal)
  #print(parameters)
  
  # model running, run_model takes care of temporal params
  # assume state is at start of data if not specified
  if (is.null(start_date))
  {
    start_date = if (start_from_first_inf) { get_date_from_model_day(parameters$first_inf_day, parameters$model_day0_date) } else { min(data_actual$date) }
  }
  if (is.null(end_date))
  {
    end_date = max(data_actual$date)
  }
  
  out = run_model_by_date(parameters, parameters_temporal, state, start_date, end_date)
  model_res = shape_data_wide(shape_data_long(out, parameters$model_day0_date))
  
  # subset necessary data columns, and use columns prefixed with "ma_" that are the 7-day moving average columns
  data_cases = dplyr::select(data_actual, date, starts_with("ma_pos")) 
  # TODO: right now combining hosp deaths and deaths, this will need to be updated depending on how deaths occur in model
  data_deaths = cbind(date = data_actual$date, 
                      data_actual[,paste0("ma_deaths", 1:parameters$n_age)] + data_actual[,paste0("ma_hosp_deaths", 1:parameters$n_age)])
  data_hosp = dplyr::select(data_actual, date, starts_with("ma_hosps")) 
  data_negtests = dplyr::select(data_actual, date, starts_with("ma_neg"))  

  
  ### cases (positive tests) ###
  out_cases = model_res %>% 
    dplyr::select(date, starts_with("diag")) %>%
    filter(date %in% data_cases$date) %>% drop_na() # get rid of rows past data and initial row of NA
  data_cases = data_cases %>% filter(date %in% out_cases$date) # make sure times are identical before substracting
  mean_cases = apply(dplyr::select(data_cases, -date), 2, mean)
  
  err_cases = apply((dplyr::select(data_cases,-date) - dplyr::select(out_cases, -date))^2, 2, sum)

  ### deaths ###
  out_deaths = model_res %>% 
    dplyr::select(date, starts_with("death")) %>%
    filter(date %in% data_deaths$date) %>% drop_na() # get rid of rows past data and initial row of NA
  data_deaths = data_deaths %>% filter(date %in% out_deaths$date) # make sure times are identical before substracting
  mean_deaths = apply(dplyr::select(data_deaths, -date), 2, mean)
  
  err_deaths = apply((dplyr::select(data_deaths,-date) - dplyr::select(out_deaths, -date))^2, 2, sum)

  ### hospitalizations ###
  out_hosp = model_res %>% 
    dplyr::select(date, starts_with("hosp")) %>%
    filter(date %in% data_hosp$date) %>% drop_na() # get rid of rows past data and initial row of NA
  data_hosp = data_hosp %>% filter(date %in% out_hosp$date) # make sure times are identical before substracting
  mean_hosps = apply(dplyr::select(data_hosp, -date), 2, mean)
  
  err_hosp = apply((dplyr::select(data_hosp,-date) - dplyr::select(out_hosp, -date))^2, 2, sum)

  # ### negative tests ###
  # out_tests = model_res %>% 
  #   dplyr::select(date, starts_with("testneg")) %>% 
  #   filter(date %in% data_negtests$date) %>% drop_na() # get rid of rows past data and initial row of NA
  # data_negtests = data_negtests %>% filter(date %in% out_tests$date) # make sure times are identical before substracting
  # mean_tests = apply(dplyr::select(data_negtests, -date), 2, mean)
  # 
  # err_tests = apply((dplyr::select(data_negtests,-date) - dplyr::select(out_tests, -date))^2, 2, sum)
    
  
  # avoid dividing by 0
  mean_cases = replace(mean_cases, which(mean_cases == 0), 1)
  mean_deaths = replace(mean_deaths, which(mean_deaths == 0), 1)
  mean_hosps = replace(mean_hosps, which(mean_hosps == 0), 1)
  # mean_tests = replace(mean_tests, which(mean_tests == 0), 1)
  
  # TODO this is sqrt in vaccine branch...
  score_cases = sqrt(err_cases ) / mean_cases
  score_deaths =  sqrt(err_deaths) /  mean_deaths
  score_hosp =  sqrt(err_hosp)  / mean_hosps
  # score_tests =  sqrt(err_tests)  / mean_tests
  
  # sum across ages
  scores = c(sum(score_cases), sum(score_deaths), sum(score_hosp)) #, sum(score_tests))
  names(scores) = c("daily cases", "daily deaths", "daily hosp") #, "daily tests")
  
  return(scores)
}
