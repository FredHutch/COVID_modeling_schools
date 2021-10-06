#' Runs model and alculates metrics for calibration from the model output 
#'
#' @param pars vector of parameters (i.e. those being calibrated)
#' @param pars_names names of pars parameters
#' @param pars_base other fixed parameters
#' @param pars_temporal other temporal parameters
#' @param state initial state
#' @param start_from_first_inf whether to start from very beginning
#' @param start_date if start_from_first_inf==TRUE, start from first_inf_day, if NULL, use min(dates)
#' @param end_date if NULL, use max(dates)
#' @param dates which dates to include
#' @param rescale_factors i.e. the mean of each time series to normalize by
#' @param stats_to_include which of cases, deaths and hosp to include
#'
#' @return vector of statistics
#' @export
calc_model_stats = function(pars, pars_names, pars_base, pars_temporal, state, start_from_first_inf = FALSE, 
                            start_date = NULL, end_date = NULL, dates,
                            rescale_factors = list( cases = rep(1, 4), deaths = rep(1, 4), hosp = rep(1, 4), negtests = rep(1, 4) ), 
                            stats_to_include = c("cases", "deaths", "hosp"))
{
  parameters = get_params(pars, pars_names, pars_base)
  parameters_temporal = get_temporal_params(pars, pars_names, pars_temporal)

  if (is.null(start_date))
  {
    start_date = if (start_from_first_inf) { get_date_from_model_day(parameters$first_inf_day, parameters$model_day0_date) } else { min(dates) }
  }
  if (is.null(end_date))
  {
    end_date = max(dates)
  }
  
  out = run_model_by_date(parameters, parameters_temporal, state, start_date, end_date)
  model_res = shape_data_wide(shape_data_long(out, parameters$model_day0_date))
  model_res = model_res %>% filter(date %in% dates)
  
  out_cases = model_res %>% dplyr::select(starts_with("diag"))
  out_deaths = model_res %>% dplyr::select(starts_with("death"))
  out_hosp = model_res %>% dplyr::select(starts_with("hosp"))
  # out_negtests = model_res %>% dplyr::select(starts_with("testneg"))
  
  # normalize model output (transpose is because otherwise division is by cols)
  out_cases = t(t(out_cases) / rescale_factors$cases)
  out_deaths = t(t(out_deaths) / rescale_factors$deaths)
  out_hosp = t(t(out_hosp) / rescale_factors$hosp)
  # out_negtests = t(t(out_negtests) / rescale_factors$negtests)
  
  # important that these are in the same order as data!
  stats = NULL
  if ("cases" %in% stats_to_include) { stats = c(stats, out_cases)}
  if ("deaths" %in% stats_to_include) { stats = c(stats, out_deaths)}
  if ("hosp" %in% stats_to_include) { stats = c(stats, out_hosp)}
  # if ("negtests" %in% stats_to_include) { stats = c(stats, out_negtests)}
  
  return(stats) 
}
