# functions used by model_runner.R for dynamic sd

#' calculates epidemic metrics used to calculate the dynamic sd
#'
#' @param model_out output metrix at timepoint to calculate metrics
#' @param parameters parameters used to run model
#'
#' @return metrics
calculate_sd_metrics = function(model_out, parameters)
{
  stopifnot(nrow(model_out) > parameters$dynamic_sd_period)
  
  diag_col_idxs = which(startsWith(colnames(model_out), "cum_diag"))
  cases_now = sum(model_out[nrow(model_out), diag_col_idxs])
  cases_start_period = sum(model_out[nrow(model_out) - parameters$dynamic_sd_period, diag_col_idxs])
  current_case_count = cases_now - cases_start_period
  
  hosp_col_idxs = which(startsWith(colnames(model_out), "cum_hosp"))
  hosp_now = sum(model_out[nrow(model_out), hosp_col_idxs])
  hosp_start_period = sum(model_out[nrow(model_out) - parameters$dynamic_sd_period, hosp_col_idxs])
  current_hosp_count = hosp_now - hosp_start_period
  
  return(list(current_case_count = current_case_count, current_hosp_count = current_hosp_count))
}

#' generate the set of dates we need to (potentially) adjust the dynamic sd and adds them to the temporal parameters
#'
#' @param pars_base base parameters
#' @param end_date end of model run
#'
#' @return list of dates to potentially adjust sd
generate_sd_dynamics = function(pars_base, end_date)
{
  if (end_date > pars_base$dynamic_sd_init_date)
  {
    adj_dates = seq(from = pars_base$dynamic_sd_init_date, to = end_date, by = pars_base$dynamic_sd_period)
    sd_dynamics = list()
    
    for (i in 1:length(adj_dates))
    {
      sd_dynamics = append(sd_dynamics, list( list(
        date = adj_dates[i],
        adjust_sd = TRUE)
      ) )
    }
  }
  return(sd_dynamics)
}

