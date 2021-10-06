#' @title Functions used to determine dynamic sd activation or relaxation
#' @description Use one of the three following functions should be specified for the "dynamic_sd_func" parameter
#' \itemize{
#' \item calculate_dynamic_sd_cases on a date that we want to update the dynamic sd, calculate what it should be (it might not change)
#' \item calculate_dynamic_sd_hosp # uses hospitalizations to evaluate lock and release thresholds
#' \item calculate_dynamic_sd_cases_and_hosp uses cases OR hospitalizations to lock and cases AND hospitalizations to release. 
# in this case the lock and release thresholds need to be vectors specifying (cases, hosp) thresholds
#' }
#'
#' @param pars model parameters
#' @param sd_update_metrics metrics from model
#'
#' @return new sd value
#'
#' @name calculate_dynamic_sd
NULL

#' @rdname calculate_dynamic_sd
#' @export
calculate_dynamic_sd_cases = function(pars, sd_update_metrics)
{
  new_sd = pars$sd
  
  if(!is.null(sd_update_metrics))
  {
    if (sd_update_metrics$current_case_count > pars$dynamic_sd_lock_thresh)
    {
      new_sd = pars$dynamic_sd_max
    } else if (sd_update_metrics$current_case_count < pars$dynamic_sd_release_thresh)
    {
      new_sd = pmax(pars$dynamic_sd_min, pars$sd - pars$dynamic_sd_delta)
    }  
  }
  
  return(new_sd)
}

#' @rdname calculate_dynamic_sd
#' @export
calculate_dynamic_sd_hosp = function(pars, sd_update_metrics)
{
  new_sd = pars$sd
  
  if(!is.null(sd_update_metrics))
  {
    if (sd_update_metrics$current_hosp_count > pars$dynamic_sd_lock_thresh)
    {
      new_sd = pars$dynamic_sd_max
    } else if (sd_update_metrics$current_hosp_count < pars$dynamic_sd_release_thresh)
    {
      new_sd = pmax(pars$dynamic_sd_min, pars$sd - pars$dynamic_sd_delta)
    }  
  }
  
  return(new_sd)
}

#' @rdname calculate_dynamic_sd
#' @export
calculate_dynamic_sd_cases_and_hosp = function(pars, sd_update_metrics)
{
  new_sd = pars$sd
  
  if(!is.null(sd_update_metrics))
  {
    if (sd_update_metrics$current_case_count > pars$dynamic_sd_lock_thresh[1] |
        sd_update_metrics$current_hosp_count > pars$dynamic_sd_lock_thresh[2])
    {
      new_sd = pars$dynamic_sd_max
    } else if (sd_update_metrics$current_case_count < pars$dynamic_sd_release_thresh[1] &
               sd_update_metrics$current_hosp_count < pars$dynamic_sd_release_thresh[2])
    {
      new_sd = pmax(pars$dynamic_sd_min, pars$sd - pars$dynamic_sd_delta)
    }  
  }
  
  return(new_sd)
}
