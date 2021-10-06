#' linear interpolation at time t between value1 at time1 and value2 at time2
#'
#' @param t time at which to interpolate
#' @param value1 value at time1
#' @param value2 value at time2
#' @param time1 time before t
#' @param time2 time after t
#'
#' @return interpolated value
interpolate_value = function(t, value1, value2, time1, time2)
{
  value = if (t < time1) { value1 }
  else if (t >= time1 & t < time2) {(as.numeric(time2 - t) * value1 + as.numeric(t - time1) * value2) / as.numeric(time2 - time1)}
  else if (t >= time2) { value2 }
  
  return(value)
}

#' Updates parameters on a certain date
#'
#' @param pars existing parameters
#' @param pars_to_update one item from pars_temporal which are the parameters to update
#' @param sd_update_metrics optionally the current metrics for determining sd dynamically
#'
#' @return updated parameters
update_temporal_params = function(pars, pars_to_update, sd_update_metrics = NULL)
{
  param_names = names(pars_to_update)
  for (i in 1:length(pars_to_update))
  {
    if (param_names[i] != "date")
    {
      if (param_names[i] == "adjust_sd")
      {
        dynamic_sd_func = match.fun(pars$dynamic_sd_func)
        sd_val = dynamic_sd_func(pars, sd_update_metrics)
        pars[["sd"]] = sd_val
        pars[["sd_i"]] = expand_vector(sd_val, pars$n_strain, pars$n_vaccine, pars$n_recovered)
      }
      else
      {
        pars[[param_names[i]]] = pars_to_update[[i]]
        
        if (is_simple_vectorized_param(param_names[i]))
        {
          pars[[paste0(param_names[i], "_i")]] = expand_vector(pars_to_update[[i]], pars$n_strain, pars$n_vaccine, pars$n_recovered)
        }
        if (is_veff_vectorized_param(param_names[i]))
        {
          stop("temporal parameters that depend on veff not supported")
        }
        if (param_names[i] == "strain_import")
        {
          pars[[paste0(param_names[i], "_i")]] = expand_strain_import(pars_to_update[[i]], pars$n_age, pars$n_vaccine + pars$n_recovered)
        }
        if (is_contact_param(param_names[i]))
        {
          pars$C = assemble_contact_matrix(pars)
          pars$C_ij = expand_contact_matrix(pars$C, pars$n_strain, pars$n_vaccine + pars$n_recovered)
        }
      }
    }
  }
  return(pars)
}

# this will sort the sublists of pars_temporal in ascending order and combine any lists with the same date into one list
# it is assumed that each sublist has a date element

#' Sorts temporal parameters
#'
#' Sorts the sublists of pars_temporal in ascending order 
#' and combine any lists with the same date into one list.
#' It is assumed that each sublist has a date element.
#' 
#' @param pars_temporal the parameters to sort
#'
#' @return sorted parameters
sort_temporal_params = function(pars_temporal)
{
  dates = sapply(pars_temporal, function(x) x$date)
  
  # check for duplicates and combine any lists
  if (anyDuplicated(dates))
  {
    dup_idxs = which(duplicated(dates))
    
    for (i in dup_idxs)
    {
      # find the first entry with that date and append list items there
      idx_to_add = which(dates == dates[i])[1]
      dup_list = pars_temporal[[i]]
      dup_list = dup_list[names(dup_list) != "date"]
      pars_temporal[[idx_to_add]] = append(pars_temporal[[idx_to_add]], dup_list)
    }
    # now remove the duplicated dates we already added elsewhere
    pars_temporal = pars_temporal[-dup_idxs]
  }
  
  # now order the list by date
  dates = sapply(pars_temporal, function(x) x$date)
  date_order = order(dates)
  pars_temporal = pars_temporal[date_order]
  
  return(pars_temporal)
}

#' Gets the days to update parameters in model time
#'
#' @param pars_base base parameters
#' @param pars_temporal temporal parameters
#' @param end_day end of simulation
#'
#' @return days to update the temporal parameters in model time
get_update_model_days = function(pars_base, pars_temporal, end_day)
{
  # assume each list has a date item
  # note lapply here or dates converted to integer
  dates = lapply(pars_temporal, function(x) x$date)
  model_days = sapply(dates, function(x) get_model_day_from_date(x, pars_base$model_day0_date))
  
  # can't start and end run on same day
  if (end_day %in% model_days) { warning(paste("End day of model run is also temporal parameter change which will not be applied on day", end_day)) }
  
  # get rid of any updates past model run time 
  # (need to keep all updates before start for indexing purposes and to ensure all params correctly applied)
  model_days = model_days[model_days < end_day]
  
  # check that times to update are ordered
  # if not, possible reasons would be not ordering the pars_temporal list or the sd ramping
  stopifnot(!is.unsorted(model_days))
  
  return(model_days)
}

#' Gets the days to update sd dynamically in model time
#'
#' @param pars_base base parameters
#' @param sd_dynamics temporal parameters for updating sd
#'
#' @return days to update dynamic sd in model time
get_update_sd_days = function(pars_base, sd_dynamics)
{
  # assume each list has a date item
  # note lapply here or dates converted to integer
  dates = lapply(sd_dynamics, function(x) x$date)
  sd_days = sapply(dates, function(x) get_model_day_from_date(x, pars_base$model_day0_date))
  
  return(sd_days)
}


#' Ramps linearly between two sd values. 
#' 
#' Generates the sd ramping that used to be in the model code for the initial transition 
#' to sd at the very start of the epidemic or for the transition between 2 sd values. 
#' For now this is hard-coded to stairstep up weekly.
#'
#' @param sd_initial starting sd
#' @param sd_final ending sd
#' @param start_ramp_date date of initial sd
#' @param end_ramp_date date of final sd
#'
#' @return temporal parameters to ramp sd
generate_sd_ramp = function(sd_initial, sd_final, start_ramp_date, end_ramp_date)
{
  # do it weekly (7 days) 
  ramp_period = 7
  # just the number of intermediate values, the start and end are already in the temporal parameters
  n_ramp_dates = floor(as.numeric(end_ramp_date - start_ramp_date) / ramp_period) - 1
  
  #  and set value on init date (generally sunday)
  params_ramp = list()
  
  for (i in 1:n_ramp_dates)
  {
    params_ramp = append(params_ramp, list(
      list( date = start_ramp_date + i * ramp_period, 
            sd = interpolate_value(start_ramp_date + i * ramp_period, sd_initial, sd_final, start_ramp_date, end_ramp_date))
      ))
  }
  return(params_ramp)
}

#' Ramp to initial sd value
#'
#' @param sd initial sd value
#' @param start_date model start date 
#' @param start_ramp_date when to start ramping sd
#' @param end_ramp_date when to attain initial sd value
#'
#' @return temporal parameters for sd ramp at epidemic start
generate_initial_ramp = function(sd, start_date, start_ramp_date, end_ramp_date)
{
  # assume we start from 0 sd
  sd_init = rep(0, length(sd))
  
  # add the inital and ending value
  # bfact_d doesn't change after initial lockdown, that is sd changes for other groups but stays the same for diagnosed
  params_ramp = list(
    list( date = start_date, 
          sd = sd_init,
          bfact_d = 1 - sd_init),
    list( date = end_ramp_date, 
          sd = sd,
          bfact_d = 1 - sd) 
  )
  
  # for initial period, need to ask for ramp starting 7 days before (rather than model start date) so first change will be on start_ramp_date
  params_ramp = append(params_ramp,
                       generate_sd_ramp(sd_init, sd, start_ramp_date - 7, end_ramp_date))
  
  # add bfact_d to ramp
  for (i in 1:length(params_ramp))
  {
    params_ramp[[i]]$bfact_d = 1 - params_ramp[[i]]$sd
  }
  
  return(params_ramp)
}

#' Generates ramps for subsequent sd parameters after initial lockdown, if any
#'
#' @param pars_temporal temporal parameters (with sd)
#' @param start_ramp_date date to start ramping
#' @param start_sd inital sd
#'
#' @return temporal parameters for sd ramps
generate_temporal_ramp = function(pars_temporal, start_ramp_date, start_sd)
{
  pars_temporal = sort_temporal_params(pars_temporal)
  sd_idxs = which(!sapply(lapply(pars_temporal, function(x) x$sd),is.null))
  params_ramp = list()
  
  if (length(sd_idxs) > 0) # might be no sd temporal params
  {
    # ramp from end of initial lockdown to first temporal sd
    params_ramp = append(params_ramp,
                         generate_sd_ramp(start_sd, pars_temporal[[sd_idxs[1]]]$sd, 
                                          start_ramp_date, pars_temporal[[sd_idxs[1]]]$date))
    # continue ramping if there are more sd values
    if (length(sd_idxs) > 1)
    {
      for (i in 1:(length(sd_idxs) - 1))
      {
        idx = sd_idxs[i]
        idx_next = sd_idxs[i + 1]
        params_ramp = append(params_ramp,
                             generate_sd_ramp(pars_temporal[[idx]]$sd, pars_temporal[[idx_next]]$sd, 
                                              pars_temporal[[idx]]$date, pars_temporal[[idx_next]]$date))
      }
    }
  }
  return(params_ramp)
}
