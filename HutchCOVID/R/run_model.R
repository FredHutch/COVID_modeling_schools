#' Runs the model for a specified number of days
#'
#' This handles all the vectorization of parameters and setting the temporal parameters
#' 
#' @param pars_base base parameters
#' @param pars_temporal temporal parameters
#' @param daycount number of days to run
#' @param init_state initial state on start day
#' @param start_day (optional) to start on another model day than first_inf_day
#' @param calc_r_eff (optional) whether to calculate R effective
#'
#' @return matrix with daily values for all model compartments
#' @export
#' @importFrom deSolve ode
#' @importFrom utils tail
run_model = function(pars_base, pars_temporal, daycount, init_state, start_day = pars_base$first_inf_day, calc_r_eff = TRUE)
{
  # which days parameters need to be updated which will define the chunks
  # only get days that are between start and end day
  end_day = start_day + daycount - 1 # since start day is first day

  init_sd_start_day = get_model_day_from_date(pars_base$init_sd_start_date, pars_base$model_day0_date)
  init_sd_full_date = get_model_day_from_date(pars_base$init_sd_full_date, pars_base$model_day0_date)
  
  sd_ramp = list()
  if (start_day < init_sd_full_date & end_day > init_sd_start_day)
  {
    sd_ramp = generate_initial_ramp(pars_base$sd, 
                               get_date_from_model_day(start_day, pars_base$model_day0_date), 
                               pars_base$init_sd_start_date, 
                               pars_base$init_sd_full_date)
  }
  sd_tem_ramp = list()
  if (pars_base$ramp_sd == TRUE)
  {
    sd_tem_ramp = generate_temporal_ramp(pars_temporal, pars_base$init_sd_relax_date, pars_base$sd)
  }
  
  pars_temporal = append(pars_temporal, append(sd_ramp, sd_tem_ramp))

  sd_days_to_update = NULL
  if (pars_base$dynamic_sd)
  {
    sd_dynamics = generate_sd_dynamics(pars_base, get_date_from_model_day(end_day, pars_base$model_day0_date))
    sd_days_to_update = get_update_sd_days(pars_base, sd_dynamics)
    pars_temporal = append(pars_temporal, sd_dynamics)
  }
  
  # now make sure pars_temporal is ordered by date
  pars_temporal = sort_temporal_params(pars_temporal)

  # parameters for number of strains/vaccines
  pars = expand_vector_params(pars_base)
    
  # future temporal parameter changes, i.e. the chunks to run the model for
  model_days_to_update = get_update_model_days(pars_base, pars_temporal, end_day)
  n_periods = length(model_days_to_update) # how many periods there are to run
  update_idx = 1

  # update all parameter changes that happen before or on start day
  if (n_periods > 0)
  {
    while (start_day >= model_days_to_update[update_idx])
    {
      pars = update_temporal_params(pars, pars_temporal[[update_idx]])
      update_idx = update_idx + 1
      n_periods = n_periods - 1
      if (n_periods == 0) { break } # no more updates
    }
  }

  # start by running to first parameter change or end if no updates
  end_time = if (n_periods == 0)  { end_day } else { model_days_to_update[update_idx] }
  sd_col_names = paste0("sd---a", 1:length(pars$sd))
  r_eff_col_name = 'r_eff---'
  
  out = ode(y = init_state, times = seq(start_day, end_time, by = 1), func = model_eq, parms = pars, 
            method = "euler", hini = 0.1, maxsteps=10000)
  if (calc_r_eff)
  {
    r_eff = apply(out, 1, function(x) R_effective(pars, x[-1], TRUE, "both")) # -1 to remove time col
    out = cbind(out, 'r_eff---' = r_eff)
  }
  
  out = cbind(out, matrix(pars$sd, ncol = length(pars$sd), nrow = nrow(out), byrow = TRUE, 
                          dimnames = list(NULL, sd_col_names)))
  
  idx_drop_for_start = which(colnames(out) %in% c("time", sd_col_names, r_eff_col_name))

  # now run for each additional parameter update, if any
  sd_update_metrics = NULL
  if (n_periods != 0)
  {
    for (i in update_idx:length(model_days_to_update))
    {
      start_time = model_days_to_update[i]
      end_time = if (i == length(model_days_to_update)) { end_day } else { model_days_to_update[i + 1] }
      
      if (pars$dynamic_sd)
      {
        if (start_time %in% sd_days_to_update)
        {
          sd_update_metrics = calculate_sd_metrics(out, pars)
        }
      }
      
      pars = update_temporal_params(pars, pars_temporal[[i]], sd_update_metrics)
      increment_time = seq(from = start_time, to = end_time, by = 1)
      out_inc = ode(y = tail(out[,-idx_drop_for_start], 1), times = increment_time, func = model_eq, parms = pars, 
                    method = "euler", hini = 0.1, maxsteps=10000) # get last row for new initial conditions, but remove time column
      
      if (calc_r_eff)
      {
        # for some reason the column names (other than time) are missing for subsequent ode calls though they are retained the first time
        colnames(out_inc)[-1] = colnames(out)[-idx_drop_for_start]
        r_eff = apply(out_inc, 1, function(x) R_effective(pars, x[-1], TRUE, "both")) # -1 to remove time col
        out_inc = cbind(out_inc, 'r_eff---' = r_eff)
      }
      
      out_inc = cbind(out_inc, matrix(pars$sd, ncol = length(pars$sd), nrow = nrow(out_inc), byrow = TRUE, 
                                      dimnames = list(NULL, sd_col_names)))
      
      out = rbind(out, out_inc[-1,]) # don't include first row because it is the same as the last row of previous run
    }
  }
  
  return (out)
}
