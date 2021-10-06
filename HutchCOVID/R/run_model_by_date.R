#' Runs the model by specifying calendar dates
#'
#' @param pars_base base parameters
#' @param pars_temporal temporal parameters
#' @param init_state initial state
#' @param start_date start calendar date for simulation
#' @param end_date end calendar date for simulation
#' @param calc_r_eff (optional) whether to calculate R effective
#'
#' @return matrix with daily values for all model compartments
#' @export
run_model_by_date = function(pars_base, pars_temporal, init_state, start_date, end_date, calc_r_eff = TRUE)
{
  start_day = get_model_day_from_date(start_date, pars_base$model_day0_date)
  daycount = as.numeric(end_date - start_date + 1)
  return(run_model(pars_base, pars_temporal, daycount, init_state, start_day, calc_r_eff))
}
