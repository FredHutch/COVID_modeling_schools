#' Runs model for a matrix of parameterizations
#' 
#' get a list where each list item is a dataframe of model output
#' the option parallel is to run in parallel, this assumes a cluster has ben set up outside this function and foreach will use it
#' 
#' @param pars_matrix matrix with each row a parameterization
#' @param params_names names of columns of pars_matrix
#' @param params_base base parameters
#' @param params_temporal temporal parameters
#' @param init_state inital state
#' @param kc_data (optional) data to set end date from
#' @param start_date (optional) start date, or will use first_inf_day
#' @param end_date (optional) end date
#' @param calc_r_eff whetether to also calculate Reff
#' @param out_type wide is a wide data frame, long is a long data frame, and matrix is the ode matrix output
#' @param parallel wheterh to run in parallel
#'
#' @return list of model output for each parameterization
#' @import foreach
#' @export
get_model_data_param_sets = function(pars_matrix, params_names, params_base, params_temporal, 
                                     init_state, kc_data = NULL, start_date = NULL, end_date = NULL, calc_r_eff = TRUE,
                                     out_type = c("wide", "long", "matrix"), parallel = FALSE)
{
  requireNamespace("foreach")
  if (!parallel) { registerDoSEQ() } # prevent warning from %dopar% if not parallelized
  out_type = match.arg(out_type)
  pars_matrix = as.matrix(pars_matrix)
  results = foreach (i = 1:nrow(pars_matrix)) %dopar%
  {
    parameters = get_params(pars_matrix[i,], params_names, params_base)
    parameters_temporal = get_temporal_params(pars_matrix[i,, drop = TRUE], params_names, params_temporal)
    
    if (is.null(start_date)) { start = get_date_from_model_day(parameters$first_inf_day, parameters$model_day0_date) }
      else { start = start_date }
    if (is.null(end_date)) 
    {
      if (is.null(kc_data)) { end = start_date + 100 }
      else { end = max(kc_data$date) }
    }
    else { end = end_date }
    
    model_out = run_model_by_date(parameters, parameters_temporal, init_state, start, end, calc_r_eff)
    
    out_data = switch(out_type,
                      wide = shape_data_wide(shape_data_long(model_out, parameters$model_day0_date)),
                      long = shape_data_long(model_out, parameters$model_day0_date),
                      matrix = model_out)
  }
  
  return(results)
}
