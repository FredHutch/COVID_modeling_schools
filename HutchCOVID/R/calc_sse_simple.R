#' @rdname calc_sse_multi
#' @export
calc_sse_simple = function(pars, pars_names, pars_base, pars_temporal, state, data_actual,
                           start_from_first_inf = FALSE, start_date = NULL, end_date = NULL)
{
  scores = calc_sse_multi(pars, pars_names, pars_base, pars_temporal, state, data_actual,
                          start_from_first_inf, start_date, end_date)
  return(sum(scores) * 100)
}
