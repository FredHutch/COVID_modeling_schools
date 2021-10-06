#' Filters out the  states that aren't actually compartments
#' 
#' i.e, sd, R-eff, the various cum_xxx tracking compartments
#'  useful for summing value across vaccination or strain
#'
#' @param data_long model output in long format
#'
#' @return model data with only true model compartments, not tracking compartments
#' @export
filter_non_compartments = function(data_long)
{
  return(filter(data_long, !startsWith(state, "cum") & state != "sd" & state != "r_eff"))
}
