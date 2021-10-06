#' Model output in long format
#' 
#' Takes ode matrix output and turns in into a long data frame with columns state, strain, vac, and age.
#' note that strain can be empty in the S box and vac empty means not vaccinated
#'
#' @param ode_out matrix model output
#' @param model_day0_date calendar date for model day 0
#'
#' @return data in long format
#' @importFrom  dplyr mutate
#' @importFrom tidyr gather separate
#' @export
shape_data_long = function(ode_out, model_day0_date)
{
  # TODO: this function assumes 4 age classes, should rewrite to be more flexible, see recode below too
  
  # drop unused cum_vac columns (only first set used)
  cols_to_drop = colnames(ode_out)[startsWith(colnames(ode_out), "cum_vac")][-(1:4)]
  
  data_out = as.data.frame(ode_out) %>% 
    dplyr::select(-all_of(cols_to_drop)) %>% 
    gather(state, value, -time) %>% 
    separate(state, into = c("state", "strain", "vac", "age"), sep = "-") %>% 
    mutate(age = recode(age, 'a1' = 1, 'a2' = 2, 'a3' = 3, 'a4' = 4, .default = NA_real_)) %>%
    mutate(date = get_date_from_model_day(time, model_day0_date))
  return(data_out)
}
