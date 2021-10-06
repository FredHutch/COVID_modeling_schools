#' Gets the names of the vaccines
#'
#' Gets the names of the vaccines which are used in the long form output.
#' Note that recovered from natural infection are never included.
#' @param params parameters
#' @param inital_state inital state vector
#' @param include_recovered_vax whether to include those that are both vaccinated and recovered, otherwise just initally vaccinated
#'
#' @return names of vaccines
#' @importFrom tidyr separate
#' @export
get_vaccine_names = function(params, inital_state, include_recovered_vax = TRUE)
{
  vax_idx = which(apply(params$V, 1, function(x) any(x > 0)))
  recovered_idx = (params$n_age * params$n_vaccine + 1):(params$n_age * (params$n_vaccine + params$n_recovered))
  
  if (!include_recovered_vax)
  {
    vax_idx = setdiff(vax_idx, recovered_idx)
  }
  
  # now split names and look for unique
  s_states = inital_state[state_idx("S", params$n_age, params$n_strain, params$n_vaccine, params$n_recovered)]
  compartments = data.frame(name = names(s_states[vax_idx])) %>% separate(name, into = c("state", "strain", "vac", "age"), sep = "-")
  return(unique(compartments$vac))
}
