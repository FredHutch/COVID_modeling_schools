#' auto-generate the initial state vector specifyin gonly S and E
#' 
#' auto-generate the initial state vector for just S and E with everything else 0
#' note E_init will be subtracted from S_init
#' note the default arguments are based on the population and age distribution of King County, WA
#' 
#' @param n_age number of age groups
#' @param n_strain number of strains
#' @param n_vaccine number of vaccines
#' @param n_recovered number of recovered
#' @param S_init starting susceptible population
#' @param E_init stating exposed population
#'
#' @return initial state vector
#' @importFrom tidyr expand_grid unite
#' @export
initial_state = function(n_age, n_strain, n_vaccine, n_recovered,
                         S_init = c(2190000 * c(0.2293,	0.4552,	0.2350,	0.0805), rep(0, n_age * (n_vaccine + n_recovered) - n_age)),
                         E_init = c(c(0, 2, 2, 0), rep(0, n_age * n_strain * (n_vaccine + n_recovered) - n_age)))
{
  # to allow recovered as a quasi vaccine
  n_vaccine_act = n_vaccine + n_recovered

  stopifnot(length(S_init) == n_age * n_vaccine_act)
  stopifnot(length(E_init) == n_age * n_strain * n_vaccine_act)
  
  # first vaccine state is no vaccine, then number from there
  vac = if (n_vaccine == 1) { "" } else { c("", paste0("v", 2:n_vaccine - 1)) }
  # now add pseudo vaccines of recovered from infection
  vac = c(vac, paste0("r", 1:n_recovered))
  
  # handle S states separately since no strain, assume it is first state!
  state_order = get_state_order()
  full_states = expand_grid(state = state_order[1], strain = "", vaccine = vac, age = paste0("a", 1:n_age))
  full_states = rbind( full_states, expand_grid(state = state_order[-1], strain = paste0("s", 1:n_strain), vaccine = vac, age = paste0("a", 1:n_age)) )
  
  # use hyphen to separate state/strain/vac/age, note _ already used in cum state names
  full_names = unlist(unite(full_states, name, state, strain, vaccine, age, sep = "-"))
  # assumption that E-init not already subtracted from S_init, do that now
  S_init = S_init - colSums(matrix(E_init, nrow = n_strain, byrow = TRUE)) # sum across strains
  states = c(S_init, E_init, rep(0, length(full_names) - length(S_init) - length(E_init)))
  names(states) = full_names
  return(states)
}
