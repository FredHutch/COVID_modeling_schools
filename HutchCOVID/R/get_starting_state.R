#' Gets staring state
#'
#' @param parameters base model parameters
#' @param starting_conditions a list specifying total_prevalence, total_population, cumulative_incidence, and proportion_vaccinated
#' @param immune_frac a list specifying the proportions in each vaccine state by age group
#' @param strain_prevalence_frac a vertor for prevalence of each strain
#'
#' @return starting state vector
#' @export
get_starting_state = function(parameters,
                              starting_conditions,
                              immune_frac,
                              strain_prevalence_frac){
  
  #Vectorize parameters
  parameters = expand_vector_params(parameters)
  
  #Read off names
  vaccine.names = names(immune_frac)
  strain.names = names(strain_prevalence_frac)
  age.names = names(immune_frac[[1]])
  
  
  #Setup table of states
  susstate = expand_grid(state = get_state_order()[1], 
                         strain = "", 
                         vaccine = vaccine.names, 
                         age = age.names)
  
  otherstates = expand_grid(state = get_state_order()[-1], 
                            strain = strain.names, 
                            vaccine = vaccine.names, 
                            age = age.names)
  
  starting.state = rbind(susstate, otherstates)
  
  starting.state.init = starting.state
  #Append a column which contains the initial value
  starting.state$value = 0
  
  starting.state$basevalue = 0
  
  starting.state$infected = 0
  
  starting.state = assign_susceptible_population(starting.state,
                                                 immune_frac,
                                                 starting_conditions$total_population)
  
  #Calculate the distribution of values within each infected state
  infected.states = get_infected_classes(parameters)
  
  for(state in names(infected.states)){
    i = state_idx(state, parameters$n_age, parameters$n_strain, parameters$n_vaccine, parameters$n_recovered)
    starting.state$basevalue[i] = infected.states[[state]]
    starting.state$infected[i] = 1
  }
  
  merge(starting.state.init, adjust_strain_prevalence(starting.state,
                                                      parameters,
                                                      starting_conditions$total_population,
                                                      starting_conditions$total_prevalence,
                                                      strain_prevalence_frac), sort = F)
  
}