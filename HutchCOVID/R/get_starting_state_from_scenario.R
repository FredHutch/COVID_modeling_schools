#' Gets starting state for scenario
#'
#' @param parameters base model parameters
#' @param starting_conditions a list specifying total_prevalence, total_population, cumulative_incidence, and proportion_vaccinated
#' @param scenario.specs a list which contains a strains.list which is a list of streans, each specifying
#' rel.transmissability. rel.severity, import.rate, frac.prevalence, and frac.cum.inc
#'
#' @return starting state vector
#' @export
get_starting_state_from_scenario = function(parameters, 
                                            starting_conditions,
                                            scenario.specs){
  
  with(as.list(c(starting_conditions, scenario.specs)),{
    
    strain_prevalence_frac = get_attribute_from_specs(strains.list, "frac.prevalence")
    
    immune_frac = create_immune_distribution(immune.list, 
                                             strains.list, 
                                             vaccination.list, 
                                             cumulative_incidence, 
                                             proportion_vaccinated)
    
    get_starting_state(parameters, 
                       starting_conditions,
                       immune_frac,
                       strain_prevalence_frac)
  })
}
