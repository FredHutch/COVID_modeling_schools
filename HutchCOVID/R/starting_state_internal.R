####Calculate Starting State####

#Author: Mia Moore
#Date: 4/7/2021
#Purpose: Define a function to calculate the complete starting state

#Input:
# parameters
# prevalence
# cumulative incidence

#Output:
# Starting State


get_cum_strain_frac = function(variant.list){
  strain_cum_frac = sapply(variant.list, function(x)x$frac.cum.inc)
  
  use_values = sum(sapply(strain_cum_frac, is.null)) == 0
  
  if(use_values){
    return(strain_cum_frac)
  }
  nstrain = length(strain_cum_frac)
  .n = names(strain_cum_frac)
  strain_cum_frac = rep(1/nstrain, nstrain)
  strain_cum_frac
  names(strain_cum_frac) = .n
  strain_cum_frac
}


get_vaccine_frac = function(vaccines.list){
  immune_frac = sapply(vaccines.list, function(x)x$frac.doses)
  
  use_values = sum(sapply(immune_frac, is.null)) == 0
  
  if(use_values){
    return(immune_frac)
  }
  nvax = length(immune_frac)
  .n = names(immune_frac)
  immune_frac = rep(1/nvax, nvax)
  immune_frac
  names(immune_frac) = .n
  immune_frac
}

#' @importFrom plyr dlply
create_immune_distribution = function(immune.list, 
                                      strains.list, 
                                      vaccination.list, 
                                      cumulative_incidence, 
                                      coverage_current){
  
  fate.table = create_fate_table(immune.list, strains.list, vaccination.list)
  strain_cum_frac = get_cum_strain_frac(strains.list)
  immune_frac = get_vaccine_frac(vaccination.list)
  
  .out = plyr::dlply(fate.table, 
                     "state2", 
                     calculate_immune_state_prevalence_single_state, 
                     cumulative_incidence, 
                     coverage_current, 
                     strain_cum_frac, 
                     immune_frac)
  .out[names(immune.list)]
}

calculate_immune_state_prevalence_single_state = function(fate.table, cumulative_incidence, coverage_current, strain_cum_frac, immune_frac){
  mapply(calculate_immune_state_prevalence_single_age, cumulative_incidence, coverage_current, MoreArgs = list(strain_cum_frac, immune_frac, fate.table)) 
}

calculate_immune_state_prevalence_single_age = function(cumulative_incidence, coverage_current, strain_cum_frac, immune_frac, fate.table){
  
  strain_cum_frac = c(strain_cum_frac * cumulative_incidence, "no infection" = 1 - cumulative_incidence)
  immune_frac = c(immune_frac * coverage_current, "no vax" = 1 - coverage_current)
  
  proportion.combined = c(strain_cum_frac, immune_frac)
  proportion.combined[fate.table$step1] * proportion.combined[fate.table$step2]
  fate.table$mult1 = proportion.combined[fate.table$step1]
  fate.table$mult2 = proportion.combined[fate.table$step2]
  sum(fate.table$fate * proportion.combined[fate.table$step1] * proportion.combined[fate.table$step2])/2
}
#######################Fate Table###############################

#' @importFrom plyr adply
create_fate_table = function(immune.list, strain.list, vaccine.list){
  
  vaccines = c("no vax", names(vaccine.list))
  strains = c("no infection", names(strain.list))
  
  immune.state = names(immune.list)
  fate.table1 = expand_grid(step1 = vaccines, step2 = strains, state1 = immune.state, state2 = immune.state)
  fate.table2 = expand_grid(step1 = strains, step2 = vaccines, state1 = immune.state, state2 = immune.state)
  
  fate.table = rbind(fate.table1, fate.table2)
  
  plyr::adply(fate.table, 1, get.fate, immune.list)
  
}

get.fate = function(p, immune.list){
  with(p, {
    
    step1prob = 1
    
    if(step1 %in% c("no vax", "no infection"))
    {
      step1prob = ifelse(state1 == "None", 1, 0)
    }
    else
    {
      step1prob = immune.list$None$Fate[[step1]][state1]
    }
    
    
    step2prob = 1
    
    if(step2 %in% c("no vax", "no infection"))
    {
      step2prob = ifelse(state1 == state2, 1, 0)
    }
    else
    {
      step2prob = immune.list[[state1]]$Fate[[step2]][state2]
    }
    c(fate = as.numeric(step1prob * step2prob))
    
  })
}

get_infected_classes = function(parameters){
  proportion = Proportion.by.state(parameters)
  
  with(as.list(proportion),{
    list(
      E = rep(Prop_Ei, length(Prop_A1i)),
      A1 = Prop_A1i,
      A2 = Prop_A2i,
      P = Prop_Pi,
      IM = Prop_Mi,
      IS = Prop_Si,
      H = Prop_Hi,
      DA1 = Prop_DA1i,
      DA2 = Prop_DA2i,
      DP = Prop_DPi,
      DM = Prop_DMi,
      DS = Prop_DSi,
      DH = Prop_DHi
    )
    
  })
}

assign_susceptible_population = function(starting.state,
                                         immune_frac,
                                         total_population){
  
  n = nrow(starting.state)
  #A for loop over each state because I'm too tired to do this in a clever way
  for(i in seq(n)){
    
    
    age.i = starting.state$age[i]
    strain.i = starting.state$strain[i]
    vaccine.i = starting.state$vaccine[i]
    
    
    v_frac = immune_frac[[vaccine.i]][age.i]
    #rec_frac = cumulative_incidence[age.i]
    total_pop = total_population[age.i]
    
    # rec_strain = recovered_by_strain[strain.i]
    
    if(starting.state$state[i] == "S"){
      starting.state$value[i] = v_frac * total_pop #(1 - rec_frac) * total_pop
    }
    
    # if(starting.state$state[i] == "R"){
    #   starting.state$value[i] = v_frac * rec_frac * total_pop * rec_strain
    # }
    
    
  }
  starting.state
}


adjust_strain_prevalence = function(state_matrix,
                                    parameters,
                                    total_population,
                                    total_prevalence,
                                    strain_prevalence_frac){
  
  state_matrix_infected = subset(state_matrix, infected == 1)
  strain.names = unique(state_matrix_infected$strain)
  
  Trans.matrix = Get.Trans.matrix(parameters, state_matrix_to_vector(state_matrix))
  
  transmission.eigenvectors = Reduce("rbind", lapply(strain.names,
                                                     function(x){
                                                       .y = subset(state_matrix_infected, state == "E")
                                                       iv = which(.y$strain == x)
                                                       
                                                       .y2 = subset(.y, strain == x)
                                                       data.frame(age = .y2$age,
                                                                  vaccine = .y2$vaccine,
                                                                  strain = .y2$strain,
                                                                  multiplier = Re(eigen(Trans.matrix[iv,iv])$vectors[, 1])
                                                       )
                                                     }))
  
  state_matrix_infected = merge(state_matrix_infected, transmission.eigenvectors, all.x = T)
  
  state_matrix_infected$strain_total = strain_prevalence_frac[state_matrix_infected$strain]  *
    total_prevalence *
    sum(total_population)
  
  
  strain_multiplier = state_matrix_infected %>%
    group_by(strain) %>%
    group_modify( ~ data.frame(multiplier2 = mean(.x$strain_total)/sum(.x$multiplier * .x$basevalue)) )
  
  state_matrix_infected = merge(state_matrix_infected, strain_multiplier)
  
  state_matrix_infected$infected.value = state_matrix_infected$basevalue * 
    state_matrix_infected$multiplier * 
    state_matrix_infected$multiplier2
  
  
  
  state_matrix = merge(state_matrix, state_matrix_infected, all = T)
  
  state_matrix$value = ifelse(is.na(state_matrix$infected.value), 
                              state_matrix$value,
                              state_matrix$infected.value)
  
  # state_matrix = ddply(state_matrix, 
  #                           .variables = c("age", "strain", "vaccine"), 
  #                           .fun = function(x){
  #                             total.infected = sum(x$infected.value, na.rm = T)
  #                             
  #                             x$value = ifelse(x$state == "R", 
  #                                              x$value - total.infected,
  #                                              x$value)
  #                             x
  #                             }
  #  )
  
  state_matrix[, c("state", "strain", "vaccine", "age", "value")]
  
}