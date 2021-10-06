#' Get parameters
#'
#' @param scenario.specs scenarios
#' @param params base parameters
#' @param params_temporal temporal parameters
#'
#' @return parameters
#' @export
get_parameters_from_specs = function(scenario.specs, params, params_temporal){
  
  with(scenario.specs, {
    params$n_strain = length(strains.list)
    params$n_vaccine = length(immune.list) - 1
    params$n_recovered = 1
    
    n_age = length(params$m)
    
    # define vaccine efficacy assumptions
    params$ves = get_attribute_from_specs(immune.list, "VEsusc")
    params_temporal = augment_temporal(params_temporal, 
                                       get_attribute_from_specs(immune.list, "VEsusc", temporal = T), 
                                       "ves")
    
    params$vep = get_attribute_from_specs(immune.list, "VEsymp")
    params_temporal = augment_temporal(params_temporal, 
                                       get_attribute_from_specs(immune.list, "VEsymp", temporal = T), 
                                       "vep")
    
    
    params$vei = get_attribute_from_specs(immune.list, "VEinf")
    params_temporal = augment_temporal(params_temporal, 
                                       get_attribute_from_specs(immune.list, "VEinf", temporal = T), 
                                       "vei")
    params$veh = get_attribute_from_specs(immune.list, "VEhosp")
    params_temporal = augment_temporal(params_temporal, 
                                       get_attribute_from_specs(immune.list, "VEhosp", temporal = T), 
                                       "veh")
    params$strain_infectivity = get_attribute_from_specs(strains.list, "rel.transmissability")
    params_temporal = augment_temporal(params_temporal, 
                                       get_attribute_from_specs(strains.list, "rel.transmissability", temporal = T), 
                                       "strain_infectivity")
    
    params$strain_severity = get_attribute_from_specs(strains.list, "rel.severity")
    params_temporal = augment_temporal(params_temporal, 
                                       get_attribute_from_specs(strains.list, "rel.severity", temporal = T), 
                                       "strain_severity")
    #params$strain_import = matrix(0, nrow = params$n_strain, ncol = n_age)
    params$strain_import = t(get_attribute_from_specs(strains.list, "import.rate"))
    params_temporal = augment_temporal(params_temporal, 
                                       lapply(get_attribute_from_specs(strains.list, "import.rate", temporal = T), t), 
                                       "strain_import")
    
    params$Q = create_Q(immune.list, n_age)
    params$W = create_W(immune.list, n_age)
    params$Y = create_Y(immune.list, strains.list, n_age)
    params$V = create_V(immune.list, vaccination.list, n_age)
    list(params = params, 
         params_temporal = params_temporal)
  })
  
}
