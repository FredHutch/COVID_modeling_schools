#' Verifies parameters are internally consistent in terms of strains and vaccines, etc.
#'
#' this function verifies that parameters are internally consistent
#' for now, this verifies the dimension of other parameters based on number of strains and vaccines
#' Note that this should take place *before* any parameters are vectorized

#' @param params Parameters to verify
#'
#' @return none
#' @export
verify_params = function(params)
{
  n_age = params$n_age
  n_strain = params$n_strain
  n_vaccine = params$n_vaccine
  n_vaccine_eff = n_vaccine + params$n_recovered 
  
  stopifnot(length(params$strain_infectivity) == n_strain)
  stopifnot(length(params$strain_severity) == n_strain)
  stopifnot(!is.null(dim(params$strain_import)))
  stopifnot(dim(params$strain_import) == c(n_strain, n_age))
  
  stopifnot(!is.null(dim(params$vep)))
  stopifnot(!is.null(dim(params$ves)))
  stopifnot(!is.null(dim(params$vei)))
  stopifnot(!is.null(dim(params$veh)))
  stopifnot(dim(params$vep) == c(n_strain, n_vaccine_eff)) # effectiveness of vaccines and natural infection
  stopifnot(dim(params$ves) == c(n_strain, n_vaccine_eff))
  stopifnot(dim(params$vei) == c(n_strain, n_vaccine_eff))
  stopifnot(dim(params$veh) == c(n_strain, n_vaccine_eff))
  stopifnot(length(params$vac_dist) == n_age)
  stopifnot(length(params$vac_priority) <= n_age)
  
  stopifnot(!is.null(dim(params$W)))
  stopifnot(!is.null(dim(params$Y)))
  stopifnot(dim(params$W) == c(n_age * (n_vaccine_eff), n_age * (n_vaccine_eff))) 
  stopifnot(dim(params$Y) == c(n_age * n_vaccine_eff, n_age * (n_vaccine_eff * n_strain)))
  
  
  for (p in get_simple_vectorized_params())
  {
    stopifnot(length(params[[p]]) == n_age)
  }
  
  if (params$dynamic_sd == TRUE)
  {
    if (params$dynamic_sd_func %in% c("calculate_dynamic_sd_cases", "calculate_dynamic_sd_hosp"))
    {
      stopifnot(length(params$dynamic_sd_lock_thresh) == 1)
      stopifnot(length(params$dynamic_sd_release_thresh) == 1)
    } else if (dynamic_sd_func == calculate_dynamic_sd_cases_and_hosp)
    {
      stopifnot(length(params$dynamic_sd_lock_thresh) == 2)
      stopifnot(length(params$dynamic_sd_release_thresh) == 2)
    } else
    {
      stop(paste("unknown dynamic_sd_func", dynamic_sd_func))
    }
  }
  
  print("Parameters good!")
}
